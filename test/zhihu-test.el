;;; zhihu-test.el --- Focused regression tests for zhihu.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(setq load-prefer-newer t)
(require 'zhihu)

(defvar secrets-enabled)

(defun zhihu-test--with-temp-file (suffix contents callback)
  "Create a temporary SUFFIX file with CONTENTS and call CALLBACK with it."
  (let ((file (make-temp-file "zhihu-test-" nil suffix)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert contents))
          (funcall callback file))
      (when-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (ignore-errors (delete-file file)))))

(defun zhihu-test--image-srcs (html)
  "Return all image src attributes from the HTML fragment HTML."
  (let* ((dom (zhihu--parse-html (concat "<html><body>" html "</body></html>")))
         (body (car (dom-by-tag dom 'body))))
    (mapcar (lambda (node) (dom-attr node 'src))
            (dom-by-tag body 'img))))

(defun zhihu-test--pandoc-reader-extension-p (reader extension)
  "Return non-nil when Pandoc READER knows EXTENSION."
  (and
   (executable-find "pandoc")
   (with-temp-buffer
     (and
      (eq
       (call-process
        "pandoc" nil t nil
        (format "--list-extensions=%s" reader))
       0)
      (progn
        (goto-char (point-min))
        (re-search-forward
         (format "^[+-]%s$" (regexp-quote extension))
         nil t))))))

(defun zhihu-test--only-anchor (html)
  "Return the only anchor in the HTML fragment HTML."
  (let* ((dom (zhihu--parse-html (concat "<html><body>" html "</body></html>")))
         (anchors (dom-by-tag dom 'a)))
    (should (= (length anchors) 1))
    (car anchors)))

(defun zhihu-test--link-card (html expected-url expected-title)
  "Return and validate the sole top-level link card in HTML."
  (let* ((dom (zhihu--parse-html (concat "<html><body>" html "</body></html>")))
         (body (car (dom-by-tag dom 'body)))
         (children (dom-children body))
         (elements (cl-remove-if-not #'consp children)))
    (dolist (child children)
      (when (stringp child)
        (should (string-empty-p (string-trim child)))))
    (should (= (length elements) 1))
    (let ((anchor (car elements)))
      (should (eq (dom-tag anchor) 'a))
      (should (equal (dom-attr anchor 'href) expected-url))
      (should (equal (dom-attr anchor 'data-draft-node) "block"))
      (should (equal (dom-attr anchor 'data-draft-type) "link-card"))
      (should (equal (dom-attr anchor 'data-draft-title) expected-title))
      (should (equal (dom-attr anchor 'data-draft-cover) ""))
      (should-not (dom-attr anchor 'data-zhihu-card))
      (should-not (dom-attr anchor 'title))
      anchor)))

(defun zhihu-test--references (html)
  "Return all native Zhihu reference nodes in HTML."
  (let ((dom
         (zhihu--parse-html
          (concat "<html><body>" html "</body></html>"))))
    (cl-remove-if-not
     (lambda (node)
       (and (eq (dom-tag node) 'sup)
            (equal (dom-attr node 'data-draft-node) "inline")
            (equal (dom-attr node 'data-draft-type) "reference")))
     (dom-by-tag dom 'sup))))

(defun zhihu-test--assert-reference (node numero text url)
  "Assert that NODE is native reference NUMERO with TEXT and URL."
  (should (equal (dom-attr node 'data-numero)
                 (number-to-string numero)))
  (should (equal (dom-attr node 'data-text) text))
  (should (equal (dom-attr node 'data-url) url))
  (should (equal (dom-children node) (list (format "[%d]" numero)))))

(defun zhihu-test--topic-record (id name &optional introduction)
  "Return a validated synthetic topic record for ID and NAME."
  (zhihu--article-topic-record
   `(:id ,id
	 :name ,name
	 :url ,(format "https://www.zhihu.com/topic/%s" id)
	 :introduction ,(or introduction ""))
   "test topic"))

(defun zhihu-test--commit-capf-candidate (capf query)
  "Complete QUERY through CAPF and commit its first candidate."
  (should capf)
  (let* ((start (nth 0 capf))
         (end (nth 1 capf))
         (table (nth 2 capf))
         (properties (nthcdr 3 capf))
         (candidate (car (all-completions query table))))
    (should candidate)
    (delete-region start end)
    (goto-char start)
    (insert candidate)
    (funcall (plist-get properties :exit-function)
             candidate 'finished)
    candidate))

(defun zhihu-test--publish-answer-body
    (answer-id question-id html &rest options)
  "Return the body `zhihu--publish-answer' passes to the request layer."
  (cl-letf (((symbol-function 'zhihu--publish-request)
             (lambda (body &rest request-options)
               (should-not request-options)
               body)))
    (apply #'zhihu--publish-answer
           answer-id question-id html options)))

(defun zhihu-test--publish-article-body
    (article-id is-published &rest options)
  "Return the body `zhihu--publish-article' passes to the request layer."
  (cl-letf (((symbol-function 'zhihu--publish-request)
             (lambda (body &rest request-options)
               (should (equal request-options '(:xsrf-state nil)))
               body)))
    (apply #'zhihu--publish-article
           article-id is-published options)))

(defun zhihu-test--publish-pin-body
    (thought-id title content topics &rest options)
  "Return the body `zhihu--publish-pin' passes to the request layer."
  (cl-letf (((symbol-function 'zhihu--publish-request)
             (lambda (body &rest request-options)
               (should-not request-options)
               body)))
    (apply #'zhihu--publish-pin
           thought-id title content topics options)))

(defun zhihu-test--hex (bytes)
  "Return a lowercase hexadecimal representation of BYTES."
  (mapconcat (lambda (byte) (format "%02x" byte)) bytes ""))

(defun zhihu-test--utf8-bytes (text)
  "Encode TEXT as an unibyte UTF-8 string."
  (encode-coding-string text 'utf-8 t))

(defconst zhihu-test--mermaid-png
  (concat
   (unibyte-string #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a)
   (zhihu-test--utf8-bytes "synthetic mermaid png"))
  "Synthetic PNG-signature bytes used by Mermaid renderer tests.")

(defun zhihu-test--with-temp-binary-file (suffix contents callback)
  "Create a binary SUFFIX file with CONTENTS and call CALLBACK with it."
  (let ((file (make-temp-file "zhihu-test-" nil suffix)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-file file
              (set-buffer-multibyte nil)
              (insert contents)))
          (funcall callback file))
      (when-let* ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (ignore-errors (delete-file file)))))

(defun zhihu-test--u32-be (value)
  "Encode VALUE as an unsigned big-endian 32-bit integer."
  (unibyte-string
   (logand (ash value -24) #xff)
   (logand (ash value -16) #xff)
   (logand (ash value -8) #xff)
   (logand value #xff)))

(defun zhihu-test--chromium-encrypt (prefix key plaintext)
  "Encrypt PLAINTEXT as a Chromium PREFIX AES-CBC value using KEY."
  (require 'gnutls)
  (let* ((padding (- 16 (% (length plaintext) 16)))
         (padded
          (concat (zhihu-test--utf8-bytes plaintext)
                  (make-string padding padding)))
         (result
          (gnutls-symmetric-encrypt
           'AES-128-CBC
           (copy-sequence key)
           zhihu--chromium-aes-cbc-iv
           padded)))
    (should (and (consp result) (stringp (car result))))
    (concat prefix (car result))))

(ert-deftest zhihu-zse-path-and-query-handles-empty-path-verbatim ()
  (should (equal (zhihu--zse-path-and-query "https://www.zhihu.com")
                 "/"))
  (should (equal (zhihu--zse-path-and-query
                  "https://www.zhihu.com?include=data%5B%2A%5D")
                 "/?include=data%5B%2A%5D"))
  (should (equal (zhihu--zse-path-and-query
                  "https://www.zhihu.com/api/v4/me?q=a%2Fb&raw=%E4%B8%AD")
                 "/api/v4/me?q=a%2Fb&raw=%E4%B8%AD")))

(ert-deftest zhihu-zse-v4-matches-known-vectors ()
  (should (equal (zhihu--zse-v4-encrypt "hello")
                 "+65P=3h=2RPlvZqa+mRtnl9K"))
  (should (equal (zhihu--zse-v4-encrypt "中文")
                 "+5Bjxr/yoPrZVlRrbplb0au=exkc4=pGs+cCd+tsMnfJ")))

(ert-deftest zhihu-zse96-header-matches-known-vector ()
  (should
   (equal
    (zhihu--zse96-header "https://www.zhihu.com/api/v4/me" "token")
    "2.0_RAVMvH/=d3Htxk4NWFJwIPhTTrWcgHZqO00rrMoSZ7gZI5w2Mg+vd5oKRwDRaz=D")))

(ert-deftest zhihu-http-json-signs-the-body-sent-on-the-wire ()
  (let ((cookie-reads 0)
        signed-body
        sent-method
        sent-url
        sent-body
        sent-headers)
    (cl-letf
        (((symbol-function 'zhihu--read-browser-cookies)
          (lambda (url)
            (should
             (equal
              url
              "https://www.zhihu.com/api/v4/test?q=%E4%B8%80"))
            (cl-incf cookie-reads)
            '(("d_c0" . "token")
              ("z_c0" . "session"))))
         ((symbol-function 'zhihu--zse96-header)
          (lambda (url dc0 &optional body)
            (should
             (equal url
                    "https://www.zhihu.com/api/v4/test?q=%E4%B8%80"))
            (should (equal dc0 "token"))
            (setq signed-body body)
            "2.0_test-signature"))
         ((symbol-function 'plz)
          (lambda (method url &rest args)
            (setq sent-method method
                  sent-url url
                  sent-body (plist-get args :body)
                  sent-headers (copy-tree (plist-get args :headers)))
            (should (equal (car plz-curl-default-args) "--disable"))
            (should (eq (plist-get args :body-type) 'binary))
            (should (eq (plist-get args :as) 'response))
            (should (eq (plist-get args :then) 'sync))
            (make-plz-response :status 200 :headers nil :body "{}"))))
      (zhihu--http-json
       "POST" "https://www.zhihu.com/api/v4/test?q=%E4%B8%80"
       :body '(:content "中文" :flag t)))
    (should (= cookie-reads 1))
    (should (eq sent-method 'post))
    (should
     (equal sent-url
            "https://www.zhihu.com/api/v4/test?q=%E4%B8%80"))
    (should
     (equal (encode-coding-string signed-body 'utf-8)
            sent-body))
    (should-not (multibyte-string-p sent-body))
    (should
     (equal (decode-coding-string sent-body 'utf-8)
            "{\"content\":\"中文\",\"flag\":true}"))
    (should
     (equal (cdr (assoc-string "x-zse-93" sent-headers t))
            "101_3_3.0"))
    (should
     (equal (cdr (assoc-string "x-zse-96" sent-headers t))
            "2.0_test-signature"))
    (should
     (equal (cdr (assoc-string "Cookie" sent-headers t))
            "d_c0=token; z_c0=session"))))

(ert-deftest zhihu-markdown-fenced-code-preserves-language ()
  (skip-unless (executable-find "pandoc"))
  (let ((html (zhihu--md->html
               "```python\nprint(\"hello\")\n```\n")))
    (should (string-match-p
             (regexp-quote "<pre lang=\"python\">")
             html))))

(ert-deftest zhihu-markdown-github-alerts-degrade-to-blockquotes ()
  (skip-unless (executable-find "pandoc"))
  (let* ((html
          (zhihu--md->html
           (mapconcat
            (lambda (type)
              (format "> [!%s]\n> %s body.\n" type type))
            '("NOTE" "TIP" "IMPORTANT" "WARNING" "CAUTION")
            "\n")))
         (dom
          (zhihu--parse-html
           (concat "<html><body>" html "</body></html>")))
         (quotes (dom-by-tag dom 'blockquote)))
    (should (= (length quotes) 5))
    (should
     (equal
      (mapcar
       (lambda (quote)
         (dom-inner-text (car (dom-by-tag quote 'strong))))
       quotes)
      '("ℹ Note" "💡 Tip" "❗ Important" "⚠ Warning" "⛔ Caution")))
    (should
     (equal
      (mapcar
       (lambda (quote)
         (dom-inner-text (car (last (dom-by-tag quote 'p)))))
       quotes)
      '("NOTE body." "TIP body." "IMPORTANT body."
        "WARNING body." "CAUTION body.")))))

(ert-deftest zhihu-markdown-github-alert-preserves-structured-body ()
  (skip-unless (executable-find "pandoc"))
  (let* ((html
          (zhihu--md->html
           (concat
            "> [!WARNING]\n"
            "> First **bold** paragraph.\n"
            ">\n"
            "> Second [linked](https://example.com) paragraph.\n"
            ">\n"
            "> - one\n"
            "> - two\n"
            ">\n"
            "> ```sh\n"
            "> echo hi\n"
            "> ```\n")))
         (dom
          (zhihu--parse-html
           (concat "<html><body>" html "</body></html>")))
         (quote (car (dom-by-tag dom 'blockquote)))
         (paragraphs (dom-by-tag quote 'p))
         (items (dom-by-tag quote 'li))
         (pre (car (dom-by-tag quote 'pre))))
    (should quote)
    (should (equal (dom-inner-text (car paragraphs)) "⚠ Warning"))
    (should
     (equal
      (zhihu--outer-html (nth 1 paragraphs))
      "<p>First <strong>bold</strong> paragraph.</p>"))
    (should (= (length (dom-by-tag (nth 1 paragraphs) 'strong)) 1))
    (should (equal (dom-attr (car (dom-by-tag quote 'a)) 'href)
                   "https://example.com"))
    (should (equal (mapcar #'dom-inner-text items) '("one" "two")))
    (should (equal (dom-attr pre 'lang) "sh"))
    (should (equal (dom-inner-text pre) "echo hi"))))

(ert-deftest zhihu-github-alert-conversion-is-markdown-ast-only ()
  (skip-unless (executable-find "pandoc"))
  (let* ((raw-html
          (zhihu--md->html
           (concat
            "<div class=\"warning\">"
            "<div class=\"title\"><p>Warning</p></div>"
            "<p>Raw HTML body.</p>"
            "</div>\n")))
         (raw-dom
          (zhihu--parse-html
           (concat "<html><body>" raw-html "</body></html>")))
         (org-html
          (zhihu--org->html
           "#+begin_quote\n[!WARNING]\nOrg body.\n#+end_quote\n"))
         (org-dom
          (zhihu--parse-html
           (concat "<html><body>" org-html "</body></html>")))
         (org-quote (car (dom-by-tag org-dom 'blockquote))))
    (should-not (dom-by-tag raw-dom 'blockquote))
    (should
     (cl-find-if
      (lambda (node) (zhihu--node-has-class-p node "warning"))
      (dom-by-tag raw-dom 'div)))
    (should org-quote)
    (should (string-match-p
             (regexp-quote "[!WARNING]")
             (dom-inner-text org-quote)))
    (should-not (dom-by-tag org-quote 'strong))))

(ert-deftest zhihu-markdown-mermaid-fences-render-png-images ()
  (skip-unless (executable-find "pandoc"))
  (let (sources)
    (cl-letf
        (((symbol-function 'zhihu--render-mermaid-png)
          (lambda (source)
            (push source sources)
            zhihu-test--mermaid-png)))
      (let* ((html
              (zhihu--md->html
               (concat
                "````mermaid\n"
                "flowchart LR\n"
                "  A --> B\n"
                "````\n\n"
                "~~~mermaid\n"
                "sequenceDiagram\n"
                "  A->>B: hello\n"
                "~~~\n")))
             (dom
              (zhihu--parse-html
               (concat "<html><body>" html "</body></html>")))
             (images (dom-by-tag dom 'img)))
        (should
         (equal
          (nreverse sources)
          '("flowchart LR\n  A --> B"
            "sequenceDiagram\n  A->>B: hello")))
        (should (= (length images) 2))
        (should-not (dom-by-tag dom 'pre))
        (dolist (image images)
          (should (equal (dom-attr image 'alt) "Mermaid diagram"))
          (let ((decoded
                 (zhihu--decode-data-url (dom-attr image 'src))))
            (should (equal (car decoded) "image/png"))
            (should (equal (cdr decoded)
                           zhihu-test--mermaid-png))))))))

(ert-deftest zhihu-org-mermaid-source-block-renders-png-image ()
  (skip-unless (executable-find "pandoc"))
  (let (source)
    (cl-letf
        (((symbol-function 'zhihu--render-mermaid-png)
          (lambda (input)
            (setq source input)
            zhihu-test--mermaid-png)))
      (let* ((html
              (zhihu--org->html
               (concat
                "#+begin_src mermaid\n"
                "flowchart LR\n"
                "  A --> B\n"
                "#+end_src\n")))
             (dom
              (zhihu--parse-html
               (concat "<html><body>" html "</body></html>")))
             (image (car (dom-by-tag dom 'img))))
        (should (equal source "flowchart LR\n  A --> B\n"))
        (should image)
        (should-not (dom-by-tag dom 'pre))
        (should (equal (dom-attr image 'alt) "Mermaid diagram"))
        (let ((decoded (zhihu--decode-data-url (dom-attr image 'src))))
          (should (equal (car decoded) "image/png"))
          (should (equal (cdr decoded) zhihu-test--mermaid-png)))))))

(ert-deftest zhihu-mermaid-rendering-is-language-specific ()
  (skip-unless (executable-find "pandoc"))
  (cl-letf
      (((symbol-function 'zhihu--render-mermaid-png)
        (lambda (&rest _args)
          (ert-fail "This code block must not be rendered as Mermaid"))))
    (should
     (string-match-p
      (regexp-quote "<pre lang=\"mermaid-js\">")
      (zhihu--md->html
       "```mermaid-js\nflowchart LR\n  A --> B\n```\n")))
    (should
     (string-match-p
      (regexp-quote "<pre lang=\"mermaid-js\">")
      (zhihu--org->html
       "#+begin_src mermaid-js\nflowchart LR\n  A --> B\n#+end_src\n")))))

(ert-deftest zhihu-markdown-and-org-task-lists-degrade-to-glyphs ()
  (skip-unless (executable-find "pandoc"))
  (dolist
      (html
       (list
        (zhihu--md->html "- [x] done\n- [ ] todo\n")
        (zhihu--org->html "- [X] done\n- [ ] todo\n")))
    (let* ((dom
            (zhihu--parse-html
             (concat "<html><body>" html "</body></html>")))
           (lists (dom-by-tag dom 'ul))
           (items (dom-by-tag dom 'li)))
      (should (= (length lists) 1))
      (should (= (length items) 2))
      (should-not (dom-by-tag dom 'input))
      (should-not (dom-by-tag dom 'label))
      (should-not (dom-attr (car lists) 'class))
      (should (equal (mapcar #'dom-inner-text items)
                     '("☑ done" "☐ todo"))))))

(ert-deftest zhihu-task-list-normalization-preserves-unrelated-classes ()
  (let* ((html
          (zhihu--zhihuify-html
           (concat
            "<ul class=\"other task-list\">"
            "<li class=\"task-list-item kept\">"
            "<label><input type=\"checkbox\" checked>done</label>"
            "</li></ul>")))
         (dom
          (zhihu--parse-html
           (concat "<html><body>" html "</body></html>")))
         (list-node (car (dom-by-tag dom 'ul)))
         (item (car (dom-by-tag dom 'li))))
    (should (equal (dom-attr list-node 'class) "other"))
    (should (equal (dom-attr item 'class) "kept"))
    (should-not (dom-by-tag dom 'input))
    (should-not (dom-by-tag dom 'label))
    (should (equal (dom-inner-text item) "☑ done"))))

(ert-deftest zhihu-source-figure-captions-use-native-image-annotations ()
  (skip-unless (executable-find "pandoc"))
  (let* ((markdown
          (zhihu--md->html
           (concat
            "![图 1：独占段落图片](block.svg)\n\n"
            "正文中的 ![替代文字](inline.svg) 图片。\n")))
         (org
          (zhihu--org->html
           "#+CAPTION: 图 2：Org 图片\n[[file:org.svg]]\n"))
         (typst-html
          (zhihu--normalize-html
           (concat
            "<figure data-size=\"small\">"
            "<img src=\"typst.svg\" alt=\"替代文字\">"
            "<figcaption>Figure&nbsp;1: <strong>Typst 图</strong></figcaption>"
            "</figure>"))))
    (dolist
        (case
         `((,markdown
            ("图 1：独占段落图片")
            ("block.svg"))
           (,org
            ("图 2：Org 图片")
            ("org.svg"))
           (,typst-html
            ("Figure 1: Typst 图")
            ("typst.svg"))))
      (pcase-let* ((`(,html ,captions ,sources) case)
                   (dom
                    (zhihu--parse-html
                     (concat "<html><body>" html "</body></html>")))
                   (images (dom-by-tag dom 'img)))
        (should-not (dom-by-tag dom 'figure))
        (should-not (dom-by-tag dom 'figcaption))
        (should (equal (mapcar (lambda (img) (dom-attr img 'src)) images)
                       sources))
        (should
         (equal
          (mapcar (lambda (img) (dom-attr img 'data-caption)) images)
          captions))))
    (let* ((dom
            (zhihu--parse-html
             (concat "<html><body>" typst-html "</body></html>")))
           (image (car (dom-by-tag dom 'img))))
      (should (equal (dom-attr image 'data-size) "small"))
      (should (equal (dom-attr image 'alt) "替代文字")))
    (let* ((dom
            (zhihu--parse-html
             (concat "<html><body>" markdown "</body></html>"))))
      (should
       (string-match-p
        (regexp-quote "正文中的 替代文字 图片。")
        (dom-inner-text (car (dom-by-tag dom 'body))))))))

(ert-deftest zhihu-native-caption-normalization-is-strict-and-plain-text ()
  (let* ((html
          (zhihu--zhihuify-html
           (concat
            "<figure data-size=\"normal\">"
            "<img src=\"diagram.svg\" "
            "data-caption=\"  A&nbsp;&nbsp;&amp; B  \">"
            "<figcaption> A <em>&amp;</em>\n B </figcaption>"
            "</figure>")))
         (dom
          (zhihu--parse-html
           (concat "<html><body>" html "</body></html>")))
         (image (car (dom-by-tag dom 'img))))
    (should (equal (dom-attr image 'data-caption) "A & B"))
    (should (equal (dom-attr image 'data-size) "normal"))
    (should-not (dom-by-tag dom 'em)))
  (should-error
   (zhihu--zhihuify-html
    (concat
     "<figure><img src=\"diagram.svg\" data-caption=\"one\">"
     "<figcaption>two</figcaption></figure>")))
  (let* ((html
          (zhihu--zhihuify-html
           (concat
            "<figure><img src=\"one.svg\"><img src=\"two.svg\">"
            "<figcaption>ambiguous</figcaption></figure>")))
         (dom
          (zhihu--parse-html
           (concat "<html><body>" html "</body></html>"))))
    (should (= (length (dom-by-tag dom 'figure)) 1))
    (dolist (image (dom-by-tag dom 'img))
      (should-not (dom-attr image 'data-caption)))))

(ert-deftest zhihu-typst-inline-svg-figure-enters-svg-upload-pipeline ()
  (skip-unless (executable-find "pandoc"))
  (let* ((html
          (zhihu--normalize-html
           (concat
            "<figure>"
            "<svg xmlns=\"http://www.w3.org/2000/svg\" "
            "width=\"20\" height=\"10\">"
            "<circle cx=\"5\" cy=\"5\" r=\"4\"></circle>"
            "</svg>"
            "<figcaption>CeTZ 图</figcaption>"
            "</figure>")))
         (dom
          (zhihu--parse-html
           (concat "<html><body>" html "</body></html>")))
         (image (car (dom-by-tag dom 'img)))
         (src (dom-attr image 'src))
         seen-svg)
    (should (equal (dom-attr image 'data-caption) "CeTZ 图"))
    (should (string-prefix-p "data:image/svg+xml;base64," src))
    (cl-letf (((symbol-function 'zhihu--render-svg-png)
               (lambda (bytes)
                 (setq seen-svg bytes)
                 zhihu-test--mermaid-png)))
      (should
       (equal
        (zhihu--img-bytes-and-mime src "/tmp/")
        (cons "image/png" zhihu-test--mermaid-png))))
    (should (string-match-p "<svg" seen-svg))
    (should (string-match-p "<circle" seen-svg))))

(ert-deftest zhihu-mermaid-renderer-uses-mmdc-and-cleans-temp-directory ()
  (let (directory)
    (cl-letf
        (((symbol-function 'executable-find)
          (lambda (program)
            (and (equal program "mmdc") "/test/bin/mmdc")))
         ((symbol-function 'zhihu--shell-convert)
          (lambda (program args input)
            (should (equal program "mmdc"))
            (should (equal input "flowchart LR\n  A --> B"))
            (let ((output (cadr (member "--output" args))))
              (setq directory (file-name-directory output))
              (should
               (equal args
                      (list "--input" "-" "--output" output)))
              (should (file-directory-p directory))
              (should-not (file-exists-p output))
              (let ((coding-system-for-write 'no-conversion))
                (with-temp-file output
                  (set-buffer-multibyte nil)
                  (insert zhihu-test--mermaid-png))))
            "")))
      (should
       (equal
        (zhihu--render-mermaid-png "flowchart LR\n  A --> B")
        zhihu-test--mermaid-png)))
    (should directory)
    (should-not (file-exists-p directory))))

(ert-deftest zhihu-mermaid-renderer-cleans-temp-directory-after-failure ()
  (let (directory)
    (cl-letf
        (((symbol-function 'executable-find)
          (lambda (_program) "/test/bin/mmdc"))
         ((symbol-function 'zhihu--shell-convert)
          (lambda (_program args _input)
            (setq directory
                  (file-name-directory
                   (cadr (member "--output" args))))
            (error "synthetic mmdc failure"))))
      (should-error
       (zhihu--render-mermaid-png "flowchart LR\n  A --> B")))
    (should directory)
    (should-not (file-exists-p directory))))

(ert-deftest zhihu-mermaid-renderer-validates-input-program-and-output ()
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_program) nil)))
    (should-error
     (zhihu--render-mermaid-png "flowchart LR\n  A --> B")
     :type 'user-error))
  (should-error (zhihu--render-mermaid-png " \n\t"))
  (let (directory)
    (cl-letf
        (((symbol-function 'executable-find)
          (lambda (_program) "/test/bin/mmdc"))
         ((symbol-function 'zhihu--shell-convert)
          (lambda (_program args _input)
            (let ((output (cadr (member "--output" args))))
              (setq directory (file-name-directory output))
              (with-temp-file output
                (insert "not a png")))
            "")))
      (should-error
       (zhihu--render-mermaid-png "flowchart LR\n  A --> B")))
    (should directory)
    (should-not (file-exists-p directory))))

(ert-deftest zhihu-svg-renderer-uses-typst-and-cleans-temp-directory ()
  (let ((svg-bytes
         (zhihu-test--utf8-bytes
          (concat
           "<svg xmlns=\"http://www.w3.org/2000/svg\" "
           "width=\"64\" height=\"32\"><text>图</text></svg>")))
        directory)
    (cl-letf
        (((symbol-function 'executable-find)
          (lambda (program)
            (and (equal program "typst") "/test/bin/typst")))
         ((symbol-function 'zhihu--shell-convert)
          (lambda (program args input)
            (should (equal program "typst"))
            (should (equal input ""))
            (setq directory (cadr (member "--root" args)))
            (let ((source (nth (- (length args) 2) args))
                  (output (car (last args))))
              (should
               (equal
                args
                (list
                 "compile"
                 "--root" directory
                 "--format" "png"
                 "--ppi" "144"
                 (expand-file-name "render.typ" directory)
                 (expand-file-name "output.png" directory))))
              (should
               (equal
                (zhihu--read-file-bytes
                 (expand-file-name "input.svg" directory))
                svg-bytes))
              (should
               (equal
                (with-temp-buffer
                  (insert-file-contents source)
                  (buffer-string))
                (concat
                 "#set page(width: auto, height: auto, "
                 "margin: 0pt, fill: none)\n"
                 "#image(\"input.svg\")\n")))
              (zhihu--write-file-bytes
               output zhihu-test--mermaid-png))
            "")))
      (should
       (equal
        (zhihu--render-svg-png svg-bytes)
        zhihu-test--mermaid-png)))
    (should directory)
    (should-not (file-exists-p directory))))

(ert-deftest zhihu-svg-renderer-cleans-up-and-validates-all-failures ()
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_program) nil)))
    (should-error
     (zhihu--render-svg-png "<svg/>")
     :type 'user-error))
  (should-error (zhihu--render-svg-png ""))
  (let (failed-directory)
    (cl-letf
        (((symbol-function 'executable-find)
          (lambda (_program) "/test/bin/typst"))
         ((symbol-function 'zhihu--shell-convert)
          (lambda (_program args _input)
            (setq failed-directory (cadr (member "--root" args)))
            (error "synthetic typst failure"))))
      (should-error (zhihu--render-svg-png "<svg/>")))
    (should failed-directory)
    (should-not (file-exists-p failed-directory)))
  (let (invalid-directory)
    (cl-letf
        (((symbol-function 'executable-find)
          (lambda (_program) "/test/bin/typst"))
         ((symbol-function 'zhihu--shell-convert)
          (lambda (_program args _input)
            (setq invalid-directory (cadr (member "--root" args)))
            (zhihu--write-file-bytes
             (car (last args))
             (zhihu-test--utf8-bytes "not a png"))
            "")))
      (should-error (zhihu--render-svg-png "<svg/>")))
    (should invalid-directory)
    (should-not (file-exists-p invalid-directory))))

(ert-deftest zhihu-markdown-pandoc-consumes-yaml-frontmatter ()
  (skip-unless (executable-find "pandoc"))
  (let ((html
         (zhihu--md->html
          (concat
           "---\n"
           "title: \"Metadata title\"\n"
           "zhihu:\n"
           "  question-id: \"123\"\n"
           "---\n"
           "# Visible heading\n"
           "## Nested heading\n"))))
    (should (string-match-p
             "<h2\\(?: [^>]*\\)?>Visible heading</h2>"
             html))
    (should (string-match-p
             "<h3\\(?: [^>]*\\)?>Nested heading</h3>"
             html))
    (should-not (string-match-p "Metadata title\\|question-id\\|zhihu:" html))))

(ert-deftest zhihu-org-pandoc-shifts-the-whole-heading-hierarchy ()
  (skip-unless (executable-find "pandoc"))
  ;; Markdown 的开关不得改变 Org 的通常导出层级。
  (let ((zhihu-enable-markdown-heading-level-shift nil))
    (let ((html
           (zhihu--org->html
            "#+TITLE: Metadata title\n* Top level\n** Nested level\n")))
      (should (string-match-p
               "<h2\\(?: [^>]*\\)?>Top level</h2>"
               html))
      (should (string-match-p
               "<h3\\(?: [^>]*\\)?>Nested level</h3>"
               html))
      (should-not (string-match-p "Metadata title" html)))))

(ert-deftest zhihu-markdown-heading-shift-can-be-disabled ()
  (skip-unless (executable-find "pandoc"))
  (let* ((zhihu-enable-markdown-heading-level-shift nil)
         (html
          (zhihu--md->html
           "---\ntitle: Metadata title\n---\n# Top level\n## Nested level\n")))
    (should (string-match-p
             "<h1\\(?: [^>]*\\)?>Top level</h1>"
             html))
    (should (string-match-p
             "<h2\\(?: [^>]*\\)?>Nested level</h2>"
             html))
    (should-not (string-match-p "Metadata title" html))))

(ert-deftest zhihu-html-normalization-preserves-heading-levels ()
  (skip-unless (executable-find "pandoc"))
  (let ((html
         (zhihu--normalize-html
          "<h2>Top level</h2><h3>Nested level</h3>")))
    (should (string-match-p
             "<h2\\(?: [^>]*\\)?>Top level</h2>"
             html))
    (should (string-match-p
             "<h3\\(?: [^>]*\\)?>Nested level</h3>"
             html))))

(ert-deftest zhihu-markdown-and-org-preserve-stable-section-links ()
  (skip-unless
   (and
    (executable-find "pandoc")
    (zhihu-test--pandoc-reader-extension-p "gfm" "attributes")))
  (dolist
      (html
       (list
        (zhihu--md->html
         (concat
          "# 开头 {#stable-start}\n\n"
          "[转到结论](#stable-conclusion)\n\n"
          "## 结论 {#stable-conclusion}\n"))
        (zhihu--org->html
         (concat
          "* 开头\n"
          ":PROPERTIES:\n"
          ":CUSTOM_ID: stable-start\n"
          ":END:\n\n"
          "[[#stable-conclusion][转到结论]]\n\n"
          "** 结论\n"
          ":PROPERTIES:\n"
          ":CUSTOM_ID: stable-conclusion\n"
          ":END:\n"))))
    (let* ((dom
            (zhihu--parse-html
             (concat "<html><body>" html "</body></html>")))
           (h2 (car (dom-by-tag dom 'h2)))
           (h3 (car (dom-by-tag dom 'h3)))
           (anchor (zhihu-test--only-anchor html)))
      (should (equal (dom-attr h2 'id) "stable-start"))
      (should (equal (dom-attr h3 'id) "stable-conclusion"))
      (should (equal (dom-attr anchor 'href) "#stable-conclusion")))))

(ert-deftest zhihu-typst-preserves-stable-section-links ()
  (skip-unless
   (and (executable-find "typst")
        (executable-find "pandoc")))
  (zhihu-test--with-temp-file
   ".typ"
   (concat
    "#set document(title: \"Section link test\")\n"
    "#set heading(numbering: \"1.\")\n"
    "= 开头 <stable-start>\n\n"
    "#link(<stable-conclusion>)[转到结论]\n\n"
    "参见 @stable-conclusion。\n\n"
    "== 结论 <stable-conclusion>\n\n"
    "#link(<stable-start>)[返回开头]\n")
   (lambda (file)
     (let* ((html (zhihu--source-to-html file))
            (dom
             (zhihu--parse-html
              (concat "<html><body>" html "</body></html>")))
            (h2 (car (dom-by-tag dom 'h2)))
            (h3 (car (dom-by-tag dom 'h3)))
            (anchors (dom-by-tag dom 'a)))
       (should (equal (dom-attr h2 'id) "stable-start"))
       (should (equal (dom-attr h3 'id) "stable-conclusion"))
       (should
        (equal
         (mapcar (lambda (anchor) (dom-attr anchor 'href)) anchors)
         '("#stable-conclusion"
           "#stable-conclusion"
           "#stable-start")))))))

(ert-deftest zhihu-article-section-links-use-h2-h3-document-order ()
  (let* ((html
          (concat
           "<h2 id=\"first\">First</h2>"
           "<h4 id=\"ignored\">Ignored</h4>"
           "<h3>Anonymous but counted</h3>"
           "<p>"
           "<a href=\"#target\">local</a>"
           "<a href=\"https://example.test/page#target\">external</a>"
           "<a role=\"doc-biblioref\" href=\"#missing-citation\">citation</a>"
           "<sup role=\"doc-noteref\">"
           "<a href=\"#missing-note\">note</a>"
           "</sup>"
           "<a role=\"doc-backlink\" href=\"#missing-backlink\">back</a>"
           "<section role=\"doc-endnotes fallback\">"
           "<a href=\"#missing-endnote\">endnote</a>"
           "</section>"
           "</p>"
           "<h2 id=\"target\">Target</h2>"))
         (targets
          (zhihu--article-section-link-targets html t))
         (rewritten
          (zhihu--rewrite-article-section-links
           html "456" targets))
         (dom
          (zhihu--parse-html
           (concat "<html><body>" rewritten "</body></html>")))
         (hrefs
          (mapcar
           (lambda (anchor) (dom-attr anchor 'href))
           (dom-by-tag dom 'a))))
    (should (equal targets '(("#target" . 2))))
    (should
     (equal
      hrefs
      '("#h_456_2"
        "https://example.test/page#target"
        "#missing-citation"
        "#missing-note"
        "#missing-backlink"
        "#missing-endnote")))))

(ert-deftest zhihu-article-section-links-decode-percent-fragments ()
  (let* ((html
          (concat
           "<h2 id=\"%E7%BB%93%E8%AE%BA\">Literal percent id</h2>"
           "<a href=\"#%E7%BB%93%E8%AE%BA\">Conclusion</a>"
           "<h3 id=\"结论\">结论</h3>"))
         (targets (zhihu--article-section-link-targets html t))
         (rewritten
          (zhihu--rewrite-article-section-links html "456" targets)))
    ;; 浏览器按 URL 语义解码 fragment；同名的原始百分号 ID 不应抢先。
    (should (equal targets '(("#%E7%BB%93%E8%AE%BA" . 1))))
    (should
     (equal
      (dom-attr (zhihu-test--only-anchor rewritten) 'href)
      "#h_456_1"))))

(ert-deftest zhihu-article-section-links-reject-invalid-targets ()
  (dolist
      (case
       '(("<h2 id=\"known\">Known</h2><a href=\"#missing\">missing</a>"
          t
          "找不到目标")
         ("<h2>Heading</h2><div id=\"not-heading\">Target</div>\
<a href=\"#not-heading\">not heading</a>"
          t
          "目标不是 h2/h3")
         ("<h2 id=\"duplicate\">One</h2><h3 id=\"duplicate\">Two</h3>\
<a href=\"#duplicate\">duplicate</a>"
          t
          "标题 id 重复")
         ("<h2 id=\"duplicate-node\">One</h2>\
<span id=\"duplicate-node\">Two</span>\
<a href=\"#duplicate-node\">duplicate node</a>"
          t
          "目标 id 重复")
         ("<h2 id=\"known\">Known</h2><a href=\"#known\">known</a>"
          nil
          "目录")))
    (let ((err
           (should-error
            (zhihu--article-section-link-targets
             (nth 0 case) (nth 1 case))
            :type 'user-error)))
      (should
       (string-match-p
        (regexp-quote (nth 2 case))
        (error-message-string err))))))

(ert-deftest zhihu-compile-source-document-reuses-one-typst-compilation ()
  (let ((compile-count 0)
        (full-html
         "<html><head><title>Typst title</title></head><body>Body</body></html>"))
    (cl-letf
        (((symbol-function 'zhihu--typst-compile-html)
          (lambda (file)
            (should (equal file "/tmp/post.typ"))
            (cl-incf compile-count)
            full-html))
         ((symbol-function 'zhihu--html-document-title)
          (lambda (html)
            (should (equal html full-html))
            "Typst title"))
         ((symbol-function 'zhihu--normalize-html)
          (lambda (html)
            (should (equal html full-html))
            "<p>Body</p>"))
         ((symbol-function 'zhihu--source-to-html)
          (lambda (&rest _args)
            (ert-fail "Typst document must not be compiled a second time"))))
      (should
       (equal
        (zhihu--compile-source-document
         "/tmp/post.typ" "Ignored metadata title")
        '(:format typst :title "Typst title" :html "<p>Body</p>"))))
    (should (= compile-count 1))))

(ert-deftest zhihu-compile-source-document-keeps-parsed-markdown-title ()
  (cl-letf (((symbol-function 'zhihu--source-to-html)
             (lambda (file)
               (should (equal file "/tmp/post.md"))
               "<p>Body</p>")))
    (should
     (equal
      (zhihu--compile-source-document "/tmp/post.md" "Metadata title")
      '(:format markdown :title "Metadata title" :html "<p>Body</p>")))))

(ert-deftest zhihu-compile-source-document-validates-title-before-body ()
  (dolist (file '("/tmp/post.md" "/tmp/post.typ"))
    (let ((compiled 0)
          (converted 0)
          (messages 0))
      (cl-letf
          (((symbol-function 'zhihu--typst-compile-html)
            (lambda (_file)
              (cl-incf compiled)
              "<html><head></head><body>Body</body></html>"))
           ((symbol-function 'zhihu--html-document-title)
            (lambda (_html) nil))
           ((symbol-function 'zhihu--normalize-html)
            (lambda (_html)
              (cl-incf converted)
              "<p>Body</p>"))
           ((symbol-function 'zhihu--source-to-html)
            (lambda (_file)
              (cl-incf converted)
              "<p>Body</p>"))
           ((symbol-function 'message)
            (lambda (&rest _args)
              (cl-incf messages))))
        (should-error
         (zhihu--compile-source-document
          file nil
          (lambda (_title)
            (user-error "Invalid title")))
         :type 'user-error))
      (should (= compiled (if (string-suffix-p ".typ" file) 1 0)))
      (should (= messages (if (string-suffix-p ".typ" file) 1 0)))
      (should (zerop converted)))))

(ert-deftest zhihu-markdown-and-org-thematic-breaks-use-hr ()
  (skip-unless (executable-find "pandoc"))
  (dolist
      (html
       (list
        (zhihu--md->html "Before\n\n---\n\nAfter\n")
        (zhihu--org->html "Before\n\n-----\n\nAfter\n")))
    (let* ((dom
            (zhihu--parse-html
             (concat "<html><body>" html "</body></html>")))
           (body (car (dom-by-tag dom 'body)))
           (elements (cl-remove-if-not #'consp (dom-children body))))
      (should (equal (mapcar #'dom-tag elements) '(p hr p)))
      (should (equal (dom-inner-text (nth 0 elements)) "Before"))
      (should (equal (dom-inner-text (nth 2 elements)) "After")))))

(ert-deftest zhihu-typst-divider-produces-hr ()
  (skip-unless (and (executable-find "typst")
                    (executable-find "pandoc")))
  (zhihu-test--with-temp-file
   ".typ"
   (concat
    "#set document(title: \"Thematic break test\")\n"
    "Before\n\n"
    "#divider()\n\n"
    "After\n")
   (lambda (file)
     (let* ((html (zhihu--source-to-html file))
            (dom
             (zhihu--parse-html
              (concat "<html><body>" html "</body></html>")))
            (body (car (dom-by-tag dom 'body)))
            (elements (cl-remove-if-not #'consp (dom-children body))))
       (should (equal (mapcar #'dom-tag elements) '(p hr p)))
       (should (equal (dom-inner-text (nth 0 elements)) "Before"))
       (should (equal (dom-inner-text (nth 2 elements)) "After"))))))

(ert-deftest zhihu-markdown-and-org-card-links-use-the-same-html ()
  (skip-unless (executable-find "pandoc"))
  (dolist
      (html
       (list
        (zhihu--md->html
         "[Git *Hub*](https://example.com/a?x=1&y=2 \"card\")\n")
        (zhihu--org->html
         (concat
          "#+ATTR_ZHIHU: :type link-card\n"
          "[[https://example.com/a?x=1&y=2][Git /Hub/]]\n"))))
    (let ((anchor
           (zhihu-test--link-card
            html "https://example.com/a?x=1&y=2" "Git Hub")))
      (should
       (equal (string-join (split-string (dom-inner-text anchor) nil t) " ")
              "Git Hub"))
      (should (car (dom-by-tag anchor 'em))))))

(ert-deftest zhihu-typst-card-link-function-produces-link-card ()
  (skip-unless (and (executable-find "typst")
                    (executable-find "pandoc")))
  (zhihu-test--with-temp-file
   ".typ"
   (concat
    "#set document(title: \"Card test\")\n"
    "#let card-link(url, body) = context {\n"
    "  if target() == \"html\" {\n"
    "    html.elem(\"a\", attrs: (\n"
    "      href: url,\n"
    "      \"data-zhihu-card\": \"\",\n"
    "    ), body)\n"
    "  } else {\n"
    "    link(url, body)\n"
    "  }\n"
    "}\n\n"
    "#card-link(\"https://example.com/a?x=1&y=2\")[Git #emph[Hub]]\n")
   (lambda (file)
     (let* ((html (zhihu--source-to-html file))
            (anchor
             (zhihu-test--link-card
              html "https://example.com/a?x=1&y=2" "Git Hub")))
       (should
        (equal (string-join (split-string (dom-inner-text anchor) nil t) " ")
               "Git Hub"))
       (should (car (dom-by-tag anchor 'em)))))))

(ert-deftest zhihu-markdown-and-org-footnotes-use-native-references ()
  (skip-unless (executable-find "pandoc"))
  (dolist
      (html
       (list
        (zhihu--md->html
         (concat
          "首次[^paper]，再次[^paper]，另见[^plain]。\n\n"
          "[^paper]: 参见 [论文标题](https://example.com/paper?a=1&b=2)。\n\n"
          "[^plain]: 纯文字说明。\n"))
        (zhihu--org->html
         (concat
          "首次[fn:paper]，再次[fn:paper]，另见[fn:plain]。\n\n"
          "[fn:paper] 参见 [[https://example.com/paper?a=1&b=2]"
          "[论文标题]]。\n"
          "[fn:plain] 纯文字说明。\n"))))
    (let ((references (zhihu-test--references html)))
      (should (= (length references) 3))
      (zhihu-test--assert-reference
       (nth 0 references) 1 "参见 论文标题。" "https://example.com/paper?a=1&b=2")
      (zhihu-test--assert-reference
       (nth 1 references) 1 "参见 论文标题。" "https://example.com/paper?a=1&b=2")
      (zhihu-test--assert-reference
       (nth 2 references) 2 "纯文字说明。" "")
      (should-not (string-match-p "doc-endnotes\\|footnote-back\\|data-zhihu-reference"
                                  html)))))

(ert-deftest zhihu-org-inline-footnote-uses-native-reference ()
  (skip-unless (executable-find "pandoc"))
  (let* ((html (zhihu--org->html "正文[fn::纯文字说明]。\n"))
         (reference (car (zhihu-test--references html))))
    (zhihu-test--assert-reference reference 1 "纯文字说明" "")))

(ert-deftest zhihu-typst-native-footnotes-use-native-references ()
  (skip-unless (and (executable-find "typst")
                    (executable-find "pandoc")))
  (zhihu-test--with-temp-file
   ".typ"
   (concat
    "#set document(title: \"Reference test\")\n"
    "首次#footnote[参见 "
    "#link(\"https://example.com/paper?a=1&b=2\")[论文标题]。] <paper>\n"
    "再次 @paper。另见#footnote[纯文字说明。]\n")
   (lambda (file)
     (let* ((html (zhihu--source-to-html file))
            (references (zhihu-test--references html)))
       (should (= (length references) 3))
       (zhihu-test--assert-reference
        (nth 0 references) 1 "参见 论文标题。" "https://example.com/paper?a=1&b=2")
       (zhihu-test--assert-reference
        (nth 1 references) 1 "参见 论文标题。" "https://example.com/paper?a=1&b=2")
       (zhihu-test--assert-reference
        (nth 2 references) 2 "纯文字说明。" "")
       (should-not
        (string-match-p "doc-endnotes\\|data-zhihu-reference" html))))))

(ert-deftest zhihu-typst-footnotes-do-not-consume-bibliography-nodes ()
  (skip-unless (and (executable-find "typst")
                    (executable-find "pandoc")))
  (zhihu-test--with-temp-file
   ".bib"
   "@article{doe, author={Doe, Jane}, title={Example Work}, year={2020}}\n"
   (lambda (bib)
     (zhihu-test--with-temp-file
      ".typ"
      (format
       (concat
        "#set document(title: \"References\")\n"
        "正文#footnote[脚注说明。]，引用 @doe。\n"
        "#bibliography(%S)\n")
       (file-name-nondirectory bib))
      (lambda (file)
        (let* ((html (zhihu--source-to-html file))
               (dom
                (zhihu--parse-html
                 (concat "<html><body>" html "</body></html>")))
               (reference (car (zhihu-test--references html))))
          (zhihu-test--assert-reference reference 1 "脚注说明。" "")
          (should
           (zhihu--dom-nodes-with-attribute
            dom 'role "doc-biblioref"))
          (should
           (zhihu--dom-nodes-with-attribute
            dom 'role "doc-bibliography"))))))))

(ert-deftest zhihu-reference-conversion-rejects-lossy-input ()
  (skip-unless (executable-find "pandoc"))
  (should-error
   (zhihu--md->html
    (concat
     "正文[^bad]。\n\n"
     "[^bad]: [一](https://one.example) 与 [二](https://two.example)。\n")))
  (should-error
   (zhihu--md->html
    "正文[^bad]。\n\n[^bad]: 第一段。\n\n    第二段。\n"))
  (should-error
   (zhihu--normalize-html
    (concat
     "<p>正文<sup role=\"doc-noteref\"><a href=\"#missing\">1</a></sup></p>"
     "<section role=\"doc-endnotes\"><ol></ol></section>"))))

(ert-deftest zhihu-typst-footnote-rewrite-is-a-no-op-without-footnotes ()
  (let ((html "<!DOCTYPE html><html><body><p>正文</p></body></html>"))
    (should (eq (zhihu--typst-rewrite-footnotes html) html))))

(ert-deftest zhihu-card-link-markers-require-a-standalone-web-link ()
  (skip-unless (executable-find "pandoc"))
  (dolist
      (markdown
       '("before [GitHub](https://example.com \"card\") after\n"
         "[GitHub](/relative \"card\")\n"
         "[](https://example.com \"card\")\n"))
    (should-error (zhihu--md->html markdown)))
  (dolist
      (org
       '("#+ATTR_ZHIHU: :type link-card\n"
         "#+ATTR_ZHIHU: :type link-card\nbefore [[https://example.com][GitHub]]\n"
         "#+ATTR_ZHIHU: :type link-card\n[[/relative][GitHub]]\n"
         "#+ATTR_ZHIHU: :type\n[[https://example.com][GitHub]]\n"))
    (should-error (zhihu--org->html org))))

(ert-deftest zhihu-card-link-filter-leaves-unmarked-links-alone ()
  (skip-unless (executable-find "pandoc"))
  (dolist
      (html
       (list
        (zhihu--md->html
         "[GitHub](https://example.com \"cardinal\")\n")
        (zhihu--org->html
         (concat
          "#+ATTR_ZHIHU: :type future-extension\n"
          "[[https://example.com][GitHub]]\n"))))
    (let ((anchor (zhihu-test--only-anchor html)))
      (should (equal (dom-attr anchor 'href) "https://example.com"))
      (should-not (dom-attr anchor 'data-draft-type)))))

(ert-deftest zhihu-pandoc-filter-temp-file-is-always-removed ()
  (let (filter)
    (cl-letf (((symbol-function 'zhihu--shell-convert)
               (lambda (program args _input)
                 (should (equal program "pandoc"))
                 (setq filter (cadr (member "--lua-filter" args)))
                 (should (and filter (file-exists-p filter)))
                 (error "conversion failed"))))
      (should-error (zhihu--pandoc-to-zhihu-html "org" "Body" nil)))
    (should filter)
    (should-not (file-exists-p filter))))

(ert-deftest zhihu-code-language-recognizes-pre-class ()
  (should
   (equal
    (zhihu--zhihuify-html
     "<pre class=\"python\"><code>x &amp; y</code></pre>")
    "<pre lang=\"python\">x &amp; y</pre>")))

(ert-deftest zhihu-insert-user-mention-supports-all-source-formats ()
  (let ((first-hash "0123456789abcdef0123456789abcdef")
        (second-hash "fedcba9876543210fedcba9876543210")
        (http-requests 0)
        (selection-requests 0))
    (cl-letf
        (((symbol-function 'read-string)
          (lambda (&rest _args) "zhang"))
         ((symbol-function 'zhihu--http)
          (lambda (method url &rest args)
            (cl-incf http-requests)
            (should (equal method "GET"))
            (should
             (equal
              url
              (concat
               "https://www.zhihu.com/people/autocomplete"
               "?token=zhang&max_matches=10&use_similar=0")))
            ;; 该公开搜索接口不应读取或发送浏览器 Cookie。
            (should-not args)
            (list
             :status 200
             :body
             (format
              (concat
               "[[\"entry\","
               "[\"people\",\"张三\",\"zhang-san\","
               "\"https://pic.example/first.jpg\",\"%s\","
               "\"第一位用户\",[1],\"\"],"
               "[\"people\",\"张三\",\"zhang-san-2\","
               "\"https://pic.example/second.jpg\",\"%s\","
               "\"第二位用户\",[2],\"\"]]]")
              first-hash second-hash))))
         ((symbol-function 'completing-read)
          (lambda (_prompt collection &rest _args)
            (cl-incf selection-requests)
            (let ((labels
                   (mapcar
                    (lambda (candidate)
                      (if (consp candidate) (car candidate) candidate))
                    collection)))
              (should (= (length labels) 2))
              (should-not (equal (car labels) (cadr labels)))
              (dolist
                  (case
                   `((,(car labels) "zhang-san" "第一位用户")
                     (,(cadr labels) "zhang-san-2" "第二位用户")))
                (dolist (fragment (cdr case))
                  (should
                   (string-match-p
                    (regexp-quote fragment)
                    (car case)))))
              (cadr labels)))))
      (dolist
          (case
           `((markdown
              "/tmp/test.md"
              ,(concat
                "[@张三](https://www.zhihu.com/people/zhang-san-2 "
                "\"member_mention_fedcba9876543210fedcba9876543210\")"))
             (org
              "/tmp/test.org"
              ,(concat
                "@@html:<a href=\"https://www.zhihu.com/people/"
                "zhang-san-2\" "
                "title=\"member_mention_"
                "fedcba9876543210fedcba9876543210\">"
                "&#64;张三</a>@@"))
             (typst
              "/tmp/test.typ"
              ,(concat
                "#html.elem(\"a\", attrs: "
                "(href: \"https://www.zhihu.com/people/zhang-san-2\", "
                "title: \"member_mention_"
                "fedcba9876543210fedcba9876543210\"), "
                "text(\"@张三\"))"))))
        (with-temp-buffer
          (setq buffer-file-name (nth 1 case))
          (insert "前后")
          (goto-char 2)
          (call-interactively #'zhihu-insert-user-mention)
          (should
           (equal (buffer-string)
                  (concat "前" (nth 2 case) "后")))
          (let* ((source (buffer-string))
                 (html
                  (pcase (car case)
                    ('markdown
                     (when (executable-find "pandoc")
                       (zhihu--md->html source)))
                    ('org
                     (when (executable-find "pandoc")
                       (zhihu--org->html source)))
                    ('typst
                     (when (and (executable-find "typst")
                                (executable-find "pandoc"))
                       (zhihu-test--with-temp-file
                        ".typ" source #'zhihu--source-to-html))))))
            (when html
              (let ((anchor (zhihu-test--only-anchor html)))
                (should
                 (equal (dom-attr anchor 'class) "member_mention"))
                (should
                 (equal
                  (dom-attr anchor 'href)
                  "/people/zhang-san-2"))
                (should
                 (equal (dom-attr anchor 'data-hash)
                        second-hash)))))))
      (should (= http-requests 3))
      (should (= selection-requests 3)))))

(ert-deftest zhihu-column-completion-labels-disambiguate-collisions ()
  (let* ((columns
      '((:id "a" :title "同名")
            (:id "b" :title "同名")
            (:id "c" :title "同名 [a]")))
         (entries
          (zhihu--column-completion-candidates columns))
         (labels (mapcar #'car entries)))
    (should (= (length labels) 3))
    (should (= (length (delete-dups (copy-sequence labels))) 3))
    (should (member "同名 [a]" labels))
    (should
     (equal
      (mapcar (lambda (entry)
                (plist-get (cdr entry) :id))
              entries)
      '("a" "b" "c")))))

(ert-deftest zhihu-markdown-metadata-key-capf-inserts-key-with-colon ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/article.md")
    (insert "---\nzhihu:\n  |\n---\n")
    (goto-char (point-min))
    (search-forward "|")
    (delete-region (1- (point)) (point))
    (let ((capf (zhihu-completion-at-point)))
      (should capf)
      (should (= (nth 0 capf) (nth 1 capf)))
      (should
       (equal
        (all-completions "" (nth 2 capf))
        '("question-id:" "article-id:" "thought-id:")))
      (should (eq (plist-get (nthcdr 3 capf) :exclusive) t))))
  (dolist
      (case
       '(("/tmp/article.md"
          "---\nzhihu:\n  article-id:\n  col| # keep\n---\n"
          "col" "column-id:" "  column-id: # keep")
         ("/tmp/article.markdown"
          "---\nzhihu:\n  column-id: writers\n  art|\n---\n"
          "art" "article-id:" "  article-id:\n")
         ("/tmp/quoted.md"
          "---\n\"zhihu\" : # root\n  'article-id':\n  col|\n---\n"
          "col" "column-id:" "  column-id:\n")
         ("/tmp/crlf.md"
          "---\r\nzhihu:\r\n  art|\r\n---\r\n"
          "art" "article-id:" "  article-id:\r\n")))
    (with-temp-buffer
      (setq buffer-file-name (nth 0 case))
      (insert (nth 1 case))
      (goto-char (point-min))
      (search-forward "|")
      (delete-region (1- (point)) (point))
      (let* ((capf (zhihu-completion-at-point))
             (start (nth 0 capf))
             (end (nth 1 capf))
             (prefix (buffer-substring-no-properties start end))
             (candidates (all-completions prefix (nth 2 capf))))
        (should capf)
        (should (equal prefix (nth 2 case)))
        (should (equal candidates (list (nth 3 case))))
        (should (eq (plist-get (nthcdr 3 capf) :exclusive) t))
        (delete-region start end)
        (goto-char start)
        (insert (car candidates))
        (should (string-match-p
                 (regexp-quote (nth 4 case))
                 (buffer-string)))))))

(ert-deftest zhihu-markdown-metadata-key-context-respects-indentation ()
  (dolist
      (case
       '(("---\nzhihu:\n    art|\n---\n" t)
         ("---\nzhihu:\n    # gap\n\n    art|\n---\n" t)
         ("---\nzhihu:\n    creation-statement: original\n  art|\n---\n"
          nil)
         ("---\nzhihu:\n  creation-statement: original\n    art|\n---\n"
          nil)))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/article.md")
      (insert (car case))
      (goto-char (point-min))
      (search-forward "|")
      (delete-region (1- (point)) (point))
      (if (cadr case)
          (progn
            (should (zhihu--markdown-metadata-key-context))
            (should (zhihu-completion-at-point)))
        (should-not (zhihu--markdown-metadata-key-context))
        (should-not (zhihu-completion-at-point))))))

(ert-deftest zhihu-markdown-metadata-key-context-rejects-non-key-slots ()
  (dolist
      (source
       '("---\nzhihu:\n  article-id:\n---\nart|\n"
         "---\nzhihu:\n  art|\n"
         "---\nzhihu: art|\n---\n"
         "---\nzhihu:\n  nested:\n    art|\n---\n"
         "---\nzhihu:\n  topics:\n    - art|\n---\n"
         "---\nzhihu:\n  article-id: art|\n---\n"
         "---\nzhihu:\n  # art|\n---\n"
         "---\nzhihu:\n  art|:\n---\n"
         "---\nzhihu:\n  art|_bad\n---\n"
         "---\nzhihu:\n  \"art|\n---\n"
         "---\nzhihu: {}\n  art|\n---\n"
         "---\nzhihu: scalar\n  art|\n---\n"
         "---\nZHIHU:\n  art|\n---\n"
         "---\nzhihu:\n  question-id: 1\nzhihu:\n  art|\n---\n"
         "---\nzhihu:\n  creation-statement: original\n  creation-statement: original\n  art|\n---\n"
         "---\nzhihu:\n  question-id: 1\n  article-id:\n  art|\n---\n"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/article.md")
      (insert source)
      (goto-char (point-min))
      (search-forward "|")
      (delete-region (1- (point)) (point))
      (should-not (zhihu--markdown-metadata-key-context))
      (should-not (zhihu-completion-at-point)))))

(ert-deftest zhihu-markdown-metadata-key-scan-is-case-sensitive ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/article.md")
    (insert "---\nzhihu:\n  ARTICLE-ID:\n  |\n---\n")
    (goto-char (point-min))
    (search-forward "|")
    (delete-region (1- (point)) (point))
    (let ((capf (zhihu-completion-at-point)))
      (should capf)
      (should
       (equal
        (all-completions "" (nth 2 capf))
        '("question-id:" "article-id:" "thought-id:"))))))

(ert-deftest zhihu-metadata-key-candidates-follow-content-kind ()
  (should
   (equal
    (zhihu--metadata-key-candidate-fields nil)
    '(:question-id :article-id :thought-id)))
  (should
   (equal
    (zhihu--metadata-key-candidate-fields '(:column-id))
    '(:article-id)))
  (should
   (equal
    (zhihu--metadata-key-candidate-fields '(:topics))
    '(:article-id :thought-id)))
  (should
   (equal
    (zhihu--metadata-key-candidate-fields '(:question-id))
    '(:answer-id :creation-statement :content-source
      :reprint-permission :comment-permission)))
  (should
   (equal
    (zhihu--metadata-key-candidate-fields '(:article-id))
    '(:column-id :creation-statement :content-source
      :reprint-permission :comment-permission :topics)))
  (should
   (equal
    (zhihu--metadata-key-candidate-fields '(:thought-id))
    '(:comment-permission :topics)))
  (should-not
   (zhihu--metadata-key-candidate-fields
    '(:question-id :article-id)))
  (should-not
   (zhihu--metadata-key-candidate-fields
    '(:article-id :article-id)))
  (should-not
   (zhihu--metadata-key-candidate-fields
    '(:question-id :topics))))

(ert-deftest zhihu-typst-metadata-key-capf-inserts-key-with-colon ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/article.typ")
    (insert "#metadata((\n  \n)) <zhihu>\n")
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (let ((capf (zhihu-completion-at-point)))
      (should capf)
      (should
       (equal
        (all-completions "" (nth 2 capf))
        '("question-id:" "article-id:" "thought-id:")))))
  (dolist
      (case
       '(("#metadata((\n  art\n)) <zhihu>\n"
          "art" "article-id:" "  article-id:\n")
         ("#metadata((QUESTION-ID: none, art)) <zhihu>\n"
          "art" "article-id:" "QUESTION-ID: none, article-id:")
         ("#let wrapped = (\n  #metadata((\n    article-id: none,\n    /* gap: , */ col\n  )) <zhihu>\n)\n"
          "col" "column-id:" "    /* gap: , */ column-id:\n")))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/article.typ")
      (insert (nth 0 case))
      (goto-char (point-min))
      (search-forward (nth 1 case))
      (let ((capf (zhihu-completion-at-point)))
        (should capf)
        (let* ((start (nth 0 capf))
               (end (nth 1 capf))
               (table (nth 2 capf))
               (prefix (buffer-substring-no-properties start end))
               (candidates (all-completions prefix table)))
          (should (equal candidates (list (nth 2 case))))
          (should (eq (plist-get (nthcdr 3 capf) :exclusive) t))
          (delete-region start end)
          (goto-char start)
          (insert (car candidates))
          (goto-char (point-min))
          (should (search-forward (nth 3 case) nil t)))))))

(ert-deftest zhihu-typst-metadata-key-context-rejects-non-key-slots ()
  (dolist
      (source
       '("#metadata((article-id: none, nested: (col|),)) <zhihu>\n"
         "#metadata((article-id: none, note: \"col|\",)) <zhihu>\n"
         "#metadata((article-id: none, /* col| */)) <zhihu>\n"
         "#metadata((article-id: none, /* outer /* inner */ col| */)) <zhihu>\n"
         "#metadata((article-id: no|ne,)) <zhihu>\n"
         "#metadata((article-id: none, col|: none,)) <zhihu>\n"
         "#metadata((article-id: none, col|_bad,)) <zhihu>\n"
         "#metadata((article-id: none, COLUMN-ID: wr|iters,)) <zhihu>\n"
         "#METADATA((art|)) <zhihu>\n"
         "#metadata((article-id: none,)) <zhihu>\ncol|\n"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/article.typ")
      (insert source)
      (goto-char (point-min))
      (search-forward "|")
      (let ((origin (1- (point))))
        (delete-region origin (point))
        (goto-char origin))
      (should-not (zhihu--typst-metadata-key-context))
      (should-not (zhihu-completion-at-point)))))

(ert-deftest zhihu-column-id-capf-writes-id-in-all-source-formats ()
  (dolist
      (case
       '((markdown
          "/tmp/article.md"
          "---\ntitle: A\nzhihu:\n  article-id:\n  column-id:\n---\n"
          "  column-id: \"writers\"")
         (org
          "/tmp/article.org"
          "#+TITLE: A\n#+ZHIHU_ARTICLE_ID:\n#+ZHIHU_COLUMN_ID:\n"
          "#+ZHIHU_COLUMN_ID: writers")
         (typst
          "/tmp/article.typ"
          "#metadata((\n  article-id: none,\n  column-id: ,\n)) <zhihu>\n"
          "  column-id: \"writers\",")))
    (with-temp-buffer
      (setq buffer-file-name (nth 1 case))
      (insert (nth 2 case))
      (when (eq (car case) 'org)
        (require 'org)
        (delay-mode-hooks (org-mode)))
      (goto-char (point-min))
      (search-forward
       (if (eq (car case) 'org)
           "COLUMN_ID:"
         "column-id:")
       nil t)
      (unless (eq (car case) 'typst)
        (end-of-line))
      (let ((requests 0))
        (cl-letf
            (((symbol-function 'zhihu--writable-columns)
              (lambda ()
                (cl-incf requests)
                '((:id "writers"
                   :title "写作者专栏"
                   :articles-count 1
                   :voteup-count 2
                   :contributions-count 3)))))
          (let* ((capf (zhihu-completion-at-point))
                 (start (nth 0 capf))
                 (end (nth 1 capf))
                 (table (nth 2 capf))
                 (properties (nthcdr 3 capf))
                 (candidate
                  (car (all-completions "" table)))
                 (annotation
                  (funcall
                   (plist-get properties :annotation-function)
                   candidate)))
            (should (equal candidate "写作者专栏"))
            (should (equal annotation "  ID writers"))
            (delete-region start end)
            (goto-char start)
            (insert candidate)
            ;; Completion frontends may invoke the callback from another
            ;; buffer; source point must still land after the inserted ID.
            (with-temp-buffer
              (funcall
               (plist-get properties :exit-function)
               candidate 'finished))
            (if (eq (car case) 'typst)
                (should (eq (char-after) ?,))
              (should (= (point) (line-end-position))))
            (goto-char (point-min))
            (should (search-forward (nth 3 case) nil t))
            (should (= requests 1))))))))

(ert-deftest zhihu-column-id-context-rejects-nested-and-blocked-fields ()
  (dolist
      (source
       '("---\nzhihu:\n  article-id:\n  nested:\n    column-id:\n---\n"
         "---\nzhihu:\n  article-id:\n---\n\ncolumn-id:\n"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/article.md")
      (insert source)
      (goto-char (point-min))
      (search-forward "column-id:")
      (should-not (zhihu--markdown-column-id-context))))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/article.org")
    (insert
     "#+begin_quote\n#+ZHIHU_COLUMN_ID:\n#+end_quote\n")
    (require 'org)
    (delay-mode-hooks (org-mode))
    (goto-char (point-min))
    (search-forward "COLUMN_ID:")
    (should-not (zhihu--org-column-id-context)))
  (dolist
      (source
       '("#metadata((nested: (column-id: \"x\"),)) <zhihu>\n"
         "#metadata((note: \"column-id: x\",)) <zhihu>\n"
         "#metadata((\n  // column-id: \"x\",\n)) <zhihu>\n"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/article.typ")
      (insert source)
      (goto-char (point-min))
      (search-forward "column-id:")
      (should-not (zhihu--typst-column-id-context)))))

(ert-deftest zhihu-typst-column-context-uses-relative-dictionary-depth ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/article.typ")
    (insert
     (concat
      "#let wrapped = (\n"
      "  #metadata((article-id: none, column-id: ,)) "
      "/* label gap */ <zhihu>\n"
      ")\n"))
    (goto-char (point-min))
    (search-forward "column-id:")
    (should (zhihu--typst-column-id-context)))
  (dolist
      (source
       '("\"#metadata((article-id: none,)) <zhihu>\"\n"
         "// #metadata((article-id: none,)) <zhihu>\n"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/article.typ")
      (insert source)
      (should-not (zhihu--typst-native-metadata-region)))))

(ert-deftest zhihu-column-completion-cache-is-lazy-and-caches-empty-results ()
  (with-temp-buffer
    (let ((requests 0))
      (cl-letf
          (((symbol-function 'zhihu--writable-columns)
            (lambda ()
              (cl-incf requests)
              nil)))
        (should-not (zhihu--cached-writable-columns))
        (should-not (zhihu--cached-writable-columns))
        (should (= requests 1))
        (zhihu--reset-completion-caches)
        (should-not (zhihu--cached-writable-columns))
        (should (= requests 2))))))

(ert-deftest zhihu-markdown-user-mention-capf-is-lazy-and-writes-marker ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/article.md")
    (insert
     "---\nzhihu:\n  article-id:\n---\n\n正文 @zhang")
    (goto-char (point-max))
    (let ((searches 0)
          (user
           '(:name "张三"
             :id "zhang-san"
             :hash "0123456789abcdef0123456789abcdef"
             :description "作者")))
      (cl-letf
          (((symbol-function 'zhihu--search-users)
            (lambda (query)
              (cl-incf searches)
              (should (equal query "zhang"))
              (list user))))
        (let* ((capf (zhihu-completion-at-point))
               (start (nth 0 capf))
               (end (nth 1 capf))
               (table (nth 2 capf))
               (properties (nthcdr 3 capf)))
          (should (= searches 0))
          (let ((candidate
                 (car (all-completions "zhang" table))))
            (should (string-prefix-p "张三" candidate))
            (should (string-match-p "作者" candidate))
            (should (string-match-p "zhang-san" candidate))
            (should (= searches 1))
            (delete-region start end)
            (goto-char start)
            (insert candidate)
            (with-temp-buffer
              (funcall
               (plist-get properties :exit-function)
               candidate 'exact))
            (should (= (point) (point-max)))
            (should
             (string-suffix-p
              (concat
               "正文 [@张三](https://www.zhihu.com/people/zhang-san "
               "\"member_mention_"
               "0123456789abcdef0123456789abcdef\")")
              (buffer-string)))))))))

(ert-deftest zhihu-markdown-user-mention-capf-rejects-non-body-contexts ()
  (dolist
      (source
       '("---\ntitle: \"@zhang\"\nzhihu:\n  article-id:\n---\n"
         "---\nzhihu:\n  article-id:\n---\n\n`@zhang`\n"
         "---\nzhihu:\n  article-id:\n---\n\n`跨行\n@zhang\n代码`\n"
         "---\nzhihu:\n  article-id:\n---\n\n```\n@zhang\n```\n"
         "---\nzhihu:\n  article-id:\n---\n\n> ```\n> @zhang\n> ```\n"
         "---\nzhihu:\n  article-id:\n---\n\n- ```\n  @zhang\n  ```\n"
         "---\nzhihu:\n  article-id:\n---\n\n```\n```still-code\n@zhang\n```\n"
         "---\nzhihu:\n  article-id:\n---\n\n[@zhang](https://example.com)\n"
         "---\nzhihu:\n  article-id:\n---\n\n<a href=\"/\">@zhang</a>\n"
         "---\nzhihu:\n  article-id:\n---\n\nme@zhang\n"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/article.md")
      (insert source)
      (goto-char (point-min))
      (search-forward "@zhang")
      (should-not (zhihu--markdown-user-mention-capf)))))

(ert-deftest zhihu-markdown-html-state-ignores-code-and-closed-comments ()
  (dolist
      (source
       '("---\nzhihu:\n  article-id:\n---\n\n```\n<div>\n```\n@zhang\n"
         "---\nzhihu:\n  article-id:\n---\n\n`<div>` @zhang\n"
         "---\nzhihu:\n  article-id:\n---\n\n<!-- <div> -->\n@zhang\n"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/article.md")
      (insert source)
      (goto-char (point-min))
      (search-forward "@zhang")
      (should (zhihu--markdown-user-mention-bounds)))))

(ert-deftest zhihu-markdown-html-state-handles-many-tags ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/article.md")
    (insert "---\nzhihu:\n  article-id:\n---\n\n")
    (dotimes (_ 500)
      (insert "<br>\n"))
    (insert "@zhang")
    (goto-char (point-max))
    (should (zhihu--markdown-user-mention-bounds))))

(ert-deftest zhihu-mode-manages-capf-and-caches ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/article.md"
          major-mode 'markdown-mode)
    (insert "---\nzhihu:\n  article-id:\n---\n")
    (zhihu-mode 1)
    (should zhihu-mode)
    (should (memq #'zhihu-completion-at-point
                  completion-at-point-functions))
    (setq zhihu--column-completion-cache nil
          zhihu--user-mention-completion-cache
          '(("x"))
          zhihu--topic-completion-cache
          '(("topic")))
    (run-hooks 'after-revert-hook)
    (should
     (eq zhihu--column-completion-cache
         zhihu--column-completion-cache-unloaded))
    (should-not zhihu--user-mention-completion-cache)
    (should-not zhihu--topic-completion-cache)
    (zhihu-mode -1)
    (should-not
     (memq #'zhihu-completion-at-point
           completion-at-point-functions))
    (should-not
     (local-variable-p
      'zhihu--column-completion-cache))
    (should-not
     (local-variable-p
      'zhihu--topic-completion-cache))))

(ert-deftest zhihu-mode-has-no-global-auto-detection ()
  (should-not (fboundp 'zhihu--maybe-enable-mode))
  (dolist (hook '(markdown-mode-hook
                  org-mode-hook
                  typst-ts-mode-hook
                  typst-mode-hook))
    (should-not
     (and (boundp hook)
          (memq #'zhihu--maybe-enable-mode
                (symbol-value hook))))))

(ert-deftest zhihu-topic-capf-writes-names-in-all-source-formats ()
  (dolist
      (case
       (list
        (list
         'markdown
         "/tmp/article.md"
         (concat
          "---\n"
          "zhihu:\n"
          "  article-id:\n"
          "  topics:\n"
          "    - \"Other\"\n"
          "    - \"Em\" # keep\n"
          "---\nBody\n")
         (concat
          "---\n"
          "zhihu:\n"
          "  article-id:\n"
          "  topics:\n"
          "    - \"Other\"\n"
          "    - \"Emacs \\\"Lisp\\\"\" # keep\n"
          "---\nBody\n"))
        (list
         'org
         "/tmp/article.org"
         (concat
          "#+ZHIHU_ARTICLE_ID:\n"
          "#+ZHIHU_TOPICS: [\"Other\", \"Em\"]\n")
         (concat
          "#+ZHIHU_ARTICLE_ID:\n"
          "#+ZHIHU_TOPICS: [\"Other\", \"Emacs \\\"Lisp\\\"\"]\n"))
        (list
         'typst
         "/tmp/article.typ"
         (concat
          "#metadata((article-id: none, "
          "topics: (\"Other\", \"Em\",),)) <zhihu>\n")
         (concat
          "#metadata((article-id: none, "
          "topics: (\"Other\", \"Emacs \\\"Lisp\\\"\",),)) <zhihu>\n"))))
    (with-temp-buffer
      (setq buffer-file-name (nth 1 case))
      (insert (nth 2 case))
      (when (eq (car case) 'org)
        (require 'org)
        (delay-mode-hooks (org-mode)))
      (goto-char (point-min))
      (search-forward "\"Em")
      (let ((searches 0))
        (cl-letf
            (((symbol-function 'zhihu--search-article-topics)
              (lambda (query)
                (cl-incf searches)
                (should (equal query "Em"))
                (list
                 (zhihu-test--topic-record
                  "2" "Emacs \"Lisp\"" "Editor")))))
          (let ((capf (zhihu-completion-at-point)))
            (should (= searches 0))
            (should
             (equal
              (zhihu-test--commit-capf-candidate capf "Em")
              "Emacs \"Lisp\" — Editor (2)"))
            (should (= searches 1))
            (should (equal (buffer-string) (nth 3 case)))))))))

(ert-deftest zhihu-topic-capf-rejects-answer-and-mixed-identities-without-search ()
  (dolist
      (case
       (list
        (list 'markdown "/tmp/answer.md"
              (concat
               "---\nzhihu:\n  question-id: \"123\"\n"
               "  topics:\n    - \"No\"\n---\n"))
        (list 'markdown "/tmp/mixed.md"
              (concat
               "---\nzhihu:\n  article-id:\n  thought-id:\n"
               "  topics:\n    - \"No\"\n---\n"))
        (list 'org "/tmp/answer.org"
              (concat
               "#+ZHIHU_QUESTION_ID: 123\n"
               "#+ZHIHU_TOPICS: [\"No\"]\n"))
        (list 'org "/tmp/mixed.org"
              (concat
               "#+ZHIHU_ARTICLE_ID:\n#+ZHIHU_THOUGHT_ID:\n"
               "#+ZHIHU_TOPICS: [\"No\"]\n"))
        (list 'typst "/tmp/answer.typ"
              (concat
               "#metadata((question-id: \"123\", "
               "topics: (\"No\",),)) <zhihu>\n"))
        (list 'typst "/tmp/mixed.typ"
              (concat
               "#metadata((article-id: none, thought-id: none, "
               "topics: (\"No\",),)) <zhihu>\n"))))
    (with-temp-buffer
      (setq buffer-file-name (nth 1 case))
      (insert (nth 2 case))
      (when (eq (car case) 'org)
        (require 'org)
        (delay-mode-hooks (org-mode)))
      (goto-char (point-min))
      (search-forward "\"No")
      (let ((searches 0))
        (cl-letf
            (((symbol-function 'zhihu--search-article-topics)
              (lambda (&rest _args)
                (cl-incf searches)
                nil)))
          (should-not (zhihu-completion-at-point))
          (should (= searches 0)))))))

(ert-deftest zhihu-article-topic-capf-allows-third-item-and-rejects-fourth ()
  (dolist (count '(3 4))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/article.md")
      (insert
       "---\nzhihu:\n  article-id:\n  topics:\n"
       (mapconcat
        (lambda (index)
          (format "    - \"T%d\"" index))
        (number-sequence 1 count)
        "\n")
       "\n---\n")
      (goto-char (point-min))
      (search-forward (format "\"T%d" count))
      (let ((searches 0))
        (cl-letf
            (((symbol-function 'zhihu--search-article-topics)
              (lambda (query)
                (cl-incf searches)
                (should (equal query "T3"))
                (list (zhihu-test--topic-record "9" "Replacement")))))
          (let ((capf (zhihu-completion-at-point)))
            (if (= count 3)
                (progn
                  (should capf)
                  (zhihu-test--commit-capf-candidate capf "T3")
                  (should (= searches 1))
                  (should
                   (string-match-p
                    (regexp-quote "    - \"Replacement\"")
                    (buffer-string))))
              (should-not capf)
              (should (= searches 0)))))))))

(ert-deftest zhihu-pin-topic-capf-allows-tenth-item-and-rejects-eleventh ()
  (dolist (count '(10 11))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/pin.org")
      (insert
       "#+ZHIHU_THOUGHT_ID:\n#+ZHIHU_TOPICS: ["
       (mapconcat
        (lambda (index) (format "\"T%d\"" index))
        (number-sequence 1 count)
        ", ")
       "]\n")
      (require 'org)
      (delay-mode-hooks (org-mode))
      (goto-char (point-min))
      (search-forward (format "\"T%d" count))
      (let ((searches 0))
        (cl-letf
            (((symbol-function 'zhihu--search-article-topics)
              (lambda (query)
                (cl-incf searches)
                (should (equal query "T10"))
                (list (zhihu-test--topic-record "19" "Replacement")))))
          (let ((capf (zhihu-completion-at-point)))
            (if (= count 10)
                (progn
                  (should capf)
                  (zhihu-test--commit-capf-candidate capf "T10")
                  (should (= searches 1))
                  (should
                   (string-match-p
                    (regexp-quote "\"T9\", \"Replacement\"]")
                    (buffer-string))))
              (should-not capf)
              (should (= searches 0)))))))))

(ert-deftest zhihu-topic-capf-skips-empty-query-caches-and-filters-existing ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/article.md")
    (insert
     (concat
      "---\nzhihu:\n  article-id:\n  topics:\n"
      "    - \"Keep\"\n"
      "    - \"\"\n"
      "---\n"))
    (goto-char (point-min))
    (search-forward "    - \"")
    (search-forward "    - \"")
    (let (queries)
      (cl-letf
          (((symbol-function 'zhihu--search-article-topics)
            (lambda (query)
              (push query queries)
              (pcase query
                ("Em"
                 (list
                  (zhihu-test--topic-record "1" "First")
                  (zhihu-test--topic-record "keep" "Keep")
                  (zhihu-test--topic-record "2" "Second")))
                ("None" nil)
                (_ (ert-fail (format "Unexpected topic query %S" query)))))))
        (let* ((capf (zhihu-completion-at-point))
               (table (nth 2 capf)))
          (should capf)
          (should-not queries)
          (should-not (funcall table "" nil t))
          (should-not queries)
          (should
           (equal
            (funcall table "Em" nil t)
            '("First (1)" "Second (2)")))
          (should
           (equal
            (funcall table "Em" nil t)
            '("First (1)" "Second (2)")))
          (should-not (funcall table "None" nil t))
          (should-not (funcall table "None" nil t))
          (should (equal (nreverse queries) '("Em" "None"))))))))

(ert-deftest zhihu-html-converts-member-mention-links ()
  (let* ((hash "0123456789abcdef0123456789abcdef")
         (anchor
          (zhihu-test--only-anchor
           (zhihu--zhihuify-html
            (format
             (concat
              "<p><a href=\"https://www.zhihu.com/people/zhang-san\" "
              "title=\"member_mention_%s\"><strong>@张三</strong></a></p>")
             hash)))))
    (should (equal (dom-attr anchor 'class) "member_mention"))
    (should (equal (dom-attr anchor 'href) "/people/zhang-san"))
    (should (equal (dom-attr anchor 'data-hash) hash))
    (should-not (dom-attr anchor 'title))
    (let ((strong (car (dom-by-tag anchor 'strong))))
      (should strong)
      (should (equal (dom-inner-text strong) "@张三")))))

(ert-deftest zhihu-html-preserves-ordinary-links ()
  (let ((anchor
         (zhihu-test--only-anchor
          (zhihu--zhihuify-html
           (concat
            "<a class=\"profile\" "
            "href=\"https://www.zhihu.com/people/zhang-san\" "
            "title=\"个人主页\">@张三</a>")))))
    (should (equal (dom-attr anchor 'class) "profile"))
    (should
     (equal
      (dom-attr anchor 'href)
      "https://www.zhihu.com/people/zhang-san"))
    (should (equal (dom-attr anchor 'title) "个人主页"))
    (should-not (dom-attr anchor 'data-hash))))

(ert-deftest zhihu-html-member-mention-keeps-arbitrary-href ()
  (let* ((hash "0123456789abcdef0123456789abcdef")
         (href "https://example.com/not-a-profile")
         (anchor
          (zhihu-test--only-anchor
           (zhihu--zhihuify-html
            (format
             "<a href=\"%s\" title=\"member_mention_%s\">@U</a>"
             href hash)))))
    (should (equal (dom-attr anchor 'class) "member_mention"))
    (should (equal (dom-attr anchor 'href) href))
    (should (equal (dom-attr anchor 'data-hash) hash))))

(ert-deftest zhihu-html-rejects-malformed-member-mention-markers ()
  (dolist
      (link
       (list
        (concat
         "<a href=\"https://www.zhihu.com/people/user\" "
         "title=\"member_mention_\">@U</a>")
        (concat
         "<a href=\"https://www.zhihu.com/people/user\" "
         "title=\"member_mention_0123456789abcdef0123456789abcdeg\">"
         "@U</a>")))
    (should-error (zhihu--zhihuify-html link))))

(ert-deftest zhihu-inner-html-escapes-top-level-text ()
  (should
   (equal (zhihu--inner-html '(body nil "a < b & c > d"))
          "a &lt; b &amp; c &gt; d")))

(ert-deftest zhihu-article-cc-statement-is-optional ()
  (let ((zhihu-article-cc-statement nil)
        (html "<p>正文 &amp; 引文</p>"))
    (should (eq (zhihu--append-article-cc-statement html) html))))

(ert-deftest zhihu-article-cc-statement-supports-cc0 ()
  (let ((zhihu-article-cc-statement 'cc0))
    (should
     (equal
      (zhihu--append-article-cc-statement "<p>正文</p>")
      (concat
       "<p>正文</p>\n"
       "<blockquote><p>除另有声明外，本文中的原创内容已通过 "
       "<a href=\"https://creativecommons.org/publicdomain/zero/"
       "1.0/deed.zh-hans\" rel=\"license\">CC0 1.0 通用</a>"
       "，在法律允许的范围内贡献至公共领域。</p></blockquote>")))))

(ert-deftest zhihu-article-cc-statement-supports-all-six-licenses ()
  (dolist
      (case
       '((by "by" "署名")
         (by-sa "by-sa" "署名—相同方式共享")
         (by-nd "by-nd" "署名—禁止演绎")
         (by-nc "by-nc" "署名—非商业性使用")
         (by-nc-sa "by-nc-sa" "署名—非商业性使用—相同方式共享")
         (by-nc-nd "by-nc-nd" "署名—非商业性使用—禁止演绎")))
    (let* ((zhihu-article-cc-statement (nth 0 case))
           (slug (nth 1 case))
           (name (nth 2 case)))
      (should
       (equal
        (zhihu--append-article-cc-statement "<p>正文 &amp; 引文</p>")
        (format
         (concat
          "<p>正文 &amp; 引文</p>\n"
          "<blockquote><p>除另有声明外，本文中的原创内容采用 "
          "<a href=\"https://creativecommons.org/licenses/%s/4.0/"
          "deed.zh-hans\" rel=\"license\">CC %s 4.0"
          "（%s 4.0 协议国际版）</a> 许可。</p></blockquote>")
         slug (upcase slug) name))))))

(ert-deftest zhihu-article-cc-statement-rejects-unknown-license ()
  (let ((zhihu-article-cc-statement 'unknown))
    (should-error
     (zhihu--append-article-cc-statement "<p>正文</p>"))))

(ert-deftest zhihu-metadata-rejects-floating-point-id ()
  (should-error
   (zhihu--zhihu-meta-from-plist '(:question-id 12.7))))

(ert-deftest zhihu-metadata-requires-an-explicit-content-identity ()
  (dolist (raw '(nil
                 (:column-id "writers")
                 (:answer-id "456")
                 (:topics ["Emacs"])))
    (let ((message
           (condition-case err
               (progn
                 (zhihu--zhihu-meta-from-plist raw)
                 nil)
             (error (error-message-string err)))))
      (should message)
      (should
       (string-match-p
        (regexp-quote
         "必须且只能包含 question-id、article-id 或 thought-id 中的一个")
        message))))
  (let ((meta (zhihu--zhihu-meta-from-plist '(:article-id nil))))
    (should (eq (plist-get meta :kind) 'article))
    (dolist
        (field
         '(:question-id :answer-id :column-id :thought-id :banner :topics
			:creation-statement :content-source :toc
			:reprint-permission :comment-permission))
      (should-not (plist-get meta field)))
    (should (plist-member meta :article-id))
    (should-not (plist-get meta :article-id))
    (should-not (plist-member meta :thought-id))))

(ert-deftest zhihu-document-banner-and-toc-stay-out-of-channel-metadata ()
  (let ((channel
         (zhihu--zhihu-meta-from-plist '(:article-id nil)))
        (source
         (zhihu--source-meta-from-parts
          '(:article-id nil) "Title" "./banner.png" t)))
    (should-not (plist-member channel :title))
    (should-not (plist-member channel :banner))
    (should-not (plist-member channel :toc))
    (should (equal (plist-get source :title) "Title"))
    (should (equal (plist-get source :banner) "./banner.png"))
    (should (plist-get source :toc))
    (should (eq (plist-get source :kind) 'article))))

(ert-deftest zhihu-metadata-rejects-explicit-empty-scalar-fields ()
  (dolist
      (field
       '(:question-id :answer-id :column-id
		      :creation-statement :content-source
		      :reprint-permission :comment-permission))
    (dolist (empty '(nil :null :json-null "" " \t\n"))
      (should-error
       (zhihu--zhihu-meta-from-plist (append (pcase field (:question-id nil) (:answer-id '(:question-id "123")) (_ '(:article-id nil))) (list field empty)))))))

(ert-deftest zhihu-metadata-preserves-empty-content-id-slots ()
  (dolist (case '((:article-id article) (:thought-id pin)))
    (pcase-let ((`(,field ,kind) case))
      (dolist (empty '(nil :null :json-null "" " \t\n"))
        (let ((meta
               (zhihu--zhihu-meta-from-plist (list field empty))))
          (should (eq (plist-get meta :kind) kind))
          (should (plist-member meta field))
          (should-not (plist-get meta field))
          (should
           (equal
            (zhihu--metadata-scalar-entries meta)
            (list (cons field nil)))))))))

(ert-deftest zhihu-metadata-normalizes-article-topics ()
  (dolist (raw
           '((" Emacs " "org-mode")
             [" Emacs " "org-mode"]))
    (let ((meta
           (zhihu--zhihu-meta-from-plist (list :article-id nil :topics raw))))
      (should (eq (plist-get meta :kind) 'article))
      (should
       (equal (plist-get meta :topics)
              '("Emacs" "org-mode")))))
  (should-not
   (plist-get
    (zhihu--zhihu-meta-from-plist '(:article-id nil))
    :topics)))

(ert-deftest zhihu-metadata-rejects-invalid-article-topics ()
  (dolist (raw
           (list nil :null :json-null [] '()
                 "Emacs" '("Emacs" 42)
                 '("Emacs" " Emacs ")
                 (list (concat "bad" (string 1)))
                 '("one" "two" "three" "four")))
    (should-error
     (zhihu--zhihu-meta-from-plist (list :article-id nil :topics raw))))
  (should-error
   (zhihu--zhihu-meta-from-plist '(:question-id "123" :topics ["Emacs"]))))

(ert-deftest zhihu-metadata-accepts-explicit-false-toc ()
  (dolist (false '(:false :json-false "false"))
    (dolist (identity '((:article-id nil)
                        (:question-id "123")
                        (:thought-id nil)))
      (let ((meta
             (zhihu--source-meta-from-parts identity nil nil false)))
        (should-not (plist-get meta :toc))))))

(ert-deftest zhihu-metadata-normalizes-publish-settings ()
  (let ((article
         (zhihu--source-meta-from-parts
          '(:article-id "456"
            :creation-statement "AI_CREATION"
            :content-source "TVMEDIA"
            :reprint-permission "need_payment"
            :comment-permission "followee")
          nil "./images/cover.jpg" "true"))
        (answer
         (zhihu--zhihu-meta-from-plist '(:question-id "123" :creation-statement "medical_advice" :content-source "newsReport" :reprint-permission "disallowed" :comment-permission "nobody"))))
    (should
     (equal (plist-get article :creation-statement) "ai_creation"))
    (should (equal (plist-get article :content-source) "TVMedia"))
    (should
     (equal (plist-get article :banner) "./images/cover.jpg"))
    (should (plist-get article :toc))
    (should
     (equal (plist-get article :reprint-permission) "need_payment"))
    (should
     (equal (plist-get article :comment-permission) "followee"))
    (should
     (equal (plist-get answer :creation-statement) "medical_advice"))
    (should (equal (plist-get answer :content-source) "newsReport"))
    (should
     (equal (plist-get answer :reprint-permission) "disallowed"))
    (should (equal (plist-get answer :comment-permission) "nobody")))
  (let ((answer
         (zhihu--source-meta-from-parts
          '(:question-id "123") nil "./cover.jpg" t)))
    (should (eq (plist-get answer :kind) 'answer))
    (should (equal (plist-get answer :banner) "./cover.jpg"))
    (should (plist-get answer :toc)))
  (dolist
      (field-and-value
       '((:creation-statement "ai-assisted")
         (:content-source "socialMedia")
         (:reprint-permission "paid")
         (:comment-permission "followees")))
    (should-error
     (zhihu--zhihu-meta-from-plist
      (list :article-id "456"
            (car field-and-value)
            (cadr field-and-value)))))
  (should-error
   (zhihu--source-meta-from-parts
    '(:article-id "456") nil nil "yes"))
  (dolist (field '(:toc :enable-table-of-contents))
    (dolist (value '(t :false :json-false "true" "false"))
      (should-error
       (zhihu--zhihu-meta-from-plist
        (list :article-id "456" field value))))))

(ert-deftest zhihu-metadata-scalar-entries-are-shared-and-ordered ()
  (let ((article
         (zhihu--source-meta-from-parts
          '(:article-id "456"
            :column-id "writers"
            :creation-statement "AI_CREATION"
            :content-source "officialWebsite"
            :reprint-permission "need_payment"
            :comment-permission "followee")
          nil "./images/cover.jpg" t))
        (answer
         (zhihu--zhihu-meta-from-plist '(:question-id "123"))))
    (should (plist-get article :toc))
    (should
     (equal
      (zhihu--metadata-scalar-entries article)
      '((:article-id . "456")
        (:column-id . "writers")
        (:creation-statement . "ai_creation")
        (:content-source . "officialWebsite")
        (:reprint-permission . "need_payment")
        (:comment-permission . "followee"))))
    (should
     (equal
      (zhihu--metadata-scalar-entries answer)
      '((:question-id . "123")))))
  (should-error (zhihu--metadata-scalar-entries '(:kind unknown))))

(ert-deftest zhihu-metadata-infers-kind-and-empty-article-id ()
  (dolist
      (raw
       '((:question-id nil)
         (:question-id "123" :answer-id nil)))
    (should-error (zhihu--zhihu-meta-from-plist raw)))
  (let ((meta
         (zhihu--zhihu-meta-from-plist '(:article-id nil))))
    (should (eq (plist-get meta :kind) 'article))
    (should (plist-member meta :article-id))
    (should-not (plist-get meta :article-id))))

(ert-deftest zhihu-metadata-rejects-mixed-answer-and-article-identities ()
  (dolist
      (raw
       '((:question-id "123" :article-id "456")
         (:question-id "123" :article-id nil)
         (:question-id "123" :column-id "writers")))
    (should-error (zhihu--zhihu-meta-from-plist raw))))

(ert-deftest zhihu-metadata-infers-and-canonicalizes-pin-kind ()
  (let ((new
         (zhihu--source-meta-from-parts '(:thought-id nil :comment-permission "follower_n_days") "想法" nil))
        (existing
         (zhihu--zhihu-meta-from-plist '(:thought-id 123))))
    (should (eq (plist-get new :kind) 'pin))
    (should (plist-member new :thought-id))
    (should-not (plist-get new :thought-id))
    (should
     (equal (plist-get new :comment-permission) "follower_n_days"))
    (should (eq (plist-get existing :kind) 'pin))
    (should (equal (plist-get existing :thought-id) "123"))
    (should
     (equal
      (zhihu--metadata-scalar-entries existing)
      '((:thought-id . "123"))))))

(ert-deftest zhihu-metadata-rejects-thought-identity-conflicts ()
  (dolist
      (raw
       '((:thought-id nil :question-id "1")
         (:thought-id nil :answer-id "2")
         (:thought-id nil :article-id nil)
         (:thought-id nil :column-id "writers")
         (:thought-id "4" :question-id "1")
         (:thought-id "4" :article-id "3")))
    (should-error (zhihu--zhihu-meta-from-plist raw))))

(ert-deftest zhihu-metadata-rejects-unsupported-pin-fields ()
  (dolist
      (entry
       '((:creation-statement "spoiler")
         (:content-source "officialWebsite")
         (:reprint-permission "allowed")))
    (should-error
     (zhihu--zhihu-meta-from-plist (append '(:thought-id nil) entry))))
  (let ((meta
         (zhihu--source-meta-from-parts
          '(:thought-id nil) nil "./banner.png" t)))
    (should (equal (plist-get meta :banner) "./banner.png"))
    (should (plist-get meta :toc))))

(ert-deftest zhihu-pin-topics-use-the-pin-limit ()
  (let ((ten (cl-loop for index from 1 to 10
                      collect (format "T%d" index)))
        (eleven (cl-loop for index from 1 to 11
                         collect (format "T%d" index))))
    (should
     (equal
      (plist-get
       (zhihu--zhihu-meta-from-plist (list :thought-id nil :topics ten))
       :topics)
      ten))
    (should-error
     (zhihu--zhihu-meta-from-plist (list :thought-id nil :topics eleven)))))

(ert-deftest zhihu-markdown-generic-toc-is-strict-and-channel-independent ()
  (let ((meta
         (zhihu--md-parse-frontmatter-meta
          (concat
           "toc: false\n"
           "zhihu:\n"
           "  article-id:\n"))))
    (should (eq (plist-get meta :kind) 'article))
    (should-not (plist-get meta :toc)))
  (let ((meta
         (zhihu--md-parse-frontmatter-meta
          "toc: true\nzhihu:\n  question-id: \"123\"\n")))
    (should (eq (plist-get meta :kind) 'answer))
    (should (plist-get meta :toc)))
  (should-error
   (zhihu--md-parse-frontmatter-meta
    "toc: null\nzhihu:\n  article-id:\n"))
  (should-error
   (zhihu--md-parse-frontmatter-meta
    "zhihu:\n  article-id:\n  toc: true\n"))
  (should-error
   (zhihu--md-parse-frontmatter-meta
    "zhihu:\n  article-id:\n  enable-table-of-contents: true\n")))

(ert-deftest zhihu-markdown-article-topics-round-trip-as-strings ()
  (let* ((meta
          (zhihu--zhihu-meta-from-plist '(:article-id nil :topics ["123" "true" "comma, quote\""])))
         (yaml (zhihu--format-zhihu-yaml meta))
         (round-tripped
          (zhihu--md-parse-frontmatter-meta yaml)))
    (should
     (string-match-p
      (regexp-quote
       "  topics:\n    - \"123\"\n    - \"true\"\n    - \"comma, quote\\\"\"")
      yaml))
    (should
     (equal (plist-get round-tripped :topics)
            '("123" "true" "comma, quote\"")))))

(ert-deftest zhihu-markdown-rejects-invalid-article-topics ()
  (dolist
      (yaml
       '("zhihu:\n  article-id:\n  topics: []\n"
         "zhihu:\n  article-id:\n  topics: Emacs\n"
         "zhihu:\n  article-id:\n  topics: {name: Emacs}\n"
         "zhihu:\n  article-id:\n  topics: null\n"))
    (should-error (zhihu--md-parse-frontmatter-meta yaml))))

(ert-deftest zhihu-markdown-preserves-string-valued-scalars ()
  (dolist (title '("123" "true" "null"))
    (let* ((source-meta
            (zhihu--source-meta-from-parts '(:article-id nil :column-id "123") nil "true"))
           (content
            (zhihu--format-new-source-metadata
             'markdown
             (plist-put source-meta :title title)))
           (frontmatter
            (car (zhihu--md-split-frontmatter content)))
           (meta (zhihu--md-parse-frontmatter-meta frontmatter))
           (yaml
            (yaml-parse-string frontmatter
                               :object-type 'plist
                               :string-values t)))
      (should (eq (plist-get meta :kind) 'article))
      (should (equal (plist-get meta :title) title))
      (should (equal (plist-get meta :column-id) "123"))
      (should (equal (plist-get meta :banner) "true"))
      (should (equal (plist-get yaml :title) title))
      (should (equal (plist-get yaml :banner) "true"))
      (should-not (plist-member (plist-get yaml :zhihu) :banner))
      (should-not (plist-member (plist-get yaml :zhihu) :title)))))

(ert-deftest zhihu-question-title-uses-the-question-endpoint ()
  (let (request)
    (cl-letf
        (((symbol-function 'zhihu--http-json)
          (lambda (&rest args)
            (setq request args)
            '(:status 200
		      :json (:title "  Question title  ")
		      :body ""))))
      (should
       (equal (zhihu--question-title "123")
              "Question title")))
    (should
     (equal request
            '("GET" "https://www.zhihu.com/api/v4/questions/123")))))

(ert-deftest zhihu-new-answer-enables-mode-explicitly ()
  (let (created-meta mode-argument)
    (cl-letf
        (((symbol-function 'zhihu--create-source-file)
          (lambda (file meta)
            (should (equal file "/tmp/answer.md"))
            (setq created-meta meta)
            file))
         ((symbol-function 'zhihu-mode)
          (lambda (&optional argument)
            (setq mode-argument argument))))
      (should
       (equal
        (zhihu-new-answer "123" "/tmp/answer.md")
        "/tmp/answer.md")))
    (should (eq (plist-get created-meta :kind) 'answer))
    (should (equal (plist-get created-meta :question-id) "123"))
    (should (= mode-argument 1))))

(ert-deftest zhihu-create-source-uses-canonical-meta-and-rejects-existing-file ()
  (dolist (raw-meta
           '((:question-id "123")
             (:article-id nil :column-id "writers")
             (:thought-id nil)))
    (let* ((directory (make-temp-file "zhihu-test-source-" t))
           (meta (zhihu--zhihu-meta-from-plist raw-meta))
           (kind (plist-get meta :kind))
           (parent (expand-file-name "missing/parents" directory))
           (file
            (expand-file-name
             (format "%s-source.md" kind)
             parent))
           opened)
      (unwind-protect
          (cl-letf (((symbol-function 'find-file)
                     (lambda (path)
                       (push path opened)))
                    ((symbol-function 'zhihu--question-title)
                     (lambda (question-id)
                       (should (equal question-id "123"))
                       "Question")))
            (should-not (file-exists-p parent))
            (zhihu--create-source-file file meta)
            (should (file-directory-p parent))
            (should-error (zhihu--create-source-file file meta)
                          :type 'user-error)
            (should (equal opened (list file)))
            (let ((written-meta (zhihu--read-source-meta file)))
              (dolist (key
                       '(:kind :question-id :answer-id
                               :article-id :column-id :thought-id))
                (should (equal (plist-get written-meta key)
                               (plist-get meta key))))
              (dolist (key '(:article-id :thought-id))
                (should
                 (eq (and (plist-member written-meta key) t)
                     (and (plist-member meta key) t))))
              (should
               (equal (plist-get written-meta :title)
                      (and (eq kind 'answer) "Question")))))
        (delete-directory directory t)))))

(ert-deftest zhihu-new-source-requires-a-supported-file-path ()
  (let ((directory (make-temp-file "zhihu-test-source-" t ".md")))
    (unwind-protect
        (progn
          (should-error (zhihu--new-source-spec directory)
                        :type 'user-error)
          (should-error
           (zhihu--new-source-spec
            (expand-file-name "article.txt" (file-name-directory directory)))
           :type 'user-error)
          (should-error
           (zhihu--new-source-spec
            (expand-file-name "   .md" (file-name-directory directory)))
           :type 'user-error)
          (should-error
           (zhihu--new-source-spec
            (expand-file-name
             (concat "article" (string 1) ".md")
             (file-name-directory directory)))
           :type 'user-error)
          (dolist (name '("..md" "...md"))
            (should-error
             (zhihu--new-source-spec
              (expand-file-name name (file-name-directory directory)))
             :type 'user-error))
          (let ((default-directory
                 "/ssh:example.invalid:/tmp/zhihu-test/"))
            (should-error (zhihu--new-source-spec "article.md")
                          :type 'user-error)))
      (delete-directory directory t))))

(ert-deftest zhihu-create-source-rejects-a-dangling-symlink ()
  (let* ((directory (make-temp-file "zhihu-test-source-" t))
         (file (expand-file-name "article.md" directory))
         (meta
          (zhihu--zhihu-meta-from-plist '(:article-id nil))))
    (unwind-protect
        (progn
          (make-symbolic-link "missing.md" file)
          (should (file-symlink-p file))
          (should-not (file-exists-p file))
          (should-error (zhihu--create-source-file file meta)
                        :type 'user-error)
          (should (file-symlink-p file)))
      (delete-directory directory t))))

(ert-deftest zhihu-new-source-preserves-empty-content-id-slots ()
  (let* ((article
          (plist-put
           (zhihu--zhihu-meta-from-plist '(:article-id nil))
           :title "Article"))
         (answer
          (plist-put
           (zhihu--zhihu-meta-from-plist '(:question-id "123"))
           :title "Question"))
         (pin
          (plist-put
           (zhihu--zhihu-meta-from-plist '(:thought-id nil))
           :title "")))
    (should
     (equal (zhihu--format-new-source-metadata 'typst article)
            (concat
             "#metadata((\n"
             "  article-id: none,\n"
             ")) <zhihu>\n\n"
             "#set document(title: \"Article\")\n\n")))
    (should
     (equal (zhihu--format-new-source-metadata 'markdown article)
            (concat
             "---\n"
             "title: \"Article\"\n"
             "zhihu:\n"
             "  article-id:\n"
             "---\n\n")))
    (should
     (equal (zhihu--format-new-source-metadata 'org article)
            "#+TITLE: Article\n#+ZHIHU_ARTICLE_ID:\n\n"))
    (dolist (format '(typst markdown org))
      (let ((source (zhihu--format-new-source-metadata format answer)))
        (should (string-match-p
                 (if (eq format 'org)
                     "ZHIHU_QUESTION_ID: 123"
                   "question-id: \"123\"")
                 source))
        (should-not
         (string-match-p
          (if (eq format 'org) "ZHIHU_ANSWER_ID" "answer-id")
          source))))
    (dolist (format '(typst markdown org))
      (let ((source (zhihu--format-new-source-metadata format pin)))
        (should
         (string-match-p
          (if (eq format 'org)
              "ZHIHU_THOUGHT_ID:[[:space:]]*$"
            (if (eq format 'typst)
                "thought-id: none"
              "thought-id:[[:space:]]*$"))
          source))))))

(ert-deftest zhihu-new-source-writes-canonical-generic-document-fields ()
  (let ((article
         (plist-put
          (zhihu--source-meta-from-parts
           '(:article-id nil) nil "./cover.jpg" t)
          :title "Article")))
    (should
     (equal
      (zhihu--format-new-source-metadata 'typst article)
      (concat
       "#metadata(\"./cover.jpg\") <banner>\n"
       "#metadata((\n"
       "  article-id: none,\n"
       ")) <zhihu>\n\n"
       "#set document(title: \"Article\")\n\n"
       "#outline()\n\n")))
    (should
     (equal
      (zhihu--format-new-source-metadata 'markdown article)
      (concat
       "---\n"
       "title: \"Article\"\n"
       "banner: \"./cover.jpg\"\n"
       "toc: true\n"
       "zhihu:\n"
       "  article-id:\n"
       "---\n\n")))
    (should
     (equal
      (zhihu--format-new-source-metadata 'org article)
      (concat
       "#+TITLE: Article\n"
       "#+BANNER: ./cover.jpg\n"
       "#+TOC: headlines 2\n"
       "#+ZHIHU_ARTICLE_ID:\n\n")))))

(ert-deftest zhihu-pin-metadata-round-trips-all-source-formats ()
  (let ((meta
         (zhihu--source-meta-from-parts '(:thought-id "456" :topics ["Emacs" "org-mode"] :comment-permission "follower_n_days") "Pin title" "./banner.png")))
    (dolist (case '((typst . ".typ")
                    (markdown . ".md")
                    (org . ".org")))
      (pcase-let ((`(,format . ,suffix) case))
        (zhihu-test--with-temp-file
         suffix
         (zhihu--format-new-source-metadata format meta)
         (lambda (file)
           (let ((round-tripped (zhihu--read-source-meta file)))
             (should (eq (plist-get round-tripped :kind) 'pin))
             (should (equal (plist-get round-tripped :thought-id) "456"))
             (should
              (equal (plist-get round-tripped :topics)
                     '("Emacs" "org-mode")))
             (should
              (equal
               (plist-get round-tripped :comment-permission)
               "follower_n_days"))
             (should
              (equal (plist-get round-tripped :banner)
                     "./banner.png")))))))))

(ert-deftest zhihu-empty-content-id-slots-round-trip-all-source-formats ()
  (dolist (identity '((:article-id article) (:thought-id pin)))
    (pcase-let ((`(,field ,kind) identity))
      (let ((meta
             (zhihu--source-meta-from-parts (list field nil) "Title" nil)))
        (dolist (case '((typst . ".typ")
                        (markdown . ".md")
                        (org . ".org")))
          (pcase-let ((`(,format . ,suffix) case))
            (let* ((field-name (substring (symbol-name field) 1))
                   (source
                    (zhihu--format-new-source-metadata format meta))
                   (empty-marker
                    (pcase format
                      ('typst (format "  %s: none," field-name))
                      ('markdown (format "  %s:" field-name))
                      ('org
                       (format
                        "#+%s:"
                        (zhihu--org-metadata-keyword field))))))
              (should
               (string-match-p
                (concat "^" (regexp-quote empty-marker) "$")
                source))
              (zhihu-test--with-temp-file
               suffix source
               (lambda (file)
                 (let ((round-tripped (zhihu--read-source-meta file)))
                   (should (eq (plist-get round-tripped :kind) kind))
                   (should (plist-member round-tripped field))
                   (should-not (plist-get round-tripped field))
                   (zhihu--write-zhihu-meta file round-tripped)
                   (setq round-tripped (zhihu--read-source-meta file))
                   (should (eq (plist-get round-tripped :kind) kind))
                   (should (plist-member round-tripped field))
                   (should-not (plist-get round-tripped field))
                   (setq round-tripped
                         (plist-put round-tripped field "456"))
                   (zhihu--write-zhihu-meta file round-tripped)
                   (let ((updated (zhihu--read-source-meta file)))
                     (should (eq (plist-get updated :kind) kind))
                     (should (equal (plist-get updated field) "456")))))))))))))

(ert-deftest zhihu-sources-require-an-explicit-content-identity ()
  (let ((message
         (condition-case err
             (progn
               (zhihu--md-parse-frontmatter-meta "title: Markdown")
               nil)
           (error (error-message-string err)))))
    (should message)
    (should
     (string-match-p
      (regexp-quote
       "必须且只能包含 question-id、article-id 或 thought-id 中的一个")
      message)))
  (zhihu-test--with-temp-file
   ".org"
   "#+TITLE: Org\n\nBody\n"
   (lambda (file)
     (let ((message
            (condition-case err
                (progn
                  (zhihu--org-read-meta file)
                  nil)
              (error (error-message-string err)))))
       (should message)
       (should
        (string-match-p
         (regexp-quote
          "必须且只能包含 question-id、article-id 或 thought-id 中的一个")
         message)))))
  (cl-letf (((symbol-function 'zhihu--typst-query-metadata)
             (lambda (_file) nil)))
    (let ((message
           (condition-case err
               (progn
                 (zhihu--read-source-meta "/tmp/article.typ")
                 nil)
             (error (error-message-string err)))))
      (should message)
      (should
       (string-match-p
        (regexp-quote
         "必须且只能包含 question-id、article-id 或 thought-id 中的一个")
        message)))))

(ert-deftest zhihu-writers-preserve-an-empty-article-id-slot ()
  (let ((meta
         (zhihu--zhihu-meta-from-plist '(:article-id nil))))
    (dolist
        (case
         `((".typ"
            . ("#metadata(\"./cover.jpg\") <banner>\n\
#metadata((article-id: \"456\")) <zhihu>\n\
#set document(title: \"Title\")\n\nBody\n"
               ,#'zhihu--typst-write-native-metadata
               "article-id: \"456\""
               "article-id: none"
               "<banner>"))
           (".md"
            . ("---\ntitle: Title\nbanner: \"./cover.jpg\"\n\
zhihu:\n  article-id: \"456\"\n\
other: keep\n---\nBody\n"
               ,#'zhihu--md-write-zhihu-meta
               "article-id: \"456\""
               "article-id:"
               "banner: \"./cover.jpg\""))
           (".org"
            . ("#+TITLE: Title\n#+BANNER: ./cover.jpg\n\
#+ZHIHU_ARTICLE_ID: 456\n\
#+OTHER: keep\n\nBody\n"
               ,#'zhihu--org-write-zhihu-meta
               "ZHIHU_ARTICLE_ID: 456"
               "ZHIHU_ARTICLE_ID:"
               "#+BANNER: ./cover.jpg"))))
      (pcase-let
          ((`(,suffix
              . (,source ,writer ,old-marker ,empty-marker ,banner-marker))
            case))
        (zhihu-test--with-temp-file
         suffix
         source
         (lambda (file)
           (funcall writer file meta)
           (let ((once
                  (with-temp-buffer
                    (insert-file-contents file)
                    (buffer-string))))
             (should-not
              (string-match-p (regexp-quote old-marker) once))
             (should
              (string-match-p (regexp-quote empty-marker) once))
             (should (string-match-p (regexp-quote banner-marker) once))
             (should (string-match-p "Title" once))
             (should (string-match-p "Body" once))
             (let ((round-tripped (zhihu--read-source-meta file)))
               (should (eq (plist-get round-tripped :kind) 'article))
               (should (plist-member round-tripped :article-id))
               (should-not (plist-get round-tripped :article-id)))
             (funcall writer file meta)
             (should
              (equal
               once
               (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))))))))))

(ert-deftest zhihu-markdown-yaml-strings-use-json-escaping ()
  (let* ((column-id (concat "quote\" slash\\ newline\n tab\t control-"
                            (string 1)))
         (yaml
          (zhihu--format-zhihu-yaml
           (list :kind 'article
                 :article-id nil
                 :column-id column-id))))
    (should
     (string-match-p
      (regexp-quote
       "column-id: \"quote\\\" slash\\\\ newline\\n tab\\t control-\\u0001\"")
      yaml))))

(ert-deftest zhihu-markdown-splits-empty-and-eof-frontmatter ()
  (should
   (equal (zhihu--md-split-frontmatter "---\n---\n正文\n")
          '("" . "正文\n")))
  (should
   (equal (zhihu--md-split-frontmatter "---\ntitle: T\n---")
          '("title: T" . ""))))

(ert-deftest zhihu-markdown-rejects-unclosed-frontmatter ()
  (dolist (text '("---\n"
                  "---\ntitle: T\n"
                  "---\ntitle: T\n正文"))
    (let ((message
           (condition-case err
               (progn
                 (zhihu--md-split-frontmatter text)
                 nil)
             (error (error-message-string err)))))
      (should message)
      (should
       (string-match-p
        (regexp-quote
         "Markdown front matter 缺少结束分隔符 ---")
        message))))
  ;; A thematic break outside the first line is ordinary Markdown body.
  (let ((body "正文\n---\n仍是正文\n"))
    (should
     (equal (zhihu--md-split-frontmatter body)
            (cons nil body)))))

(ert-deftest zhihu-markdown-unclosed-frontmatter-read-and-write-are-atomic ()
  (let ((source "---\ntitle: \"Title\"\n正文\n"))
    (zhihu-test--with-temp-file
     ".md" source
     (lambda (file)
       (should-error (zhihu--md-read-meta file))
       (should-error
        (zhihu--md-write-zhihu-meta
         file
         (list :kind 'article
               :article-id "456")))
       (should
        (equal
         (with-temp-buffer
           (insert-file-contents file)
           (buffer-string))
         source))))))

(ert-deftest zhihu-markdown-writer-replaces-zhihu-block-across-blank-lines ()
  (zhihu-test--with-temp-file
   ".md"
   (concat
    "---\n"
    "title: \"Title\"\n"
    "zhihu:\n"
    "  question-id: \"123\"\n"
    "\n"
    "# zhihu mapping 内部的顶层注释\n"
    "  # 以及缩进注释\n"
    "  answer-id: \"old\"\n"
    "other: keep\n"
    "---\n"
    "正文\n")
   (lambda (file)
     (zhihu--md-write-zhihu-meta
      file
      (list :kind 'answer
            :question-id "123"
            :answer-id "456"))
     (let ((source
            (with-temp-buffer
              (insert-file-contents file)
              (buffer-string))))
       (should-not (string-match-p "answer-id: \"old\"" source))
       (should (string-match-p "^other: keep$" source))
       (let ((meta (zhihu--md-read-meta file)))
         (should (equal (plist-get meta :question-id) "123"))
         (should (equal (plist-get meta :answer-id) "456")))))))

(ert-deftest zhihu-markdown-rejects-duplicate-channel-metadata ()
  (dolist
      (frontmatter
       '("zhihu:\n  article-id:\n  article-id: \"456\"\n"
         "zhihu:\n  article-id:\nzhihu:\n  thought-id:\n"
         "zhihu:\n  article-id:\n  topics: [\"A\"]\n  topics: [\"B\"]\n"))
    (let ((message
           (condition-case err
               (progn
                 (zhihu--md-parse-frontmatter-meta frontmatter)
                 nil)
             (error (error-message-string err)))))
      (should message)
      (should (string-match-p "不能重复" message)))))

(ert-deftest zhihu-markdown-zhihu-mapping-is-block-and-writer-locatable ()
  (should-error
   (zhihu--md-parse-frontmatter-meta
    "zhihu: {article-id: null}\n"))
  (dolist (key '("\"zhihu\" :" "'zhihu':"))
    (zhihu-test--with-temp-file
     ".md"
     (format "---\ntitle: Article\n%s\n  article-id:\n---\n" key)
     (lambda (file)
       (let ((meta (zhihu--md-read-meta file)))
         (should (eq (plist-get meta :kind) 'article))
         (should (plist-member meta :article-id))
         (setq meta (plist-put meta :article-id "456"))
         (zhihu--md-write-zhihu-meta file meta))
       (let ((source
              (with-temp-buffer
                (insert-file-contents file)
                (buffer-string))))
         (should (= (zhihu--md-top-level-key-count source "zhihu") 1))
         (should (string-match-p "^zhihu:$" source))
         (should
          (equal
           (plist-get (zhihu--md-read-meta file) :article-id)
           "456")))))))

(ert-deftest zhihu-markdown-publish-settings-round-trip ()
  (zhihu-test--with-temp-file
   ".md"
   (concat
    "---\n"
    "title: \"Title\"\n"
    "banner: \"./images/cover.jpg\"\n"
    "toc: true\n"
    "zhihu:\n"
    "  article-id: \"456\"\n"
    "  creation-statement: ai_creation\n"
    "  content-source: officialWebsite\n"
    "  reprint-permission: need_payment\n"
    "  comment-permission: followee\n"
    "---\n"
    "正文\n")
   (lambda (file)
     (let ((meta (zhihu--md-read-meta file)))
       (should
        (equal
         (mapcar (lambda (key) (plist-get meta key))
                 '(:banner :creation-statement :content-source
			   :toc
			   :reprint-permission :comment-permission))
         '("./images/cover.jpg" "ai_creation" "officialWebsite"
           t "need_payment" "followee")))
       (zhihu--md-write-zhihu-meta file meta)
       (let ((round-tripped (zhihu--md-read-meta file)))
         (should
          (equal
           (mapcar (lambda (key) (plist-get round-tripped key))
                   '(:banner :creation-statement :content-source
			     :toc
			     :reprint-permission :comment-permission))
           '("./images/cover.jpg" "ai_creation" "officialWebsite"
             t "need_payment" "followee"))))
       (dolist (key
                '(:creation-statement :content-source
				      :reprint-permission :comment-permission))
         (setq meta (plist-put meta key nil)))
       (zhihu--md-write-zhihu-meta file meta)
       (let ((source
              (with-temp-buffer
                (insert-file-contents file)
                (buffer-string))))
         (dolist (field
                  '("creation-statement" "content-source"
                    "reprint-permission" "comment-permission"))
           (should-not (string-match-p field source)))
         (should (string-match-p "^toc: true$" source))
         (should-not (string-match-p "^[ \t]+toc:" source))
         (should
          (string-match-p
           (regexp-quote "banner: \"./images/cover.jpg\"")
           source))
         (should-not
          (string-match-p
           (regexp-quote "  banner: \"./images/cover.jpg\"")
           source)))))))

(ert-deftest zhihu-markdown-banner-is-top-level-only-and-strict ()
  (let ((meta
         (zhihu--md-parse-frontmatter-meta
          (concat
           "banner: \"./canonical.jpg\"\n"
           "zhihu:\n"
           "  question-id: \"123\"\n"
           "  banner: \"./ignored.jpg\"\n"))))
    (should (eq (plist-get meta :kind) 'answer))
    (should (equal (plist-get meta :banner) "./canonical.jpg")))
  (should-not
   (plist-get
    (zhihu--md-parse-frontmatter-meta
     "zhihu:\n  article-id:\n  banner: \"./ignored.jpg\"\n")
    :banner))
  (dolist
      (frontmatter
       '("banner: null\nzhihu:\n  article-id:\n"
         "banner: \"\"\nzhihu:\n  article-id:\n"
         "banner: [\"./cover.jpg\"]\nzhihu:\n  article-id:\n"
         "banner: one\nbanner: two\nzhihu:\n  article-id:\n"
         "'banner': one\n\"banner\": two\nzhihu:\n  article-id:\n"))
    (should-error (zhihu--md-parse-frontmatter-meta frontmatter))))

(ert-deftest zhihu-org-rejects-empty-and-duplicate-keywords ()
  (dolist
      (source
       '("#+TITLE: Test\n#+ZHIHU_QUESTION_ID:\n"
         "#+TITLE: Test\n#+ZHIHU_QUESTION_ID:   \n"
         "#+TITLE: Test\n#+ZHIHU_QUESTION_ID: 123\n\
#+ZHIHU_QUESTION_ID: 456\n"
         "#+TITLE: Test\n#+ZHIHU_QUESTION_ID: 123\n\
#+zhihu_question_id: 456\n"))
    (zhihu-test--with-temp-file
     ".org"
     source
     (lambda (file)
       (should-error (zhihu--org-read-meta file))))))

(ert-deftest zhihu-org-banner-is-generic-only-and-strict ()
  (zhihu-test--with-temp-file
   ".org"
   (concat
    "#+TITLE: Test\n"
    "#+BANNER: ./canonical.jpg\n"
    "#+ZHIHU_QUESTION_ID: 123\n"
    "#+ZHIHU_BANNER: ./ignored.jpg\n")
   (lambda (file)
     (let ((meta (zhihu--org-read-meta file)))
       (should (eq (plist-get meta :kind) 'answer))
       (should (equal (plist-get meta :banner) "./canonical.jpg")))))
  (zhihu-test--with-temp-file
   ".org"
   "#+TITLE: Test\n#+ZHIHU_BANNER: ./ignored.jpg\n#+ZHIHU_ARTICLE_ID:\n"
   (lambda (file)
     (should-not (plist-get (zhihu--org-read-meta file) :banner))))
  (dolist
      (source
       '("#+TITLE: Test\n#+ZHIHU_ARTICLE_ID:\n#+BANNER:\n"
         "#+TITLE: Test\n#+ZHIHU_ARTICLE_ID:\n\
#+BANNER: one\n#+banner: two\n"))
    (zhihu-test--with-temp-file
     ".org" source
     (lambda (file)
       (should-error (zhihu--org-read-meta file))))))

(ert-deftest zhihu-org-native-toc-is-strict-and-channel-independent ()
  (dolist (value '("headlines" "headlines 1" "HEADLINES 2" "headlines 3"))
    (zhihu-test--with-temp-file
     ".org"
     (format
      "#+TITLE: Test\n#+TOC: %s\n#+ZHIHU_ARTICLE_ID:\n"
      value)
     (lambda (file)
       (should (plist-get (zhihu--org-read-meta file) :toc)))))
  (zhihu-test--with-temp-file
   ".org"
   "#+TITLE: Test\n#+TOC: headlines 2\n#+ZHIHU_QUESTION_ID: 123\n"
   (lambda (file)
     (let ((meta (zhihu--org-read-meta file)))
       (should (eq (plist-get meta :kind) 'answer))
       (should (plist-get meta :toc)))))
  (dolist
      (source
       '("#+TITLE: Test\n#+TOC:\n#+ZHIHU_ARTICLE_ID:\n"
         "#+TITLE: Test\n#+TOC: headlines 0\n#+ZHIHU_ARTICLE_ID:\n"
         "#+TITLE: Test\n#+TOC: headlines 4\n#+ZHIHU_ARTICLE_ID:\n"
         "#+TITLE: Test\n#+TOC: contents\n#+ZHIHU_ARTICLE_ID:\n"
         "#+TITLE: Test\n#+TOC: headlines 1 2\n#+ZHIHU_ARTICLE_ID:\n"
         "#+TITLE: Test\n#+TOC: headlines 1\n\
#+toc: headlines 2\n#+ZHIHU_ARTICLE_ID:\n"))
    (zhihu-test--with-temp-file
     ".org" source
     (lambda (file)
       (should-error (zhihu--org-read-meta file))))))

(ert-deftest zhihu-org-ignores-keywords-in-source-blocks ()
  (zhihu-test--with-temp-file
   ".org"
   (concat
    "#+TITLE: Test\n"
    "#+ZHIHU_ARTICLE_ID:\n"
    "#+begin_src org\n"
    "#+BANNER:\n"
    "#+BANNER: ./inside-source-block.jpg\n"
    "#+TOC: headlines 2\n"
    "#+ZHIHU_QUESTION_ID:\n"
    "#+ZHIHU_QUESTION_ID: 123\n"
    "#+end_src\n")
   (lambda (file)
     (let ((meta (zhihu--org-read-meta file)))
       (should (eq (plist-get meta :kind) 'article))
       (should-not (plist-get meta :banner))
       (should-not (plist-get meta :toc))
       (should-not (plist-get meta :question-id))))))

(ert-deftest zhihu-org-publish-settings-round-trip ()
  (zhihu-test--with-temp-file
   ".org"
   (concat
    "#+TITLE: Test\n"
    "#+BANNER: ./images/cover.jpg\n"
    "#+TOC: headlines 2\n"
    "#+ZHIHU_ARTICLE_ID: 456\n"
    "#+ZHIHU_CREATION_STATEMENT: medical_advice\n"
    "#+ZHIHU_CONTENT_SOURCE: printMedia\n"
    "#+ZHIHU_REPRINT_PERMISSION: disallowed\n"
    "#+ZHIHU_COMMENT_PERMISSION: censor\n"
    "\n正文\n")
   (lambda (file)
     (let ((meta (zhihu--org-read-meta file)))
       (should
        (equal
         (mapcar (lambda (key) (plist-get meta key))
                 '(:banner :creation-statement :content-source
			   :toc
			   :reprint-permission :comment-permission))
         '("./images/cover.jpg" "medical_advice" "printMedia"
           t "disallowed" "censor")))
       (zhihu--org-write-zhihu-meta file meta)
       (let ((round-tripped (zhihu--org-read-meta file)))
         (should
          (equal
           (mapcar (lambda (key) (plist-get round-tripped key))
                   '(:banner :creation-statement :content-source
			     :toc
			     :reprint-permission :comment-permission))
           '("./images/cover.jpg" "medical_advice" "printMedia"
             t "disallowed" "censor"))))
       (dolist (key
                '(:creation-statement :content-source
				      :reprint-permission :comment-permission))
         (setq meta (plist-put meta key nil)))
       (zhihu--org-write-zhihu-meta file meta)
       (let ((source
              (with-temp-buffer
                (insert-file-contents file)
                (buffer-string))))
         (dolist
             (keyword
              '("ZHIHU_CREATION_STATEMENT"
                "ZHIHU_CONTENT_SOURCE"
                "ZHIHU_REPRINT_PERMISSION" "ZHIHU_COMMENT_PERMISSION"))
           (should-not (string-match-p keyword source)))
         (should
          (string-match-p
           (regexp-quote "#+TOC: headlines 2")
           source))
         (should-not (string-match-p "ZHIHU_.*TOC" source))
         (should
          (string-match-p
           (regexp-quote "#+BANNER: ./images/cover.jpg")
           source)))))))

(ert-deftest zhihu-org-article-topics-round-trip-and-remove ()
  (zhihu-test--with-temp-file
   ".org"
   (concat
    "#+TITLE: Test\n"
    "#+ZHIHU_ARTICLE_ID: 456\n"
    "#+ZHIHU_TOPICS: [\"123\",\"comma, quote\\\"\"]\n"
    "#+OTHER: keep\n\n正文\n")
   (lambda (file)
     (let ((meta (zhihu--org-read-meta file)))
       (should
        (equal (plist-get meta :topics)
               '("123" "comma, quote\"")))
       (zhihu--org-write-zhihu-meta file meta)
       (should
        (equal (plist-get (zhihu--org-read-meta file) :topics)
               '("123" "comma, quote\"")))
       (setq meta (plist-put meta :topics nil))
       (zhihu--org-write-zhihu-meta file meta)
       (let ((source
              (with-temp-buffer
                (insert-file-contents file)
                (buffer-string))))
         (should-not (string-match-p "ZHIHU_TOPICS" source))
         (should
          (string-match-p (regexp-quote "#+OTHER: keep") source))
         (should (string-match-p "正文" source)))))))

(ert-deftest zhihu-org-rejects-invalid-article-topics ()
  (dolist
      (raw
       '("[]"
         "null"
         "\"Emacs\""
         "{\"name\":\"Emacs\"}"
         "[\"Emacs\",42]"
         "["))
    (zhihu-test--with-temp-file
     ".org"
     (format
      "#+TITLE: Test\n#+ZHIHU_ARTICLE_ID:\n#+ZHIHU_TOPICS: %s\n"
      raw)
     (lambda (file)
       (should-error (zhihu--org-read-meta file)))))
  (zhihu-test--with-temp-file
   ".org"
   "#+ZHIHU_ARTICLE_ID:\n\
#+ZHIHU_TOPICS: [\"Emacs\"]\n#+zhihu_topics: [\"Org\"]\n"
   (lambda (file)
     (should-error (zhihu--org-read-meta file)))))

(ert-deftest zhihu-typst-root-resolves-all-absolute-imports-together ()
  (let* ((root (make-temp-file "zhihu-test-typst-root-" t))
         (project (expand-file-name "project" root))
         (source-dir (expand-file-name "src" project))
         (source (expand-file-name "main.typ" source-dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "assets" project) t)
          (make-directory (expand-file-name "assets" root) t)
          (make-directory (expand-file-name "shared" root) t)
          (make-directory source-dir t)
          (dolist (file
                   (list (expand-file-name "assets/a.typ" project)
                         (expand-file-name "assets/a.typ" root)
                         (expand-file-name "shared/b.typ" root)))
            (with-temp-file file))
          (with-temp-file source
            (insert "#import \"/assets/a.typ\"\n"
                    "#include \"/shared/b.typ\"\n"))
          (should (equal (zhihu--typst-root source)
                         (directory-file-name root))))
      (delete-directory root t))))

(ert-deftest zhihu-typst-root-ignores-imports-in-block-comments ()
  (let* ((root (make-temp-file "zhihu-test-typst-comments-" t))
         (source-dir (expand-file-name "src" root))
         (source (expand-file-name "main.typ" source-dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "assets" root) t)
          (make-directory source-dir t)
          (with-temp-file (expand-file-name "assets/a.typ" root))
          (with-temp-file source
            (insert
             "/*\n"
             "#import \"/missing-from-block-comment.typ\"\n"
             "*/\n"
             "#import \"/assets/a.typ\"\n"))
          (should (equal (zhihu--typst-root source)
                         (directory-file-name root))))
      (delete-directory root t))))

(ert-deftest zhihu-typst-cli-uses-native-target-without-input-shim ()
  (let (calls)
    (cl-letf
        (((symbol-function 'zhihu--typst-root)
          (lambda (_file) "/tmp/project"))
         ((symbol-function 'zhihu--shell-convert)
          (lambda (program args input)
            (push (list program args input) calls)
            (pcase (car args)
              ("eval"
               (concat
                "{\"zhihu-count\":0,\"zhihu-value\":null,"
                "\"banner-count\":0,\"banner-value\":null,"
                "\"toc-count\":0}"))
              ("compile" "<html><head></head><body></body></html>")
              (_ (ert-fail (format "Unexpected Typst args: %S" args)))))))
      (should
       (equal
        (zhihu--typst-query-metadata "/tmp/project/post.typ")
        '(:zhihu nil :banner nil :toc nil)))
      (should
       (equal
        (zhihu--typst-compile-html "/tmp/project/post.typ")
        "<html><head></head><body></body></html>")))
    (setq calls (nreverse calls))
    (should (= (length calls) 2))
    (dolist (call calls)
      (let ((program (nth 0 call))
            (args (nth 1 call)))
        (should (equal program "typst"))
        (should-not (member "--input" args))
        (should-not (member "target=html" args))))
    (let ((eval-args (nth 1 (nth 0 calls)))
          (compile-args (nth 1 (nth 1 calls))))
      (should (equal (cl-subseq eval-args 0 5)
                     '("eval" "--features" "html" "--target" "html")))
      (should
       (equal
        compile-args
        '("compile" "--features=html" "--root" "/tmp/project"
          "-f" "html" "-" "-"))))))

(ert-deftest zhihu-typst-compile-wrapper-preserves-source-path-and-root ()
  (skip-unless (executable-find "typst"))
  (let* ((root (make-temp-file "zhihu-test-typst-wrapper-" t))
         (source-dir (expand-file-name "src" root))
         (source (expand-file-name "main.typ" source-dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "assets" root) t)
          (make-directory source-dir t)
          (with-temp-file (expand-file-name "assets/absolute.typ" root)
            (insert "#let absolute = [Absolute import]\n"))
          (with-temp-file (expand-file-name "relative.typ" source-dir)
            (insert "#let relative = [Relative import]\n"))
          (with-temp-file source
            (insert
             "#import \"/assets/absolute.typ\": absolute\n"
             "#import \"relative.typ\": relative\n"
             "#set document(title: \"Wrapper test\")\n"
             "#outline()\n"
             "= Body\n"
             "#absolute\n"
             "#relative\n"))
          (let ((html (zhihu--typst-compile-html source)))
            (should (string-match-p "Absolute import" html))
            (should (string-match-p "Relative import" html))
            (should
             (string-match-p
              (regexp-quote "<title>Wrapper test</title>")
              html))
            (should-not (string-match-p ">Contents<" html))))
      (delete-directory root t))))

(ert-deftest zhihu-typst-query-distinguishes-missing-and-empty-metadata ()
  (skip-unless (executable-find "typst"))
  (zhihu-test--with-temp-file
   ".typ" "= Body\n"
   (lambda (file)
     (should
     (equal (zhihu--typst-query-metadata file)
             '(:zhihu nil :banner nil :toc nil)))))
  (dolist
      (source
       '("#metadata(none) <zhihu>\n"
         "#metadata((:)) <zhihu>\n"
         "#metadata((question-id: \"123\",)) <zhihu>\n\
#metadata((answer-id: \"456\",)) <zhihu>\n"))
    (zhihu-test--with-temp-file
     ".typ" source
     (lambda (file)
       (should-error (zhihu--typst-query-metadata file)))))
  (zhihu-test--with-temp-file
   ".typ" "#metadata((question-id: \"123\",)) <zhihu>\n"
   (lambda (file)
     (should
      (equal (zhihu--typst-query-metadata file)
             '(:zhihu (:question-id "123") :banner nil :toc nil))))))

(ert-deftest zhihu-typst-native-heading-outline-controls-toc ()
  (skip-unless (executable-find "typst"))
  (dolist
      (outline
       '("#outline()"
         "#outline(depth: 2)"
         "#outline(target: heading.where(level: 1))"))
    (zhihu-test--with-temp-file
     ".typ"
     (format
      "#metadata((article-id: none,)) <zhihu>\n%s\n= Body\n"
      outline)
     (lambda (file)
       (should (plist-get (zhihu--read-source-meta file) :toc)))))
  (zhihu-test--with-temp-file
   ".typ"
   (concat
    "#metadata((article-id: none,)) <zhihu>\n"
    "#outline(target: figure)\n"
   "= Body\n")
   (lambda (file)
     (should-not (plist-get (zhihu--read-source-meta file) :toc))))
  (dolist (field '("toc" "enable-table-of-contents"))
    (zhihu-test--with-temp-file
     ".typ"
     (format
      "#metadata((article-id: none, %s: true,)) <zhihu>\n= Body\n"
      field)
     (lambda (file)
       (should-error (zhihu--read-source-meta file))))))

(ert-deftest zhihu-typst-compile-hides-only-heading-outlines ()
  (skip-unless (executable-find "typst"))
  (zhihu-test--with-temp-file
   ".typ"
   (concat
    "#set document(title: \"Outline test\")\n"
    "#outline()\n"
    "#outline(target: heading.where(level: 1), title: [Top headings])\n"
    "#outline(target: figure, title: [Figures])\n"
    "= Body\n"
    "#figure(table(columns: 1, [Cell]), caption: [A table])\n")
   (lambda (file)
     (let ((html (zhihu--typst-compile-html file)))
       (should
        (string-match-p
         (regexp-quote "<title>Outline test</title>")
         html))
       (should-not (string-match-p ">Contents<" html))
       (should-not (string-match-p ">Top headings<" html))
       (should (string-match-p ">Figures<" html))
       (should
        (= (with-temp-buffer
             (insert html)
             (goto-char (point-min))
             (how-many "<nav role=\"doc-toc\">" (point-min) (point-max)))
           1))))))

(ert-deftest zhihu-typst-query-reads-only-canonical-banner-metadata ()
  (skip-unless (executable-find "typst"))
  (zhihu-test--with-temp-file
   ".typ"
   (concat
    "#metadata(\"./canonical.jpg\") <banner>\n"
    "#metadata((article-id: none, banner: \"./ignored.jpg\",)) <zhihu>\n"
    "= Banner <banner>\n")
   (lambda (file)
     (let ((meta (zhihu--read-source-meta file)))
       (should (equal (plist-get meta :banner) "./canonical.jpg")))))
  (zhihu-test--with-temp-file
   ".typ"
   "#metadata((article-id: none, banner: \"./ignored.jpg\",)) <zhihu>\n"
   (lambda (file)
     (should-not (plist-get (zhihu--read-source-meta file) :banner))))
  (dolist
      (source
       '("#metadata((article-id: none,)) <zhihu>\n\
#metadata(none) <banner>\n"
         "#metadata((article-id: none,)) <zhihu>\n\
#metadata(42) <banner>\n"
         "#metadata((article-id: none,)) <zhihu>\n\
#metadata(\"\") <banner>\n"
         "#metadata((article-id: none,)) <zhihu>\n\
#metadata(\"one\") <banner>\n#metadata(\"two\") <banner>\n"))
    (zhihu-test--with-temp-file
     ".typ" source
     (lambda (file)
       (should-error (zhihu--read-source-meta file))))))

(ert-deftest zhihu-typst-article-topics-round-trip ()
  (skip-unless (executable-find "typst"))
  (dolist
      (case
       '(("#metadata((article-id: none, topics: (\"Emacs\",),)) <zhihu>\n= Body\n"
          ("Emacs"))
         ("#metadata((article-id: none, topics: (\"123\", \"org-mode\",),)) <zhihu>\n= Body\n"
          ("123" "org-mode"))))
    (zhihu-test--with-temp-file
     ".typ"
     (car case)
     (lambda (file)
       (should
        (equal (plist-get (zhihu--read-source-meta file) :topics)
               (cadr case))))))
  (dolist
      (source
       '("#metadata((article-id: none, topics: (),)) <zhihu>\n= Body\n"
         "#metadata((article-id: none, topics: (\"Emacs\"),)) <zhihu>\n= Body\n"))
    (zhihu-test--with-temp-file
     ".typ"
     source
     (lambda (file)
       (should-error (zhihu--read-source-meta file))))))

(ert-deftest zhihu-typst-article-topics-formatter-keeps-array-comma ()
  (let* ((meta
          (zhihu--zhihu-meta-from-plist '(:article-id nil :topics ["Emacs"])))
         (source (zhihu--format-typst-zhihu-metadata meta)))
    (should
     (string-match-p
      (regexp-quote
       "topics: (\n    \"Emacs\",\n  ),")
      source))))

(ert-deftest zhihu-typst-metadata-writer-ignores-labels-in-comments-and-strings ()
  (zhihu-test--with-temp-file
   ".typ"
   (concat
    "// #metadata((question-id: \"comment\")) <zhihu>\n"
    "/* #metadata((question-id: \"block-comment\")) <zhihu> */\n"
    "#let example = \"#metadata((question-id: \\\"string\\\")) <zhihu>\"\n"
    "= 正文\n")
   (lambda (file)
     (with-temp-buffer
       (insert-file-contents file)
       (should-not (zhihu--typst-native-metadata-region)))
     (zhihu--typst-write-native-metadata
      file
      (list :kind 'answer
            :question-id "123"))
     (let ((source
            (with-temp-buffer
              (insert-file-contents file)
              (buffer-string))))
       (should (string-match-p
                (regexp-quote
                 "// #metadata((question-id: \"comment\")) <zhihu>")
                source))
       (should (string-match-p
                (regexp-quote
                 "#let example = \"#metadata((question-id: \\\"string\\\")) <zhihu>\"")
                source))
       (with-temp-buffer
         (insert source)
         (let ((region (zhihu--typst-native-metadata-region)))
           (should region)
           (should
            (string-match-p
             (regexp-quote "question-id: \"123\"")
             (buffer-substring-no-properties (car region) (cdr region))))))))))

(ert-deftest zhihu-typst-publish-settings-read-and-write ()
  (cl-letf (((symbol-function 'zhihu--typst-query-metadata)
             (lambda (_file)
               '(:zhihu
                 (:article-id "456"
			      :creation-statement "fictional_creation"
			      :content-source "newsReport"
			      :reprint-permission "allowed"
			      :comment-permission "nobody")
                 :banner "./images/cover.jpg"
                 :toc t))))
    (let ((meta (zhihu--read-source-meta "/tmp/article.typ")))
      (should
       (equal
        (mapcar (lambda (key) (plist-get meta key))
                '(:banner :creation-statement :content-source
			  :toc
			  :reprint-permission :comment-permission))
        '("./images/cover.jpg" "fictional_creation" "newsReport"
          t "allowed" "nobody")))))
  (let* ((meta
          (zhihu--source-meta-from-parts
           '(:article-id "456"
             :creation-statement "fictional_creation"
             :content-source "newsReport"
             :reprint-permission "allowed"
             :comment-permission "nobody")
           nil "./images/cover.jpg" t))
         (source (zhihu--format-typst-zhihu-metadata meta)))
    (dolist
        (line
         '("creation-statement: \"fictional_creation\","
           "content-source: \"newsReport\","
           "reprint-permission: \"allowed\","
           "comment-permission: \"nobody\","))
      (should (string-match-p (regexp-quote line) source)))
    (should-not (string-match-p "banner:" source))
    (should-not (string-match-p "toc:" source))
    (let ((new-source
           (zhihu--format-new-source-metadata 'typst meta)))
      (should
       (string-match-p
        (regexp-quote "#metadata(\"./images/cover.jpg\") <banner>")
        new-source))
      (should
       (string-match-p
        (regexp-quote "#outline()")
        new-source))
      (should-not (string-match-p "<toc>" new-source)))))

(ert-deftest zhihu-writable-columns-fetches-all-pages-and-deduplicates ()
  (let (requests)
    (cl-letf
        (((symbol-function 'zhihu--http-json)
          (lambda (method url &rest args)
            (push (list method url args) requests)
            (cond
             ((equal url zhihu--current-user-endpoint)
              '(:status 200
                :json (:url_token "member /中")
                :body "{}"))
             ((string-match-p "[?&]offset=0\\(?:&\\|\\'\\)" url)
              '(:status 200
                :json
                (:paging (:is_end :json-false :totals 4)
                 :data
                 [(:column
                   (:id "first"
                    :title "第一栏"
                    :articles_count 10
                    :voteup_count 20)
                   :contributions_count 3)
                  (:column
                   (:id "duplicate"
                    :title "重复栏（第一页）"
                    :articles_count 30
                    :voteup_count 40)
                   :contributions_count 5)])
                :body "{}"))
             ((string-match-p "[?&]offset=2\\(?:&\\|\\'\\)" url)
              '(:status 200
                :json
                (:paging (:is_end t :totals 4)
                 :data
                 [(:column
                   (:id "duplicate"
                    :title "重复栏（第二页）"
                    :articles_count 300
                    :voteup_count 400)
                   :contributions_count 50)
                  (:column
                   (:id "last"
                    :title "最后一栏"
                    :articles_count 50
                    :voteup_count 60)
                   :contributions_count 7)])
                :body "{}"))
             (t (ert-fail (format "unexpected request: %s" url)))))))
      (should
       (equal
        (zhihu--writable-columns)
        '((:id "first"
           :title "第一栏"
           :articles-count 10
           :voteup-count 20
           :contributions-count 3)
          (:id "duplicate"
           :title "重复栏（第一页）"
           :articles-count 30
           :voteup-count 40
           :contributions-count 5)
          (:id "last"
           :title "最后一栏"
           :articles-count 50
           :voteup-count 60
           :contributions-count 7)))))
    (setq requests (nreverse requests))
    (should (= (length requests) 3))
    (should
     (equal (car requests)
            (list "GET" zhihu--current-user-endpoint nil)))
    (pcase-let
        ((`((,first-method ,first-url ,first-args)
           (,second-method ,second-url ,second-args))
          (cdr requests)))
      (dolist (method (list first-method second-method))
        (should (equal method "GET")))
      (dolist (args (list first-args second-args))
        ;; 省略 `:sign-json'，以保留请求层默认的 Cookie/ZSE 行为。
        (should-not args))
      (dolist (url (list first-url second-url))
        (should
         (string-prefix-p
          (concat
           "https://www.zhihu.com/api/v4/members/"
           (url-hexify-string "member /中")
           "/column-contributions?")
          url))
        (should (string-match-p "[?&]limit=50\\(?:&\\|\\'\\)" url))
        (should
         (string-match-p
          (concat
           "[?&]include="
           (regexp-quote
            (url-hexify-string zhihu--writable-columns-include))
           "\\(?:&\\|\\'\\)")
          url)))
      (should (string-match-p "[?&]offset=0\\(?:&\\|\\'\\)" first-url))
      (should (string-match-p "[?&]offset=2\\(?:&\\|\\'\\)" second-url)))))

(ert-deftest zhihu-writable-columns-accepts-an-empty-finished-list ()
  (let ((calls 0))
    (cl-letf
        (((symbol-function 'zhihu--http-json)
          (lambda (_method url &rest _args)
            (cl-incf calls)
            (if (equal url zhihu--current-user-endpoint)
                '(:status 200 :json (:url_token "member") :body "{}")
              '(:status 200
                :json
                (:paging (:is_end t :totals 0)
                 :data [])
                :body "{}")))))
      (should-not (zhihu--writable-columns)))
    (should (= calls 2))))

(ert-deftest zhihu-writable-columns-reports-login-errors-at-either-endpoint ()
  (dolist (failing-request '(me columns))
    (let ((calls 0))
      (cl-letf
          (((symbol-function 'zhihu--http-json)
            (lambda (_method url &rest _args)
              (cl-incf calls)
              (cond
               ((and (eq failing-request 'me)
                     (equal url zhihu--current-user-endpoint))
                '(:status 401
                  :json (:error (:message "need login"))
                  :body "{}"))
               ((equal url zhihu--current-user-endpoint)
                '(:status 200 :json (:url_token "member") :body "{}"))
               (t
                '(:status 403
                  :json (:error (:message "verification required"))
                  :body "{}"))))))
        (let ((err
               (should-error
                (zhihu--writable-columns)
                :type 'user-error)))
          (should
           (string-match-p
            "登录状态不可用.*登录并完成验证"
            (error-message-string err)))))
      (should (= calls (if (eq failing-request 'me) 1 2))))))

(ert-deftest zhihu-writable-columns-rejects-malformed-account-and-pages ()
  (dolist
      (json
       '(nil
         (:name "missing token")
         (:url_token nil)
         (:url_token "")
         (:url_token " \t ")
         (:url_token "bad\nmember")
         (:url_token 42)))
    (cl-letf
        (((symbol-function 'zhihu--http-json)
          (lambda (&rest _args)
            (list :status 200 :json json :body "{}"))))
      (should-error (zhihu--current-user-url-token))))
  (dolist
      (json
       '(nil
         (:paging (:is_end t :totals 0))
         (:data [])
         (:paging (:is_end t :totals 0) :data nil)
         (:paging (:totals 0) :data [])
         (:paging (:is_end nil :totals 0) :data [])
         (:paging (:is_end :json-null :totals 0) :data [])
         (:paging (:is_end t :totals "0") :data [])
         (:paging (:is_end t :totals -1) :data [])
         (:paging (:is_end t :totals 1)
          :data [(:contributions_count 1)])
         (:paging (:is_end t :totals 1)
          :data
          [(:column
            (:id "" :title "标题"
             :articles_count 1 :voteup_count 2)
            :contributions_count 3)])
         (:paging (:is_end t :totals 1)
          :data
          [(:column
            (:id "column" :title ""
             :articles_count 1 :voteup_count 2)
            :contributions_count 3)])
         (:paging (:is_end t :totals 1)
          :data
          [(:column
            (:id "column" :title "标题"
             :articles_count nil :voteup_count 2)
            :contributions_count 3)])
         (:paging (:is_end t :totals 1)
          :data
          [(:column
            (:id "column" :title "标题"
             :articles_count 1 :voteup_count -1)
            :contributions_count 3)])
         (:paging (:is_end t :totals 1)
          :data
          [(:column
            (:id "column" :title "标题"
             :articles_count 1 :voteup_count 2)
            :contributions_count "3")])))
    (cl-letf
        (((symbol-function 'zhihu--http-json)
          (lambda (&rest _args)
            (list :status 200 :json json :body "{}"))))
      (should-error (zhihu--writable-columns-page "member" 0)))))

(ert-deftest zhihu-writable-columns-rejects-pagination-without-progress ()
  (dolist
      (page
       '((:records nil :count 0 :is-end nil :totals 1)
         (:records
          ((:id "only" :title "Only"
            :articles-count 0 :voteup-count 0 :contributions-count 0))
          :count 1 :is-end nil :totals 1)))
    (cl-letf
        (((symbol-function 'zhihu--current-user-url-token)
          (lambda () "member"))
         ((symbol-function 'zhihu--writable-columns-page)
          (lambda (_url-token _offset) page)))
      (should-error (zhihu--writable-columns)))))

(ert-deftest zhihu-create-column-sends-current-web-request-contract ()
  (let (request ensured-state)
    (cl-letf
        (((symbol-function 'zhihu--ensure-xsrf-token)
          (lambda (state)
            (setq ensured-state state)
            "xsrf-token"))
         ((symbol-function 'zhihu--http-json)
          (lambda (method url &rest args)
            (setq request (list method url args))
            '(:status 200
              :json
              (:payload
               (:manualCensor :json-false :id "emacs-writers"))
              :body "{}"))))
      (should
       (equal
        (zhihu--create-column "  Emacs 写作  " "一份普通简介")
        "emacs-writers")))
    (pcase-let ((`(,method ,url ,args) request))
      (should (equal method "POST"))
      (should (equal url zhihu--column-request-endpoint))
      (should
       (equal
        (plist-get args :body)
        '(:title "Emacs 写作"
          :intro "一份普通简介"
          :intro_type "plain")))
      (should-not (plist-member (plist-get args :body) :images))
      (should
       (equal
        (plist-get args :extra-headers)
        `(("x-xsrftoken" . "xsrf-token")
          ("Origin" . "https://www.zhihu.com")
          ("Referer" . ,zhihu--column-request-referer))))
      (should (eq (plist-get args :xsrf-state) ensured-state))
      ;; 省略该参数才能使用 `zhihu--http-json' 默认开启的 ZSE 签名。
      (should-not (plist-member args :sign-json)))
    (should (zhihu--xsrf-state-p ensured-state))))

(ert-deftest zhihu-create-column-understands-review-and-both-field-spellings ()
  (dolist
      (case
       '(((:payload (:manualCensor t)) pending)
         ((:payload (:manual_censor :json-false :id "writers"))
          "writers")
         ((:payload (:manual_censor :json-false :id 42)) "42")
         ((:payload
           (:manualCensor :json-false
            :manual_censor :json-false
            :id "  same-result  "))
          "same-result")))
    (pcase-let ((`(,json ,expected) case))
      (cl-letf
          (((symbol-function 'zhihu--ensure-xsrf-token)
            (lambda (_state) "token"))
           ((symbol-function 'zhihu--http-json)
            (lambda (&rest _args)
              (list :status 201 :json json :body "{}"))))
        (should
         (equal (zhihu--create-column "专栏" "") expected))))))

(ert-deftest zhihu-create-column-rejects-malformed-success-as-unknown ()
  (dolist
      (json
       '(nil
         (:payload nil)
         (:payload (:id "missing-manual-censor"))
         (:payload (:manualCensor nil :id "lisp-nil-is-not-json-false"))
         (:payload (:manual_censor nil :id "lisp-nil-is-not-json-false"))
         (:payload (:manualCensor :json-null :id "bad-boolean"))
         (:payload (:manualCensor :json-false))
         (:payload (:manualCensor :json-false :id " \t "))
         (:payload (:manualCensor :json-false :id "bad\nid"))
         (:payload
          (:manualCensor t :manual_censor :json-false :id "conflict"))))
    (cl-letf
        (((symbol-function 'zhihu--ensure-xsrf-token)
          (lambda (_state) "token"))
         ((symbol-function 'zhihu--http-json)
          (lambda (&rest _args)
            (list :status 200 :json json :body "{}"))))
      (let ((err
             (should-error
              (zhihu--create-column "专栏" "")
              :type 'zhihu-create-result-unknown)))
        (should
         (string-match-p
          "可能已经提交.*勿直接重试"
          (error-message-string err)))))))

(ert-deftest zhihu-create-column-distinguishes-definite-http-rejections ()
  (dolist (status '(400 409 422))
    (cl-letf
        (((symbol-function 'zhihu--ensure-xsrf-token)
          (lambda (_state) "token"))
         ((symbol-function 'zhihu--http-json)
          (lambda (&rest _args)
            (list :status status
                  :json '(:message "server rejected")
                  :body ""))))
      (let ((err
             (should-error
              (zhihu--create-column "专栏")
              :type 'user-error)))
        (should (string-match-p "server rejected"
                                (error-message-string err))))))
  (dolist (status '(401 403))
    (cl-letf
        (((symbol-function 'zhihu--ensure-xsrf-token)
          (lambda (_state) "token"))
         ((symbol-function 'zhihu--http-json)
          (lambda (&rest _args)
            (list :status status
                  :json '(:message "verification required")
                  :body ""))))
      (let ((err
             (should-error
              (zhihu--create-column "专栏")
              :type 'user-error)))
        (should
         (string-match-p
          "登录并完成知乎验证"
          (error-message-string err)))))))

(ert-deftest zhihu-create-column-treats-ambiguous-http-as-unknown ()
  (dolist (status '(302 408 500 503 nil))
    (cl-letf
        (((symbol-function 'zhihu--ensure-xsrf-token)
          (lambda (_state) "token"))
         ((symbol-function 'zhihu--http-json)
          (lambda (&rest _args)
            (list :status status :json '(:message "uncertain") :body ""))))
      (should-error
       (zhihu--create-column "专栏")
       :type 'zhihu-create-result-unknown))))

(ert-deftest zhihu-create-column-wraps-only-post-transport-interruptions ()
  (dolist (condition '(plz-error quit))
    (cl-letf
        (((symbol-function 'zhihu--ensure-xsrf-token)
          (lambda (_state) "token"))
         ((symbol-function 'zhihu--http-json)
          (lambda (&rest _args)
            (signal condition '("interrupted")))))
      (should-error
       (zhihu--create-column "专栏")
       :type 'zhihu-create-result-unknown)))
  (cl-letf
      (((symbol-function 'zhihu--ensure-xsrf-token)
        (lambda (_state) "token"))
       ((symbol-function 'zhihu--http-json)
        (lambda (&rest _args)
          (signal 'wrong-type-argument '(stringp 42)))))
    (should-error
     (zhihu--create-column "专栏")
     :type 'wrong-type-argument))
  ;; XSRF bootstrap 失败发生在非幂等 POST 之前，不能谎报“可能已提交”。
  (cl-letf
      (((symbol-function 'zhihu--ensure-xsrf-token)
        (lambda (_state)
          (signal 'plz-error '("bootstrap failed"))))
       ((symbol-function 'zhihu--http-json)
        (lambda (&rest _args)
          (ert-fail "POST must not run after a failed bootstrap"))))
    (should-error
     (zhihu--create-column "专栏")
     :type 'plz-error)))

(ert-deftest zhihu-create-column-validates-current-form-limits ()
  (should
   (equal
    (zhihu--normalize-new-column-title "  专栏名称  ")
    "专栏名称"))
  (should
   (equal
    (zhihu--normalize-new-column-title (make-string 20 ?栏))
    (make-string 20 ?栏)))
  (should-error
   (zhihu--normalize-new-column-title (make-string 21 ?栏))
   :type 'user-error)
  (dolist
      (title
       (list
        nil 42 "" " \n\t " "标题\n换行" "标题\t制表"
        (string #x00a0) (string #x3000) (string #xfeff)))
    (should-error
     (zhihu--normalize-new-column-title title)
     :type 'user-error))
  (dolist (codepoint '(#x1f300 #x1f64f))
    (should-error
     (zhihu--normalize-new-column-title
      (concat "专栏" (string codepoint)))
     :type 'user-error))
  (should
   (equal
    (zhihu--normalize-new-column-title
     (concat "专栏" (string #x1f650)))
    (concat "专栏" (string #x1f650))))
  (should (equal (zhihu--normalize-new-column-intro nil) ""))
  (should (equal (zhihu--normalize-new-column-intro "") ""))
  (should
   (equal
    (zhihu--normalize-new-column-intro "第一行\n第二行")
    "第一行\n第二行"))
  (should
   (equal
    (zhihu--normalize-new-column-intro " 保留两侧空格 ")
    " 保留两侧空格 "))
  (should
   (equal
    (zhihu--normalize-new-column-intro (make-string 1000 ?介))
    (make-string 1000 ?介)))
  (dolist (intro (list 42 " \n\t "
                       (string #x00a0) (string #x3000) (string #xfeff)
                       (make-string 1001 ?介)
                       (concat "简介" (string #x1f300))))
    (should-error
     (zhihu--normalize-new-column-intro intro)
     :type 'user-error)))

(ert-deftest zhihu-new-column-reports-and-returns-server-outcome ()
  (should (commandp 'zhihu-new-column))
  (let (messages)
    (cl-letf
        (((symbol-function 'zhihu--create-column)
          (lambda (title intro)
            (if (equal title "待审")
                (progn
                  (should (equal intro "介绍"))
                  'pending)
              (should (equal title "即时"))
              (should-not intro)
              "writers")))
         ((symbol-function 'message)
          (lambda (format-string &rest args)
            (push (apply #'format format-string args) messages))))
      (should (eq (zhihu-new-column "待审" "介绍") 'pending))
      (should (equal (zhihu-new-column "即时") "writers")))
    (should
     (equal
      (nreverse messages)
      '("zhihu: 专栏申请已提交，正在等待知乎人工审核"
        "zhihu: 已创建专栏 writers")))))

(ert-deftest zhihu-article-topic-search-validates-and-labels-candidates ()
  (let (request)
    (cl-letf
        (((symbol-function 'zhihu--http)
          (lambda (method url &rest args)
            (setq request (list method url args))
            `(:status 200
		      :body
		      ,(json-serialize
			[(:id "19572409"
			      :name "Emacs"
			      :type "topic"
			      :url "https://www.zhihu.com/topic/19572409"
			      :introduction "Text\neditor")])))))
      (let* ((records (zhihu--search-article-topics "人工 AI"))
             (record (car records)))
        (should (= (length records) 1))
        (should (equal (plist-get record :id) "19572409"))
        (should (equal (plist-get record :name) "Emacs"))
        (should
         (equal
          (zhihu--article-topic-candidate-label record)
          "Emacs — Text editor (19572409)"))))
    (should (equal (car request) "GET"))
    (should
     (string-match-p
      (regexp-quote
       (concat "token=" (url-hexify-string "人工 AI")
               "&max_matches=5&use_similar=0&topic_filter=1"))
      (cadr request)))
    (should-not (caddr request))))

(ert-deftest zhihu-article-topic-search-rejects-invalid-responses ()
  (dolist
      (response
       '((:status 503 :body "busy")
         (:status 200 :body "not-json")
         (:status 200 :body "{\"id\":\"1\"}")))
    (cl-letf (((symbol-function 'zhihu--http)
               (lambda (&rest _args) response)))
      (should-error (zhihu--search-article-topics "X"))))
  (should-error (zhihu--search-article-topics "  ")
                :type 'user-error))

(ert-deftest zhihu-get-article-topics-validates-draft-response ()
  (let ((state (zhihu--make-xsrf-state "token"))
        request)
    (cl-letf
        (((symbol-function 'zhihu--http-json)
          (lambda (&rest args)
            (setq request args)
            '(:status 200
		      :json
		      (:topics
		       [(:id "1" :name "Emacs")
			(:id 2 :name "org-mode")])
		      :body "{}"))))
      (should
       (equal
        (mapcar
         (lambda (record)
           (list (plist-get record :id)
                 (plist-get record :name)))
         (zhihu--get-article-topics state "456"))
        '(("1" "Emacs") ("2" "org-mode")))))
    (should (equal (car request) "GET"))
    (should
     (equal
      (cadr request)
      "https://zhuanlan.zhihu.com/api/articles/456/draft"))
    (should-not (plist-get (cddr request) :sign-json))
    (should (eq (plist-get (cddr request) :xsrf-state) state)))
  (dolist
      (json
       '((:title "draft")
         (:topics :json-null)))
    (cl-letf (((symbol-function 'zhihu--http-json)
               (lambda (&rest _args)
                 (list :status 200 :json json :body "{}"))))
      (should-error
       (zhihu--get-article-topics
        (zhihu--make-xsrf-state "token")
        "456")))))

(ert-deftest zhihu-sync-article-topics-replaces-in-delete-first-order ()
  (let* ((state (zhihu--make-xsrf-state "token"))
         (a (zhihu-test--topic-record "1" "A"))
         (b (zhihu-test--topic-record "2" "B"))
         (old (zhihu-test--topic-record "3" "Old"))
         (reads 0)
         events)
    (cl-letf
        (((symbol-function 'zhihu--get-article-topics)
          (lambda (actual-state article-id)
            (should (eq actual-state state))
            (should (equal article-id "456"))
            (cl-incf reads)
            (if (= reads 1)
                (list a old)
              (list b a))))
         ((symbol-function 'zhihu--resolve-article-topic)
          (lambda (name)
            (push (list 'resolve name) events)
            (should (equal name "B"))
            b))
         ((symbol-function 'zhihu--mutate-article-topic)
          (lambda (_state article-id method suffix body _action)
            (should (equal article-id "456"))
            (push (list method suffix body) events)
            '(:status 200))))
      (should
       (equal
        (zhihu--sync-article-topics state "456" '("B" "A"))
        '("B" "A"))))
    (should (= reads 2))
    (setq events (nreverse events))
    (should (equal (car events) '(resolve "B")))
    (should (equal (caadr events) "DELETE"))
    (should (equal (cadadr events) "/3"))
    (should (equal (car (nth 2 events)) "POST"))
    (should (equal (nth 2 (nth 2 events))
                   (plist-get b :object)))))

(ert-deftest zhihu-sync-article-topics-clears-remote-when-local-is-empty ()
  (let ((state (zhihu--make-xsrf-state "token"))
        (remote
         (list (zhihu-test--topic-record "1" "A")
               (zhihu-test--topic-record "2" "B")))
        (reads 0)
        methods)
    (cl-letf
        (((symbol-function 'zhihu--get-article-topics)
          (lambda (&rest _args)
            (cl-incf reads)
            (if (= reads 1) remote nil)))
         ((symbol-function 'zhihu--resolve-article-topic)
          (lambda (&rest _args)
            (ert-fail "Empty local topics must not resolve candidates")))
         ((symbol-function 'zhihu--ensure-xsrf-token)
          (lambda (_state) "token"))
         ((symbol-function 'zhihu--mutate-article-topic)
          (lambda (_state _article-id method _suffix _body _action)
            (push method methods)
            '(:status 204))))
      (should-not (zhihu--sync-article-topics state "456" nil)))
    (should (= reads 2))
    (should (equal methods '("DELETE" "DELETE")))))

(ert-deftest zhihu-sync-article-topics-noops-for-equal-unordered-set ()
  (let ((state (zhihu--make-xsrf-state "token"))
        (reads 0))
    (cl-letf
        (((symbol-function 'zhihu--get-article-topics)
          (lambda (&rest _args)
            (cl-incf reads)
            (list (zhihu-test--topic-record "2" "B")
                  (zhihu-test--topic-record "1" "A"))))
         ((symbol-function 'zhihu--resolve-article-topic)
          (lambda (&rest _args)
            (ert-fail "Existing topics must not be resolved again")))
         ((symbol-function 'zhihu--ensure-xsrf-token)
          (lambda (&rest _args)
            (ert-fail "No-op sync must not ensure XSRF")))
         ((symbol-function 'zhihu--mutate-article-topic)
          (lambda (&rest _args)
            (ert-fail "No-op sync must not mutate"))))
      (should
       (equal
        (zhihu--sync-article-topics state "456" '("A" "B"))
        '("A" "B"))))
    (should (= reads 1))))

(ert-deftest zhihu-sync-article-topics-resolves-before-mutating ()
  (let ((state (zhihu--make-xsrf-state "token"))
        mutated)
    (cl-letf
        (((symbol-function 'zhihu--get-article-topics)
          (lambda (&rest _args)
            (list (zhihu-test--topic-record "1" "Old"))))
         ((symbol-function 'zhihu--resolve-article-topic)
          (lambda (_name) (error "no exact topic")))
         ((symbol-function 'zhihu--ensure-xsrf-token)
          (lambda (&rest _args)
            (setq mutated t)))
         ((symbol-function 'zhihu--mutate-article-topic)
          (lambda (&rest _args)
            (setq mutated t))))
      (should-error
       (zhihu--sync-article-topics state "456" '("Missing"))))
    (should-not mutated)))

(ert-deftest zhihu-sync-article-topics-verifies-final-state ()
  (let ((state (zhihu--make-xsrf-state "token"))
        (target (zhihu-test--topic-record "2" "Target"))
        (reads 0))
    (cl-letf
        (((symbol-function 'zhihu--get-article-topics)
          (lambda (&rest _args)
            (cl-incf reads)
            nil))
         ((symbol-function 'zhihu--resolve-article-topic)
          (lambda (_name) target))
         ((symbol-function 'zhihu--ensure-xsrf-token)
          (lambda (_state) "token"))
         ((symbol-function 'zhihu--mutate-article-topic)
          (lambda (&rest _args) '(:status 200))))
      (should-error
       (zhihu--sync-article-topics state "456" '("Target"))))
    (should (= reads 2))))

(ert-deftest zhihu-article-topic-mutations-use-xsrf-and-full-object ()
  (let ((state (zhihu--make-xsrf-state "token"))
        requests)
    (cl-letf
        (((symbol-function 'zhihu--http-json)
          (lambda (&rest args)
            (push args requests)
            '(:status 204 :json nil :body ""))))
      (zhihu--mutate-article-topic
       state "456" "DELETE" "/1" nil "解绑")
      (zhihu--mutate-article-topic
       state "456" "POST" ""
       '(:id "2" :name "B" :type "topic")
       "绑定"))
    (setq requests (nreverse requests))
    (should (equal (caar requests) "DELETE"))
    (should
     (equal (cadar requests)
            "https://zhuanlan.zhihu.com/api/articles/456/topics/1"))
    (should-not (plist-get (cddar requests) :body))
    (should (equal (caadr requests) "POST"))
    (should
     (equal
      (plist-get (cddr (cadr requests)) :body)
      '(:id "2" :name "B" :type "topic")))
    (dolist (request requests)
      (let ((args (cddr request)))
        (should-not (plist-get args :sign-json))
        (should (eq (plist-get args :xsrf-state) state))
        (should
         (equal
          (cdr
           (assoc-string
            "x-xsrftoken"
            (plist-get args :extra-headers)
            t))
          "token"))))))

(ert-deftest zhihu-article-topic-mutations-reject-errors ()
  (cl-letf
      (((symbol-function 'zhihu--http-json)
        (lambda (&rest _args)
          '(:status 409 :json (:message "conflict") :body ""))))
    (should-error
     (zhihu--mutate-article-topic
      (zhihu--make-xsrf-state "token")
      "456" "POST" ""
      '(:id "1" :name "A" :type "topic")
      "绑定"))))

(ert-deftest zhihu-article-mutations-share-zhuanlan-request-helper ()
  (let ((state (zhihu--make-xsrf-state "token"))
        requests)
    (cl-letf
        (((symbol-function 'zhihu--zhuanlan-mutation-request)
          (lambda (&rest args)
            (push args requests)
            (if (string-suffix-p "/drafts" (nth 2 args))
                '(:status 200 :json (:id 789) :body "")
              '(:status 204 :json nil :body ""))))
         ((symbol-function 'zhihu--http-json)
          (lambda (&rest _args)
            (ert-fail "Business mutations must use the shared helper"))))
      (should
       (equal
        (zhihu--create-article-draft state "Title" "<p>Body</p>")
        "789"))
      (zhihu--mutate-article-topic
       state "456" "POST" ""
       '(:id "2" :name "B" :type "topic")
       "绑定")
      (zhihu--add-article-to-column state "writers" "456"))
    (setq requests (nreverse requests))
    (should (= (length requests) 3))
    (dolist (request requests)
      (should (eq (car request) state)))
    (pcase-let
        ((`((,_ "POST"
		"https://zhuanlan.zhihu.com/api/articles/drafts"
		"https://zhuanlan.zhihu.com/write"
		:body ,draft-body)
            (,_ "POST"
		"https://zhuanlan.zhihu.com/api/articles/456/topics"
		"https://zhuanlan.zhihu.com/p/456/edit"
		:body ,topic-body)
            (,_ "POST"
		"https://www.zhihu.com/api/v4/columns/writers/items"
		"https://zhuanlan.zhihu.com/p/456/edit"
		:body ,column-body))
          requests))
      (should (equal (plist-get draft-body :title) "Title"))
      (should (equal (plist-get draft-body :content) "<p>Body</p>"))
      (should
       (equal topic-body '(:id "2" :name "B" :type "topic")))
      (should
       (equal column-body '(:type "article" :id "456"))))))

(ert-deftest zhihu-http-recovers-non-2xx-plz-response ()
  (let ((state (zhihu--make-xsrf-state "current")))
    (cl-letf
        (((symbol-function 'zhihu--read-browser-cookies)
          (lambda (url)
            (should (equal url "https://www.zhihu.com/test"))
            '(("z_c0" . "login"))))
         ((symbol-function 'plz)
          (lambda (method _url &rest _args)
            (should (eq method 'get))
            (signal
             'plz-http-error
             (list
              "HTTP error"
              (make-plz-error
               :response
               (make-plz-response
                :status 503
                :headers
                '((set-cookie . "_xsrf=rotated; Path=/; Secure")
                  (zhi-request-id . "req-1"))
                :body "unavailable")))))))
      (let ((response
             (zhihu--http
              "GET" "https://www.zhihu.com/test"
              :xsrf-state state
              :with-zhihu-cookies t)))
        (should (= (plist-get response :status) 503))
        (should (equal (plist-get response :body) "unavailable"))
        (should
         (equal
          (plist-get response :headers)
          '((set-cookie . "_xsrf=rotated; Path=/; Secure")
            (zhi-request-id . "req-1"))))
        (should (equal (zhihu--response-request-id response) "req-1"))
        (should
         (equal (zhihu--xsrf-state-xsrf-token state) "rotated"))))))

(ert-deftest zhihu-http-reports-plz-transport-errors ()
  (cl-letf
      (((symbol-function 'plz)
        (lambda (&rest _args)
          (signal
           'plz-curl-error
           (list
            "Curl error"
            (make-plz-error
             :curl-error '(28 . "Operation timeout.")))))))
    (let ((data
           (condition-case err
               (progn
                 (zhihu--http "GET" "https://example.invalid/test")
                 nil)
             (plz-curl-error
              (cl-find-if #'plz-error-p (cdr err))))))
      (should (plz-error-p data))
      (should
       (equal (plz-error-curl-error data)
              '(28 . "Operation timeout."))))))

(ert-deftest zhihu-http-maps-patch-method-to-plz-symbol ()
  (let (sent-method)
    (cl-letf
        (((symbol-function 'plz)
          (lambda (method _url &rest _args)
            (setq sent-method method)
            (make-plz-response :status 204 :headers nil :body ""))))
      (should
       (= (plist-get
           (zhihu--http "PATCH" "https://example.invalid/test")
           :status)
          204)))
    (should (eq sent-method 'patch))))

(ert-deftest zhihu-cookie-header-replaces-only-xsrf ()
  (let ((cookies '(("z_c0" . "login")
                   ("_xsrf" . "stale-1")
                   ("theme" . "dark")
                   ("_xsrf" . "stale-2"))))
    (should
     (equal
      (zhihu--format-cookie-header cookies "fresh")
      "z_c0=login; theme=dark; _xsrf=fresh"))
    (should
     (equal
      (zhihu--format-cookie-header cookies nil)
      "z_c0=login; theme=dark"))))

(ert-deftest zhihu-cookie-browser-is-a-custom-choice ()
  (should (custom-variable-p 'zhihu-cookie-browser))
  (should (eq (default-value 'zhihu-cookie-browser) 'firefox))
  (should
   (equal
    (mapcar (lambda (choice) (car (last choice)))
            (cdr (get 'zhihu-cookie-browser 'custom-type)))
    '(firefox chromium chrome edge))))

(ert-deftest zhihu-cookie-profile-directory-selects-one-fixed-store ()
  (should (custom-variable-p 'zhihu-cookie-profile-directory))
  (should-not (default-value 'zhihu-cookie-profile-directory))
  (let ((profile (make-temp-file "zhihu-test-cookie-profile-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "Network" profile))
          (dolist
              (relative
               '("cookies.sqlite"
                 "Network/Cookies"))
            (with-temp-file (expand-file-name relative profile)))
          (let ((zhihu-cookie-profile-directory profile))
            (dolist
                (case
                 '((firefox . "cookies.sqlite")
                   (chromium . "Network/Cookies")
                   (chrome . "Network/Cookies")
                   (edge . "Network/Cookies")))
              (should
               (equal
                (zhihu--cookie-store-file (car case))
                (expand-file-name (cdr case) profile))))))
      (delete-directory profile t))))

(ert-deftest zhihu-cookie-profile-directory-must-be-explicit-and-valid ()
  (let ((zhihu-cookie-profile-directory nil))
    (should-error (zhihu--cookie-store-file 'firefox)))
  (let ((zhihu-cookie-profile-directory
         "/definitely/missing/zhihu-test-profile"))
    (should-error (zhihu--cookie-store-file 'firefox)))
  (let ((profile (make-temp-file "zhihu-test-empty-profile-" t)))
    (unwind-protect
        (let ((zhihu-cookie-profile-directory profile))
          (should-error (zhihu--cookie-store-file 'firefox))
          (should-error (zhihu--cookie-store-file 'chromium)))
      (delete-directory profile t)))
  (should-error (zhihu--cookie-store-file 'unknown)))

(ert-deftest zhihu-cookie-domain-and-path-matching-follow-request-url ()
  (should
   (zhihu--cookie-domain-matches-p
    ".zhihu.com" "zhuanlan.zhihu.com"))
  (should
   (zhihu--cookie-domain-matches-p ".zhihu.com" "zhihu.com"))
  (should-not
   (zhihu--cookie-domain-matches-p ".zhihu.com" "evilzhihu.com"))
  (should
   (zhihu--cookie-domain-matches-p
    "www.zhihu.com" "www.zhihu.com"))
  (should-not
   (zhihu--cookie-domain-matches-p
    "www.zhihu.com" "zhuanlan.zhihu.com"))
  (should (zhihu--cookie-path-matches-p "/api" "/api"))
  (should (zhihu--cookie-path-matches-p "/api" "/api/v4/me"))
  (should-not (zhihu--cookie-path-matches-p "/api" "/apiv4/me"))
  (should
   (equal
    (zhihu--cookie-url-parts
     "https://WWW.ZHIHU.COM/api/v4/me?q=/ignored#fragment")
    '("www.zhihu.com" "/api/v4/me" t))))

(ert-deftest zhihu-cookie-records-apply-secure-expiry-and-rfc-order ()
  (let* ((now (float-time))
         (records
          (list
           (zhihu--make-cookie-record
            :name "same" :value "root"
            :domain ".zhihu.com" :path "/"
            :expires (+ now 60) :secure nil :creation 1)
           (zhihu--make-cookie-record
            :name "same" :value "api"
            :domain ".zhihu.com" :path "/api"
            ;; nil expiry represents a session Cookie.
            :expires nil :secure t :creation 2)
           (zhihu--make-cookie-record
            :name "expired" :value "no"
            :domain ".zhihu.com" :path "/"
            :expires (- now 1) :secure nil :creation 3)
           (zhihu--make-cookie-record
            :name "other-host" :value "no"
            :domain "zhuanlan.zhihu.com" :path "/"
            :expires nil :secure nil :creation 4))))
    (should
     (equal
      (zhihu--cookie-records-for-url
       records "https://www.zhihu.com/api/v4/me?q=ignored")
      '(("same" . "api")
        ("same" . "root"))))
    (should
     (equal
      (zhihu--cookie-records-for-url
       records "http://www.zhihu.com/api/v4/me")
      '(("same" . "root"))))))

(ert-deftest zhihu-chromium-pbkdf2-matches-rfc-6070-vectors ()
  (should
   (equal
    (zhihu-test--hex
     (zhihu--pbkdf2-hmac-sha1
      (zhihu-test--utf8-bytes "password")
      (zhihu-test--utf8-bytes "salt")
      1 20))
    "0c60c80f961f0e71f3a9b524af6012062fe037a6"))
  (should
   (equal
    (zhihu-test--hex
     (zhihu--pbkdf2-hmac-sha1
      (zhihu-test--utf8-bytes "password")
      (zhihu-test--utf8-bytes "salt")
      2 20))
    "ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957"))
  (should
   (equal
    (zhihu-test--hex zhihu--chromium-linux-v10-key)
    "fd621fe5a2b402539dfa147ca9272778")))

(ert-deftest zhihu-chromium-v10-key-is-linux-only-and-fresh ()
  (let ((system-type 'gnu/linux))
    (let ((first (zhihu--chromium-v10-key))
          (second (zhihu--chromium-v10-key)))
      (should (equal first zhihu--chromium-linux-v10-key))
      (should-not (eq first second))
      (aset first 0 0)
      (should (= (aref second 0) #xfd))))
  (let ((system-type 'unsupported-system))
    (should-error (zhihu--chromium-v10-key) :type 'error)))

(ert-deftest zhihu-chromium-decrypts-v10-and-verifies-v24-domain ()
  (let* ((host ".zhihu.com")
         (key (copy-sequence zhihu--chromium-linux-v10-key))
         (domain-hash
          (secure-hash
           'sha256 (encode-coding-string host 'utf-8 t)
           nil nil t))
         (encrypted-v23
          (zhihu-test--chromium-encrypt
           "v10" key "cookie-value"))
         (encrypted-v24
          (zhihu-test--chromium-encrypt
           "v10" key (concat domain-hash "cookie-value")))
         (encrypted-v11
          (zhihu-test--chromium-encrypt
           "v11" key "cookie-value"))
         (keys `(("v10" . ,key)
                 ("v11" . ,key))))
    (should
     (equal
      (zhihu--chromium-decrypt-cookie
       encrypted-v23 host 23 keys)
      "cookie-value"))
    (should
     (equal
      (zhihu--chromium-decrypt-cookie
       encrypted-v24 host 24 keys)
      "cookie-value"))
    (should
     (equal
      (zhihu--chromium-decrypt-cookie
       encrypted-v11 host 23 keys)
      "cookie-value"))
    (should-error
     (zhihu--chromium-decrypt-cookie
      encrypted-v24 ".example.com" 24 keys)
     :type 'error)
    (should-error
     (zhihu--chromium-decrypt-cookie
      (concat "v12" (make-string 32 0))
      host 24 keys)
     :type 'error)))

(ert-deftest zhihu-chromium-rejects-invalid-pkcs7-padding ()
  (let* ((key (copy-sequence zhihu--chromium-linux-v10-key))
         (invalid-block (concat (make-string 15 ?x) (unibyte-string 0)))
         (result
          (gnutls-symmetric-encrypt
           'AES-128-CBC
           (copy-sequence key)
           zhihu--chromium-aes-cbc-iv
           invalid-block)))
    (should-error
     (zhihu--chromium-aes-cbc-decrypt key (car result))
     :type 'error)))

(ert-deftest zhihu-browser-cookie-dispatch-keeps-the-complete-url ()
  (let ((url "https://www.zhihu.com/api/v4/me?q=full")
        (store "/profile/cookie-store"))
    (cl-letf
        (((symbol-function 'zhihu--cookie-store-file)
          (lambda (browser)
            (if (memq browser '(firefox chromium))
                store
              (error "unsupported")))))
      (let ((zhihu-cookie-browser 'firefox))
        (cl-letf (((symbol-function 'zhihu--read-firefox-cookies)
                   (lambda (path request-url)
                     (should (equal path store))
                     (should (equal request-url url))
                     '(("source" . "firefox")))))
          (should
           (equal
            (zhihu--read-browser-cookies url)
            '(("source" . "firefox"))))))
      (let ((zhihu-cookie-browser 'chromium))
        (cl-letf (((symbol-function 'zhihu--read-chromium-cookies)
                   (lambda (path request-url browser)
                     (should (equal path store))
                     (should (equal request-url url))
                     (should (eq browser 'chromium))
                     '(("source" . "chromium")))))
          (should
           (equal
            (zhihu--read-browser-cookies url)
            '(("source" . "chromium"))))))
      (let ((zhihu-cookie-browser 'unknown))
        (should-error
         (zhihu--read-browser-cookies url)
         :type 'error)))))

(ert-deftest zhihu-chromium-sqlite-reader-matches-before-decrypting ()
  (let* ((directory
          (make-temp-file "zhihu-test-chromium-db-" t))
         (network (expand-file-name "Network" directory))
         (database (expand-file-name "Cookies" network))
         (key (copy-sequence zhihu--chromium-linux-v10-key))
         (now (float-time))
         db)
    (unwind-protect
        (progn
          (make-directory network)
          (setq db (sqlite-open database))
          (sqlite-execute
           db
           "CREATE TABLE meta (key TEXT PRIMARY KEY, value TEXT)")
          (sqlite-execute
           db
           (concat
            "CREATE TABLE cookies ("
            "creation_utc INTEGER, host_key TEXT, "
            "top_frame_site_key TEXT, name TEXT, value TEXT, "
            "encrypted_value BLOB, path TEXT, expires_utc INTEGER, "
            "is_secure INTEGER, has_expires INTEGER)"))
          (sqlite-execute db "INSERT INTO meta VALUES (?, ?)"
                          '("version" "24"))
          (cl-labels
              ((encrypted
                 (host value)
                 (zhihu-test--chromium-encrypt
                  "v10" key
                  (concat
                   (secure-hash
                    'sha256
                    (encode-coding-string host 'utf-8 t)
                    nil nil t)
                   value)))
               (insert-cookie
                 (creation host top-frame name value encrypted path
                           expires secure has-expires)
                 (sqlite-execute
                  db
                  (concat
                   "INSERT INTO cookies VALUES "
                   "(?, ?, ?, ?, ?, CAST(? AS BLOB), ?, ?, ?, ?)")
                  (list creation host top-frame name value encrypted path
			expires secure has-expires))))
            (insert-cookie
             2 ".zhihu.com" "" "same" ""
             (encrypted ".zhihu.com" "api")
             "/api" 0 1 0)
            (insert-cookie
             1 ".zhihu.com" "" "same" ""
             (encrypted ".zhihu.com" "root")
             "/"
             (truncate
              (+ zhihu--chromium-time-epoch-offset
                 (* (+ now 60) 1000000)))
             0 1)
            (insert-cookie
             3 "www.zhihu.com" "" "host-only" "plain" ""
             "/" 0 0 0)
            ;; These unsupported values do not match the request and must
            ;; never force a key lookup or abort the valid Cookie snapshot.
            (insert-cookie
             4 ".zhihu.com" "" "expired" ""
             (concat "v12" (make-string 32 0))
             "/api"
             (truncate
              (+ zhihu--chromium-time-epoch-offset
                 (* (- now 60) 1000000)))
             0 1)
            (insert-cookie
             5 ".zhihu.com" "" "wrong-path" ""
             (concat "v12" (make-string 32 0))
             "/other" 0 0 0)
            (insert-cookie
             6 ".zhihu.com" "https://example.com" "partitioned" ""
             (encrypted ".zhihu.com" "no")
             "/api" 0 1 0)
            ;; Chromium treats a non-empty plaintext value as authoritative.
            (insert-cookie
             7 ".zhihu.com" "" "plain-wins" "usable"
             (concat "v12" (make-string 32 0))
             "/api" 0 1 0))
          (sqlite-close db)
          (setq db nil)
          (let ((zhihu-cookie-browser 'chromium)
                (zhihu-cookie-profile-directory directory))
            (should
             (equal
              (zhihu--read-browser-cookies
               "https://www.zhihu.com/api/v4/me?q=ignored")
              '(("same" . "api")
                ("plain-wins" . "usable")
                ("same" . "root")
                ("host-only" . "plain"))))))
      (when db
        (ignore-errors (sqlite-close db)))
      (ignore-errors (delete-directory directory t)))))

(ert-deftest zhihu-chromium-secret-service-uses-v2-schema-and-collections ()
  (require 'secrets)
  (let ((secrets-enabled t)
        searches)
    (cl-letf
        (((symbol-function 'secrets-list-collections)
          (lambda () '("Login" "Work")))
         ((symbol-function 'secrets-search-item-paths)
          (lambda (collection &rest attributes)
            (push (cons collection attributes) searches)
            (and
             (equal collection "Login")
             (equal
              attributes
              '(:xdg:schema
                "chrome_libsecret_os_crypt_password_v2"
                :application "chromium"))
             '("/org/freedesktop/secrets/item/chromium"))))
         ((symbol-function 'secrets-get-secret)
          (lambda (collection item)
            (should (equal collection "Login"))
            (should
             (equal
              item
              "/org/freedesktop/secrets/item/chromium"))
            "safe-secret")))
      (should
       (equal
        (zhihu--chromium-secret-service-password
         (zhihu--chromium-browser-spec 'edge))
        (zhihu-test--utf8-bytes "safe-secret"))))
    (should
     (member
      '("Login"
        :xdg:schema "chrome_libsecret_os_crypt_password_v2"
        :application "chromium")
      searches))))

(ert-deftest zhihu-chromium-kwallet-uses-typed-read-and-always-closes ()
  (require 'dbus)
  (let ((zhihu--chromium-kwallet-endpoints
         '(("org.kde.kwalletd6" "/modules/kwalletd6")))
        calls
        closed)
    (cl-letf
        (((symbol-function 'dbus-call-method)
          (lambda (bus service path interface method &rest args)
            (push (cons method args) calls)
            (should (eq bus :session))
            (should (equal service "org.kde.kwalletd6"))
            (should (equal path "/modules/kwalletd6"))
            (should (equal interface "org.kde.KWallet"))
            (pcase method
              ("isEnabled" t)
              ("networkWallet" "wallet")
              ("open"
               (should
                (equal args '("wallet" :int64 0 "zhihu.el")))
               7)
              ("hasFolder"
               (should
                (equal
                 args
                 '(:int32 7 "Chromium Keys" "zhihu.el")))
               t)
              ("hasEntry"
               (should
                (equal
                 args
                 '(:int32 7 "Chromium Keys"
			  "Chromium Safe Storage" "zhihu.el")))
               t)
              ("readPassword"
               (should
                (equal
                 args
                 '(:int32 7 "Chromium Keys"
			  "Chromium Safe Storage" "zhihu.el")))
               "wallet-secret")
              ("close"
               (setq closed t)
               (should
                (equal args '(:int32 7 nil "zhihu.el")))
               0)
              (_ (ert-fail (format "Unexpected method %s" method)))))))
      (should
       (equal
        (zhihu--chromium-kwallet-password
         (zhihu--chromium-browser-spec 'edge))
        (zhihu-test--utf8-bytes "wallet-secret"))))
    (should closed)
    (should (assoc-string "readPassword" calls))
    (should (assoc-string "close" calls))))

(ert-deftest zhihu-cookie-database-readonly-uses-escaped-uri-and-transaction ()
  (let* ((directory
          (make-temp-file "zhihu-test-cookie-uri-#?-" t))
         (path (expand-file-name "Cookies" directory))
         statements
         closed)
    (unwind-protect
        (cl-letf
            (((symbol-function 'sqlite-open)
              (lambda (&rest args)
                (should-not args)
                'readonly-db))
             ((symbol-function 'sqlite-execute)
              (lambda (db statement &optional values)
                (should (eq db 'readonly-db))
                (push (list statement values) statements)))
             ((symbol-function 'sqlite-close)
              (lambda (db)
                (should (eq db 'readonly-db))
                (setq closed t)))
             ((symbol-function 'make-temp-file)
              (lambda (&rest _args)
                (ert-fail "Successful readonly query must not make a copy"))))
          (should
           (equal
            (zhihu--query-cookie-database
             path "test cookies"
             (lambda (db schema)
               (should (eq db 'readonly-db))
               (should (equal schema "cookies"))
               'readonly-result))
            'readonly-result))
          (setq statements (nreverse statements))
          (should
           (equal (mapcar #'car statements)
                  '("ATTACH DATABASE ? AS cookies"
                    "BEGIN"
                    "COMMIT")))
          (let ((uri (car (cadr (car statements)))))
            (should (string-match-p "%23%3F" uri))
            (should (string-suffix-p "?mode=ro&cache=private" uri)))
          (should closed))
      (ignore-errors (delete-directory directory t)))))

(ert-deftest zhihu-cookie-database-fallback-is-only-for-sqlite-errors ()
  (let ((path (make-temp-file "zhihu-test-cookie-database-"))
        snapshot-created)
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'sqlite-open)
                (lambda (&rest _args)
                  (error "key access denied")))
               ((symbol-function 'make-temp-file)
                (lambda (&rest _args)
                  (setq snapshot-created t)
                  (ert-fail "Ordinary errors must not trigger a copy"))))
            (should-error
             (zhihu--query-cookie-database
              path "test cookies" (lambda (&rest _args) nil)))
            (should-not snapshot-created))
          (let ((opens 0)
                schemas
                snapshot-directory
                (real-make-temp-file
                 (symbol-function 'make-temp-file)))
            (cl-letf
                (((symbol-function 'make-temp-file)
                  (lambda (&rest args)
                    (setq snapshot-directory
                          (apply real-make-temp-file args))))
                 ((symbol-function 'sqlite-open)
                  (lambda (&rest args)
                    (cl-incf opens)
                    (if args 'copy-db 'readonly-db)))
                 ((symbol-function 'sqlite-execute)
                  (lambda (db statement &optional _values)
                    (when (and (eq db 'readonly-db)
                               (string-prefix-p "ATTACH" statement))
                      (signal 'sqlite-error '("database is locked")))))
                 ((symbol-function 'sqlite-close) #'ignore))
              (should
               (equal
                (zhihu--query-cookie-database
                 path "test cookies"
                 (lambda (_db schema)
                   (push schema schemas)
                   'copied-result))
                'copied-result)))
            (should (= opens 2))
            (should (equal schemas '("main")))
            (should snapshot-directory)
            (should-not (file-exists-p snapshot-directory))))
      (ignore-errors (delete-file path)))))

(ert-deftest zhihu-cookie-database-reports-both-errors-and-cleans-up ()
  (let* ((source-directory
          (make-temp-file "zhihu-test-cookie-source-" t))
         (path (expand-file-name "Cookies" source-directory))
         snapshot-directory
         opened-snapshot
         closed
         rollbacks
         (real-make-temp-file (symbol-function 'make-temp-file)))
    (unwind-protect
        (progn
          (with-temp-file path (insert "database"))
          (with-temp-file (concat path "-wal") (insert "wal"))
          (cl-letf
              (((symbol-function 'make-temp-file)
                (lambda (&rest args)
                  (setq snapshot-directory
                        (apply real-make-temp-file args))))
               ((symbol-function 'sqlite-open)
                (lambda (&rest args)
                  (if args
                      (progn
                        (setq opened-snapshot (car args))
                        (should (file-exists-p opened-snapshot))
                        (should
                         (file-exists-p (concat opened-snapshot "-wal")))
                        'copy-db)
                    'readonly-db)))
               ((symbol-function 'sqlite-execute)
                (lambda (db statement &optional _values)
                  (when (equal statement "ROLLBACK")
                    (push db rollbacks))))
               ((symbol-function 'sqlite-close)
                (lambda (db)
                  (push db closed))))
            (let ((message
                   (condition-case err
                       (progn
                         (zhihu--query-cookie-database
                          path "test cookies"
                          (lambda (_db schema)
                            (if (equal schema "cookies")
                                (signal
                                 'sqlite-error
                                 '("database is locked"))
                              (error "snapshot is invalid"))))
                         nil)
                     (error (error-message-string err)))))
              (should message)
              (should
               (string-match-p
                (regexp-quote "database is locked") message))
              (should
               (string-match-p
                (regexp-quote "snapshot is invalid") message))))
          (should opened-snapshot)
          (should (equal closed '(copy-db readonly-db)))
          (should (equal rollbacks '(copy-db readonly-db)))
          (should snapshot-directory)
          (should-not (file-exists-p snapshot-directory)))
      (ignore-errors (delete-directory source-directory t)))))

(ert-deftest zhihu-firefox-and-chromium-share-cookie-database-lifecycle ()
  (zhihu-test--with-temp-file
   ".sqlite" ""
   (lambda (path)
     (let ((url "https://www.zhihu.com/")
           calls)
       (cl-letf
           (((symbol-function 'zhihu--query-cookie-database)
             (lambda (actual-path label query)
               (should (equal actual-path path))
               (push label calls)
               (funcall query 'test-db "test-schema")))
            ((symbol-function 'zhihu--select-firefox-cookies)
             (lambda (db table actual-url)
               (should (eq db 'test-db))
               (should (equal table "test-schema.moz_cookies"))
               (should (equal actual-url url))
               'firefox-result))
            ((symbol-function 'zhihu--select-chromium-cookies)
             (lambda (db schema actual-url spec)
               (should (eq db 'test-db))
               (should (equal schema "test-schema"))
               (should (equal actual-url url))
               (should
                (equal
                 spec
                 (zhihu--chromium-browser-spec 'chromium)))
               'chromium-result)))
         (should
          (eq (zhihu--read-firefox-cookies path url)
              'firefox-result))
         (should
          (eq
           (zhihu--read-chromium-cookies path url 'chromium)
           'chromium-result)))
       (should (= (length calls) 2))
       (should (member "Firefox cookies" calls))
       (should (member "chromium Cookie" calls))))))

(ert-deftest zhihu-xsrf-token-update-is-local-to-its-state ()
  (let ((first (zhihu--make-xsrf-state "old"))
        (second (zhihu--make-xsrf-state "other")))
    (should
     (equal
      (zhihu--xsrf-token-after-headers
       '((content-type . "text/html"))
       "old")
      "old"))
    (zhihu--update-xsrf-state
     first
     '((set-cookie . "_xsrf=new; Path=/; Secure")))
    (should (equal (zhihu--xsrf-state-xsrf-token first) "new"))
    (should (equal (zhihu--xsrf-state-xsrf-token second) "other"))
    (zhihu--update-xsrf-state
     first
     '((set-cookie . "_xsrf=; Path=/; Max-Age=0")))
    (should-not (zhihu--xsrf-state-xsrf-token first))))

(ert-deftest zhihu-ensure-xsrf-token-reuses-only-the-given-state ()
  (let ((requests 0))
    (cl-letf (((symbol-function 'zhihu--http)
               (lambda (method url &rest args)
                 (let ((state (plist-get args :xsrf-state)))
                   (should (zhihu--xsrf-state-p state))
                   (should (equal method "GET"))
                   (should (equal url "https://www.zhihu.com/"))
                   (cl-incf requests)
                   (setf (zhihu--xsrf-state-xsrf-token state)
                         (format "token-%d" requests))
                   '(:status 200)))))
      (let ((first (zhihu--make-xsrf-state))
            (second (zhihu--make-xsrf-state)))
        (should (equal (zhihu--ensure-xsrf-token first) "token-1"))
        (should (equal (zhihu--ensure-xsrf-token first) "token-1"))
        (should (equal (zhihu--ensure-xsrf-token second) "token-2"))
        (should (= requests 2))))))

(ert-deftest zhihu-http-sends-and-refreshes-xsrf-state ()
  (let ((state (zhihu--make-xsrf-state "current"))
        sent-headers)
    (cl-letf (((symbol-function 'zhihu--read-browser-cookies)
               (lambda (url)
                 (should (equal url "https://www.zhihu.com/test"))
                 '(("z_c0" . "login")
                   ("_xsrf" . "stale"))))
              ((symbol-function 'plz)
               (lambda (_method _url &rest args)
                 (setq sent-headers
                       (copy-tree (plist-get args :headers)))
                 (make-plz-response
                  :status 200
                  :headers
                  '((set-cookie . "_xsrf=rotated; Path=/; Secure"))
                  :body "{}"))))
      (should
       (eq (plist-get
            (zhihu--http
             "GET" "https://www.zhihu.com/test"
             :xsrf-state state
             :with-zhihu-cookies t)
            :status)
           200)))
    (should
     (equal (cdr (assoc-string "Cookie" sent-headers t))
            "z_c0=login; _xsrf=current"))
    (should
     (equal (zhihu--xsrf-state-xsrf-token state) "rotated"))))

(ert-deftest zhihu-xsrf-bootstrap-and-rotation-feed-the-next-mutation ()
  (let* ((state (zhihu--make-xsrf-state))
         (make-response
          (lambda (&optional token)
            (make-plz-response
             :status 200
             :headers
             (and token
                  `((set-cookie
                     . ,(format "_xsrf=%s; Path=/; Secure" token))))
             :body "{}")))
         (responses
          (list (funcall make-response "bootstrapped")
                (funcall make-response "rotated")
                (funcall make-response)))
         requests)
    (cl-letf (((symbol-function 'zhihu--read-browser-cookies)
               (lambda (_url)
                 '(("z_c0" . "login")
                   ("_xsrf" . "stale"))))
              ((symbol-function 'plz)
               (lambda (_method url &rest args)
                 (push (list url
                             (copy-tree (plist-get args :headers)))
                       requests)
                 (pop responses))))
      (should (equal (zhihu--ensure-xsrf-token state) "bootstrapped"))
      (dolist (expected '("bootstrapped" "rotated"))
        (zhihu--zhuanlan-mutation-request
         state
         "POST" "https://zhuanlan.zhihu.com/api/test"
         "https://zhuanlan.zhihu.com/write"
         :body '(:value 1))
        (should
         (equal
          (cdr (assoc-string
                "Cookie" (cadar requests) t))
          (format "z_c0=login; _xsrf=%s" expected)))
        (should
         (equal
          (cdr (assoc-string
                "x-xsrftoken" (cadar requests) t))
          expected))
        (should
         (equal
          (cdr (assoc-string "Origin" (cadar requests) t))
          "https://zhuanlan.zhihu.com"))
        (should
         (equal
          (cdr (assoc-string "Referer" (cadar requests) t))
          "https://zhuanlan.zhihu.com/write"))))
    (setq requests (nreverse requests))
    (should
     (equal
      (cdr (assoc-string "Cookie" (cadar requests) t))
      "z_c0=login"))
    (should (equal (zhihu--xsrf-state-xsrf-token state) "rotated"))))

(ert-deftest zhihu-hmac-sha1-base64-matches-known-vector ()
  (should
   (equal
    (zhihu--hmac-sha1-base64
     "key" "The quick brown fox jumps over the lazy dog")
    "3nybhbi3iqa8ino29wqQcBydtNk=")))

(ert-deftest zhihu-oss-put-sends-binary-without-zhihu-credentials ()
  (let ((bytes (unibyte-string 0 255 1))
        signed-data
        sent-url
        sent-method
        sent-body
        sent-headers
        sent-body-type)
    (cl-letf
        (((symbol-function 'zhihu--read-browser-cookies)
          (lambda (&rest _args)
            (ert-fail "OSS request must not read Zhihu cookies")))
         ((symbol-function 'zhihu--hmac-sha1-base64)
          (lambda (key data)
            (should (equal key "key"))
            (setq signed-data data)
            "signature"))
         ((symbol-function 'plz)
          (lambda (method url &rest args)
            (setq sent-url url
                  sent-method method
                  sent-body (plist-get args :body)
                  sent-headers (copy-tree (plist-get args :headers))
                  sent-body-type (plist-get args :body-type))
            (make-plz-response :status 200 :headers nil :body ""))))
      (zhihu--image-oss-put
       "object-key"
       bytes
       "image/png"
       '(:access_id "id"
		    :access_key "key"
		    :access_token "token")))
    (should
     (equal sent-url
            "https://zhihu-pics-upload.zhimg.com/object-key"))
    (should (eq sent-method 'put))
    (should (eq sent-body-type 'binary))
    (should (equal sent-body bytes))
    (should-not (multibyte-string-p sent-body))
    (should
     (equal (cdr (assoc-string "Content-Type" sent-headers t))
            "image/png"))
    (should
     (equal
      (cdr (assoc-string "x-oss-security-token" sent-headers t))
      "token"))
    (should
     (equal (cdr (assoc-string "Authorization" sent-headers t))
            "OSS id:signature"))
    (let ((date (cdr (assoc-string "x-oss-date" sent-headers t)))
          (user-agent
           (cdr (assoc-string "x-oss-user-agent" sent-headers t))))
      (should
       (equal
        signed-data
        (zhihu--oss-string-to-sign
         "image/png" date "token" "object-key" user-agent))))
    (should-not (assoc-string "Cookie" sent-headers t))
    (should-not (assoc-string "x-xsrftoken" sent-headers t))
    (should-not (assoc-string "x-zse-93" sent-headers t))
    (should-not (assoc-string "x-zse-96" sent-headers t))))

(ert-deftest zhihu-image-poll-rejects-non-200-without-retrying ()
  (let ((requests 0)
        (sleeps 0))
    (cl-letf (((symbol-function 'zhihu--http-json)
               (lambda (method url &rest args)
                 (should (equal method "GET"))
                 (should
                  (equal url "https://api.zhihu.com/images/image-id"))
                 (should (plist-member args :sign-json))
                 (should-not (plist-get args :sign-json))
                 (cl-incf requests)
                 '(:status 503 :body "unavailable" :json nil)))
              ((symbol-function 'sleep-for)
               (lambda (&rest _args)
                 (cl-incf sleeps))))
      (should-error (zhihu--image-poll "image-id")))
    (should (= requests 1))
    (should (= sleeps 0))))

(ert-deftest zhihu-image-poll-rejects-invalid-json-without-retrying ()
  (let ((requests 0)
        (sleeps 0))
    (cl-letf (((symbol-function 'zhihu--http-json)
               (lambda (&rest _args)
                 (cl-incf requests)
                 '(:status 200 :body "{not-json" :json nil)))
              ((symbol-function 'sleep-for)
               (lambda (&rest _args)
                 (cl-incf sleeps))))
      (should-error (zhihu--image-poll "image-id")))
    (should (= requests 1))
    (should (= sleeps 0))))

(ert-deftest zhihu-image-poll-retries-init-and-pending-then-returns-src ()
  (let ((responses
         (list '(:status 200
			 :body "{\"status\":\"init\"}"
			 :json (:status "init"))
               '(:status 200
			 :body "{\"status\":\"pending\"}"
			 :json (:status "pending"))
               '(:status 200
			 :body
			 "{\"status\":\"success\",\"src\":\"https://picx.zhimg.com/a.png\"}"
			 :json
			 (:status "success"
				  :src "https://picx.zhimg.com/a.png"))))
        (requests 0)
        (sleeps 0))
    (cl-letf (((symbol-function 'zhihu--http-json)
               (lambda (&rest _args)
                 (cl-incf requests)
                 (prog1 (car responses)
                   (setq responses (cdr responses)))))
              ((symbol-function 'sleep-for)
               (lambda (&rest _args)
                 (cl-incf sleeps))))
      (should
       (equal (zhihu--image-poll "image-id")
              "https://picx.zhimg.com/a.png")))
    (should (= requests 3))
    (should (= sleeps 2))
    (should-not responses)))

(ert-deftest zhihu-image-poll-times-out-after-retriable-statuses ()
  (let ((requests 0)
        (sleeps 0))
    (cl-letf (((symbol-function 'zhihu--http-json)
               (lambda (&rest _args)
                 (cl-incf requests)
                 (let ((status (if (cl-oddp requests) "init" "pending")))
                   (list
                    :status 200
                    :body (format "{\"status\":%S}" status)
                    :json (list :status status)))))
              ((symbol-function 'sleep-for)
               (lambda (&rest _args)
                 (cl-incf sleeps))))
      (should-error (zhihu--image-poll "image-id")))
    (should (= requests 10))
    (should (= sleeps 9))))

(ert-deftest zhihu-image-poll-rejects-failed-status-without-retrying ()
  (dolist (status '("failed" "error"))
    (let ((requests 0)
          (sleeps 0))
      (cl-letf (((symbol-function 'zhihu--http-json)
                 (lambda (&rest _args)
                   (cl-incf requests)
                   (list :status 200
                         :body (format "{\"status\":%S}" status)
                         :json (list :status status))))
                ((symbol-function 'sleep-for)
                 (lambda (&rest _args)
                   (cl-incf sleeps))))
        (should-error (zhihu--image-poll "image-id")))
      (should (= requests 1))
      (should (= sleeps 0)))))

(ert-deftest zhihu-image-poll-rejects-unknown-status-without-retrying ()
  (let ((requests 0)
        (sleeps 0))
    (cl-letf (((symbol-function 'zhihu--http-json)
               (lambda (&rest _args)
                 (cl-incf requests)
                 '(:status 200
			   :body "{\"status\":\"mystery\"}"
			   :json (:status "mystery"))))
              ((symbol-function 'sleep-for)
               (lambda (&rest _args)
                 (cl-incf sleeps))))
      (should-error (zhihu--image-poll "image-id")))
    (should (= requests 1))
    (should (= sleeps 0))))

(ert-deftest zhihu-image-poll-rejects-missing-status-without-retrying ()
  (let ((requests 0)
        (sleeps 0))
    (cl-letf (((symbol-function 'zhihu--http-json)
               (lambda (&rest _args)
                 (cl-incf requests)
                 '(:status 200
			   :body "{\"src\":\"unused\"}"
			   :json (:src "unused"))))
              ((symbol-function 'sleep-for)
               (lambda (&rest _args)
                 (cl-incf sleeps))))
      (should-error (zhihu--image-poll "image-id")))
    (should (= requests 1))
    (should (= sleeps 0))))

(ert-deftest zhihu-image-poll-rejects-success-without-src ()
  (let ((requests 0)
        (sleeps 0)
        failure)
    (cl-letf (((symbol-function 'zhihu--http-json)
               (lambda (&rest _args)
                 (cl-incf requests)
                 '(:status 200
			   :body "{\"status\":\"success\"}"
			   :json (:status "success"))))
              ((symbol-function 'sleep-for)
               (lambda (&rest _args)
                 (cl-incf sleeps))))
      (setq failure
            (should-error (zhihu--image-poll "image-id"))))
    (should (= requests 1))
    (should (= sleeps 0))
    (should (string-match-p "src" (error-message-string failure)))))

(ert-deftest zhihu-image-src-normalizes-local-path ()
  (let ((base-dir "/tmp/zhihu source/"))
    (should
     (equal
      (zhihu--normalize-img-src
       "images/a%20b.png?width=640#preview" base-dir)
      "/tmp/zhihu source/images/a b.png"))
    (should
     (equal
      (zhihu--normalize-img-src
       "%E4%B8%AD%E6%96%87%23%E5%9B%BE.png#preview" base-dir)
      "/tmp/zhihu source/中文#图.png"))))

(ert-deftest zhihu-image-src-handles-url-schemes-explicitly ()
  (dolist (src '("http://example.test/a.png"
                 "https://example.test/a.png"
                 "//example.test/a.png"))
    (should (eq (zhihu--normalize-img-src src "/tmp/") 'external)))
  (should
   (equal
    (zhihu--normalize-img-src
     "file:///tmp/a%20b.png?width=640#preview" "/unused/")
    "/tmp/a b.png"))
  (should
   (equal
    (zhihu--normalize-img-src
     "file://localhost/tmp/a.png" "/unused/")
    "/tmp/a.png"))
  (should-error
   (zhihu--normalize-img-src "ftp://example.test/a.png" "/tmp/"))
  (should-error
   (zhihu--normalize-img-src "file://example.test/a.png" "/tmp/")))

(ert-deftest zhihu-image-src-normalization-feeds-local-file-reader ()
  (let* ((directory (make-temp-file "zhihu-test-images-" t))
         (path (expand-file-name "a b.png" directory))
         (non-image-path (expand-file-name "not-image.txt" directory))
         (bytes (zhihu-test--utf8-bytes "image bytes")))
    (unwind-protect
        (progn
          (with-temp-file path
            (set-buffer-multibyte nil)
            (insert bytes))
          (with-temp-file non-image-path
            (insert "not an image"))
          (should
           (equal
            (zhihu--img-bytes-and-mime
             "a%20b.png?width=640#preview" directory)
            (cons "image/png" bytes)))
          (should-error
           (zhihu--img-bytes-and-mime "not-image.txt" directory)))
      (delete-directory directory t))))

(ert-deftest zhihu-svg-data-and-local-images-share-one-png-normalizer ()
  (let* ((first
          (encode-coding-string
           "<svg xmlns=\"http://www.w3.org/2000/svg\"><text>一</text></svg>"
           'utf-8 t))
         (second
          (encode-coding-string
           "<svg xmlns=\"http://www.w3.org/2000/svg\"><text>二</text></svg>"
           'utf-8 t))
         rendered)
    (cl-letf
        (((symbol-function 'zhihu--render-svg-png)
          (lambda (bytes)
            (push bytes rendered)
            zhihu-test--mermaid-png)))
      (let ((base64-image
             (zhihu--img-bytes-and-mime
              (concat
               "data:IMAGE/SVG+XML;base64,"
               (base64-encode-string first t))
              "/tmp/"))
            (percent-image
             (zhihu--img-bytes-and-mime
              (concat
               "data:image/svg+xml;charset=utf-8,"
               (url-hexify-string second))
              "/tmp/")))
        (dolist (image (list base64-image percent-image))
          (should (equal (car image) "image/png"))
          (should (equal (cdr image) zhihu-test--mermaid-png))))
      (zhihu-test--with-temp-binary-file
       ".svg"
       first
       (lambda (file)
         (let ((image
                (zhihu--img-bytes-and-mime
                 (file-name-nondirectory file)
                 (file-name-directory file))))
           (should (equal (car image) "image/png"))
           (should (equal (cdr image) zhihu-test--mermaid-png)))))
      (should
       (equal
        (zhihu--img-bytes-and-mime
         "data:image/png;base64,aGVsbG8="
         "/tmp/")
        (cons "image/png" (zhihu-test--utf8-bytes "hello"))))
      (should
       (eq
        (zhihu--img-bytes-and-mime
         "https://example.test/diagram.svg"
         "/tmp/")
        'external)))
    (should (equal (nreverse rendered) (list first second first)))))

(ert-deftest zhihu-upload-image-file-rasterizes-svg-before-upload ()
  (let ((svg (zhihu-test--utf8-bytes "<svg/>"))
        uploaded
        killed)
    (cl-letf
        (((symbol-function 'zhihu--read-file-bytes)
          (lambda (path)
            (should (equal path "/tmp/diagram.svg"))
            svg))
         ((symbol-function 'zhihu--render-svg-png)
          (lambda (bytes)
            (should (equal bytes svg))
            zhihu-test--mermaid-png))
         ((symbol-function 'zhihu--upload-bytes)
          (lambda (bytes mime source)
            (setq uploaded (list bytes mime source))
            "https://picx.zhimg.com/diagram.png"))
         ((symbol-function 'kill-new)
          (lambda (url)
            (setq killed url))))
      (should
       (equal
        (zhihu-upload-image-file "/tmp/diagram.svg")
        "https://picx.zhimg.com/diagram.png")))
    (should
     (equal
      uploaded
      (list zhihu-test--mermaid-png "image/png" "answer")))
    (should (equal killed "https://picx.zhimg.com/diagram.png"))))

(ert-deftest zhihu-image-local-path-requires-explicit-base-directory ()
  (dolist (base-dir '(nil ""))
    (let ((failure
           (should-error
            (zhihu--img-bytes-and-mime "image.png" base-dir))))
      (should
       (string-match-p "base-dir" (error-message-string failure))))))

(ert-deftest zhihu-upload-bytes-puts-only-when-prefetch-requires-it ()
  (let ((bytes (zhihu-test--utf8-bytes "bytes"))
        (hash "4b3a6218bb3e3a7303e8a171a60fcf92"))
    (dolist (state '(1 2))
      (let ((puts 0)
            (polls 0))
        (cl-letf
            (((symbol-function 'zhihu--image-prefetch)
              (lambda (actual-hash source)
                (should (equal actual-hash hash))
                (should (equal source "answer"))
                `(:upload_file
                  (:state ,state
			  :image_id "image-id"
			  :object_key "object-key")
                  :upload_token (:access_id "id"))))
             ((symbol-function 'zhihu--image-oss-put)
              (lambda (object-key actual-bytes mime token)
                (cl-incf puts)
                (should (equal object-key "object-key"))
                (should (equal actual-bytes bytes))
                (should (equal mime "image/png"))
                (should (equal token '(:access_id "id")))))
             ((symbol-function 'zhihu--image-poll)
              (lambda (image-id)
                (cl-incf polls)
                (should (equal image-id "image-id"))
                "https://picx.zhimg.com/image.png")))
          (should
           (equal
            (zhihu--upload-bytes bytes "image/png" "answer")
            "https://picx.zhimg.com/image.png")))
        (should (= puts (if (= state 2) 1 0)))
        (should (= polls 1))))))

(ert-deftest zhihu-upload-bytes-rejects-unknown-prefetch-state-before-poll ()
  (let ((polls 0))
    (cl-letf
        (((symbol-function 'zhihu--image-prefetch)
          (lambda (&rest _args)
            '(:upload_file (:state 3 :image_id "image-id"))))
         ((symbol-function 'zhihu--image-poll)
          (lambda (&rest _args)
            (cl-incf polls))))
      (should-error
       (zhihu--upload-bytes
        (zhihu-test--utf8-bytes "bytes")
        "image/png"
        "answer")))
    (should (= polls 0))))

(ert-deftest zhihu-upload-bytes-rejects-unknown-source ()
  (dolist (source '(nil "question"))
    (should-error
     (zhihu--upload-bytes
      (zhihu-test--utf8-bytes "bytes")
      "image/png"
      source))))

(ert-deftest zhihu-upload-image-file-uses-explicit-answer-source ()
  (let ((bytes (zhihu-test--utf8-bytes "bytes"))
        killed)
    (cl-letf (((symbol-function 'zhihu--read-file-bytes)
               (lambda (path)
                 (should (equal path "/tmp/image.png"))
                 bytes))
              ((symbol-function 'zhihu--upload-bytes)
               (lambda (uploaded mime source)
                 (should (equal uploaded bytes))
                 (should (equal mime "image/png"))
                 (should (equal source "answer"))
                 "https://picx.zhimg.com/uploaded.png"))
              ((symbol-function 'kill-new)
               (lambda (url) (setq killed url))))
      (should
       (equal
        (zhihu-upload-image-file "/tmp/image.png")
        "https://picx.zhimg.com/uploaded.png")))
    (should (equal killed "https://picx.zhimg.com/uploaded.png"))))

(ert-deftest zhihu-upload-image-file-rejects-non-image ()
  (should-error
   (zhihu-upload-image-file "/tmp/not-image.txt")
   :type 'user-error))

(ert-deftest zhihu-image-rewrite-forwards-explicit-source ()
  (dolist (source '("answer" "article"))
    (cl-letf (((symbol-function 'zhihu--upload-bytes)
               (lambda (_bytes _mime uploaded-source)
                 (should (equal uploaded-source source))
                 "https://picx.zhimg.com/uploaded.png")))
      (should
       (equal
        (zhihu-test--image-srcs
         (zhihu--rewrite-img-srcs
          "<img src=\"data:image/png;base64,aGVsbG8=\">"
          "/tmp/"
         source))
        '("https://picx.zhimg.com/uploaded.png"))))))

(ert-deftest zhihu-image-rewrite-preserves-native-caption-attributes ()
  (cl-letf
      (((symbol-function 'zhihu--upload-bytes)
        (lambda (_bytes mime source)
          (should (equal mime "image/png"))
          (should (equal source "article"))
          "https://picx.zhimg.com/uploaded.png")))
    (let* ((html
            (zhihu--rewrite-img-srcs
             (concat
              "<p><img src=\"data:image/png;base64,aGVsbG8=\" "
              "alt=\"替代文字\" data-caption=\"原生图注\" "
              "data-size=\"normal\"></p>")
             "/tmp/"
             "article"))
           (dom
            (zhihu--parse-html
             (concat "<html><body>" html "</body></html>")))
           (image (car (dom-by-tag dom 'img))))
      (should
       (equal
        (dom-attr image 'src)
        "https://picx.zhimg.com/uploaded.png"))
      (should (equal (dom-attr image 'alt) "替代文字"))
      (should (equal (dom-attr image 'data-caption) "原生图注"))
      (should (equal (dom-attr image 'data-size) "normal")))))

(ert-deftest zhihu-image-rewrite-skips-only-web-urls ()
  (let ((urls '("http://example.test/a.png"
                "https://example.test/b.png"
                "//example.test/c.png"))
        checked-paths)
    (cl-letf (((symbol-function 'file-readable-p)
               (lambda (path)
                 (push path checked-paths)
                 nil))
              ((symbol-function 'zhihu--upload-bytes)
               (lambda (&rest _args)
                 (ert-fail "External images must not be uploaded"))))
      (let* ((html (mapconcat
                    (lambda (url) (format "<img src=\"%s\">" url))
                    urls
                    ""))
             (rewritten
              (zhihu--rewrite-img-srcs
               html
               "/tmp/"
               "answer")))
        (should (equal (zhihu-test--image-srcs rewritten) urls))))
    (should-not checked-paths)))

(ert-deftest zhihu-image-rewrite-uploads-each-repeated-data-image ()
  (let ((bytes (zhihu-test--utf8-bytes "hello"))
        (uploads 0))
    (cl-letf (((symbol-function 'zhihu--upload-bytes)
               (lambda (uploaded mime _source)
                 (cl-incf uploads)
                 (should (equal uploaded bytes))
                 (should (equal mime "image/png"))
                 (format "https://picx.zhimg.com/uploaded-%d.png"
                         uploads))))
      (let ((html
             (zhihu--rewrite-img-srcs
              (concat
               "<img src=\"data:image/png;base64,aGVsbG8=\">"
               "<img src=\"data:image/png;base64,aGVsbG8=\">")
              "/tmp/"
              "answer")))
        (should
         (equal (zhihu-test--image-srcs html)
                '("https://picx.zhimg.com/uploaded-1.png"
                  "https://picx.zhimg.com/uploaded-2.png")))))
    (should (= uploads 2))))

(ert-deftest zhihu-image-rewrite-rejects-missing-local-file ()
  (let ((directory (make-temp-file "zhihu-test-images-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'zhihu--upload-bytes)
                   (lambda (&rest _args)
                     (ert-fail "A missing file must not be uploaded"))))
          (should-error
           (zhihu--rewrite-img-srcs
            "<img src=\"missing.png\">"
            directory
            "answer")))
      (delete-directory directory t))))

(ert-deftest zhihu-image-rewrite-rejects-invalid-data-url ()
  (cl-letf (((symbol-function 'zhihu--upload-bytes)
             (lambda (&rest _args)
               (ert-fail "Invalid data must not be uploaded"))))
    (dolist (src '("data:not-a-valid-image"
                   "data:text/plain;base64,aGVsbG8="))
      (should-error
       (zhihu--rewrite-img-srcs
        (format "<img src=\"%s\">" src)
        "/tmp/"
        "answer")))))

(ert-deftest zhihu-pin-image-dimensions-support-common-formats ()
  (let* ((png
          (concat
           (unibyte-string
            #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a
            0 0 0 13)
           "IHDR"
           (zhihu-test--u32-be 320)
           (zhihu-test--u32-be 240)))
         (gif
          (concat "GIF89a"
                  (unibyte-string #x40 #x01 #xf0 #x00)))
         (jpeg
          (unibyte-string
           #xff #xd8
           #xff #xe0 0 4 0 0
           #xff #xc0 0 11 8
           0 #xf0 #x01 #x40
           3 1 0 2 0 3 0
           #xff #xd9))
         (webp
          (concat
           "RIFF"
           (unibyte-string 22 0 0 0)
           "WEBPVP8X"
           (unibyte-string 10 0 0 0 0 0 0 0
                           #x3f #x01 0
                           #xef 0 0))))
    (should (equal (zhihu--image-dimensions png "image/png")
                   '(320 . 240)))
    (should (equal (zhihu--image-dimensions gif "image/gif")
                   '(320 . 240)))
    (should (equal (zhihu--image-dimensions jpeg "image/jpeg")
                   '(320 . 240)))
    (should (equal (zhihu--image-dimensions webp "image/webp")
                   '(320 . 240)))
    (should-error
     (zhihu--image-dimensions
      (zhihu-test--utf8-bytes "not an image")
      "image/png"))))

(ert-deftest zhihu-pin-content-extracts-images-in-document-order ()
  (let (uploaded)
    (cl-letf
        (((symbol-function 'zhihu--img-bytes-and-mime)
          (lambda (src base-dir)
            (should (equal base-dir "/tmp/source/"))
            (cons "image/png" (zhihu-test--utf8-bytes src))))
         ((symbol-function 'zhihu--image-dimensions)
          (lambda (bytes mime)
            (should (equal mime "image/png"))
            (if (equal bytes (zhihu-test--utf8-bytes "one.png"))
                '(100 . 80)
              '(200 . 160))))
         ((symbol-function 'zhihu--upload-bytes)
          (lambda (bytes mime source)
            (should (equal mime "image/png"))
            (should (equal source "pin"))
            (push bytes uploaded)
            (format "https://picx.zhimg.com/%s"
                    (decode-coding-string bytes 'utf-8)))))
      (let* ((content
              (zhihu--pin-content-from-html
               (concat
                "<p>before<img src=\"one.png\">after</p>"
                "<p><img src=\"two.png\"></p>")
               "/tmp/source/"))
             (medias (plist-get content :medias)))
        (should (equal (plist-get content :html)
                       "<p>beforeafter</p>"))
        (should (= (plist-get content :text-length) 11))
        (should-not
         (string-match-p "<img" (plist-get content :html)))
        (should
         (equal
          (append medias nil)
          '((:image
             (:height 80 :width 100
		      :url "https://picx.zhimg.com/one.png"
		      :originalUrl "https://picx.zhimg.com/one.png"))
            (:image
             (:height 160 :width 200
		      :url "https://picx.zhimg.com/two.png"
		      :originalUrl "https://picx.zhimg.com/two.png")))))
        (dolist (media (append medias nil))
          (let ((image (plist-get media :image)))
            (should-not (plist-member image :watermark))
            (should-not (plist-member image :watermarkUrl))))))
    (should
     (equal
      (mapcar
       (lambda (bytes) (decode-coding-string bytes 'utf-8))
       (nreverse uploaded))
      '("one.png" "two.png")))))

(ert-deftest zhihu-pin-caption-degrades-to-body-text-without-using-alt ()
  (let ((uploads 0))
    (cl-letf
        (((symbol-function 'zhihu--img-bytes-and-mime)
          (lambda (src _base-dir)
            (cons "image/png" (zhihu-test--utf8-bytes src))))
         ((symbol-function 'zhihu--image-dimensions)
          (lambda (&rest _args) '(20 . 10)))
         ((symbol-function 'zhihu--upload-bytes)
          (lambda (_bytes _mime source)
            (should (equal source "pin"))
            (cl-incf uploads)
            "https://picx.zhimg.com/image.png")))
      (let ((captioned
             (zhihu--pin-content-from-html
              (concat
               "<p>before</p>"
               "<p><img src=\"captioned.png\" "
               "alt=\"替代文字\" data-caption=\"  图片   注释  \"></p>")
              "/tmp/"))
            (alt-only
             (zhihu--pin-content-from-html
              "<p><img src=\"alt-only.png\" alt=\"替代文字\"></p>"
              "/tmp/")))
        (should
         (equal
          (plist-get captioned :html)
          "<p>before</p><p>图片 注释</p>"))
        (should (= (plist-get captioned :text-length) 11))
        (should (equal (plist-get alt-only :html) ""))
        (should (= (plist-get alt-only :text-length) 0))
        (should (= (length (plist-get captioned :medias)) 1))
        (should (= (length (plist-get alt-only :medias)) 1))))
    (should (= uploads 2))))

(ert-deftest zhihu-pin-images-are-preflighted-before-upload ()
  (let ((uploads 0))
    (cl-letf
        (((symbol-function 'zhihu--img-bytes-and-mime)
          (lambda (src _base-dir)
            (if (equal src "external")
                'external
              (cons "image/png" (zhihu-test--utf8-bytes src)))))
         ((symbol-function 'zhihu--image-dimensions)
          (lambda (&rest _args) '(1 . 1)))
         ((symbol-function 'zhihu--upload-bytes)
          (lambda (&rest _args)
            (cl-incf uploads)
            "https://picx.zhimg.com/image.png")))
      (should-error
       (zhihu--pin-content-from-html
        "<img src=\"local\"><img src=\"external\">"
        "/tmp/")))
    (should (= uploads 0))))

(ert-deftest zhihu-pin-content-keeps-equations-out-of-photo-media ()
  (let ((uploads 0))
    (cl-letf
        (((symbol-function 'zhihu--img-bytes-and-mime)
          (lambda (src _base-dir)
            (should (equal src "photo.png"))
            (cons "image/png" (string-to-unibyte "photo"))))
         ((symbol-function 'zhihu--image-dimensions)
          (lambda (&rest _args) '(20 . 10)))
         ((symbol-function 'zhihu--upload-bytes)
          (lambda (_bytes _mime source)
            (should (equal source "pin"))
            (cl-incf uploads)
            "https://picx.zhimg.com/photo.png")))
      (let* ((content
              (zhihu--pin-content-from-html
               (concat
                "<p><img eeimg=\"1\" "
                "src=\"//www.zhihu.com/equation?tex=x\" alt=\"x\"></p>"
                "<p><img src=\"photo.png\"></p>")
               "/tmp/"))
             (html (plist-get content :html)))
        (should (= uploads 1))
        (should (= (plist-get content :text-length) 0))
        (should
         (equal
          (zhihu-test--image-srcs html)
          '("//www.zhihu.com/equation?tex=x")))
        (should (= (length (plist-get content :medias)) 1))))))

(ert-deftest zhihu-pin-content-rejects-lossy-or-empty-input ()
  (cl-letf
      (((symbol-function 'zhihu--img-bytes-and-mime)
        (lambda (&rest _args)
          (ert-fail "Validation should fail before reading images"))))
    (should-error
     (zhihu--pin-content-from-html
      ""
      "/tmp/"))
    (should-error
     (zhihu--pin-content-from-html
      (concat
       "<a href=\"https://example.test\" "
       "data-draft-type=\"link-card\">card</a>")
      "/tmp/"))
    (should-error
     (zhihu--pin-content-from-html
      (concat
       "<p><img eeimg=\"1\" "
       "src=\"//www.zhihu.com/equation?tex=x\"></p>")
      "/tmp/"))
    (should-error
     (zhihu--pin-content-from-html
      (mapconcat
       (lambda (_index) "<img src=\"x.png\">")
       (number-sequence 1 19)
       "")
      "/tmp/"))))

(ert-deftest zhihu-pin-text-length-matches-web-builder ()
  (should (= (zhihu--pin-html-text-length "<p>A😀&amp;B</p>") 8))
  (let ((content
         (zhihu--pin-content-from-html
          "<p>A😀</p>"
          "/tmp/")))
    (should (= (plist-get content :text-length) 2)))
  (should-error
   (zhihu--pin-content-from-html
    (concat "<p>" (make-string 2001 ?a) "</p>")
    "/tmp/")))

(ert-deftest zhihu-publish-bodies-separate-answer-and-article-fields ()
  (let* ((answer-body
          (zhihu-test--publish-answer-body
           nil "123" "<p>answer</p>"))
         (answer-data (plist-get answer-body :data))
         (answer-extra (plist-get answer-data :extra_info))
         (answer-draft (plist-get answer-data :draft))
         (article-body (zhihu-test--publish-article-body "456" t))
         (article-data (plist-get article-body :data))
         (article-extra (plist-get article-data :extra_info))
         (article-draft (plist-get article-data :draft)))
    (should (equal (plist-get answer-body :action) "answer"))
    (should (equal (plist-get answer-extra :question_id) "123"))
    (should (equal (plist-get (plist-get answer-data :hybrid) :html)
                   "<p>answer</p>"))
    (should-not (plist-member answer-draft :contentId))
    (should (eq (plist-get answer-draft :isPublished) :json-false))
    (should (equal (plist-get article-body :action) "article"))
    (should (equal (plist-get article-draft :id) "456"))
    (should (eq (plist-get article-draft :isPublished) t))
    (should-not (plist-member article-extra :question_id))
    (should-not (plist-member article-data :hybrid))))

(ert-deftest zhihu-publish-pin-builds-new-and-update-payloads ()
  (let* ((content
          '(:html "<p>想法</p>"
		  :text-length 2
		  :medias
		  [(:image
		    (:height 80 :width 100
			     :url "https://picx.zhimg.com/image.png"
			     :originalUrl "https://picx.zhimg.com/image.png"))]))
         (topics
          [(:topic_id "123" :topic_name "#Emacs#")])
         (new
          (zhihu-test--publish-pin-body
           nil "标题" content topics
           :comment-permission "follower_n_days"
           :trace-id "1700000000000,uuid"))
         (update
          (zhihu-test--publish-pin-body
           "456" nil content []
           :trace-id "1700000000001,uuid"))
         (new-data (plist-get new :data))
         (new-draft (plist-get new-data :draft))
         (update-data (plist-get update :data))
         (update-draft (plist-get update-data :draft)))
    (should (equal (plist-get new :action) "pin"))
    (should (equal new-draft '(:disabled 1)))
    (should-not (plist-member new-draft :id))
    (should-not (plist-member new-draft :isPublished))
    (should
     (equal
      (plist-get (plist-get new-data :publish) :traceId)
      "1700000000000,uuid"))
    (should
     (equal
      (plist-get
       (plist-get new-data :commentsPermission)
       :comment_permission)
      "follower_n_days"))
    (should
     (equal (plist-get (plist-get new-data :extra_info)
                       :view_permission)
            "all"))
    (should
     (equal (plist-get (plist-get new-data :extra_info) :publisher)
            "pc"))
    (should (equal (plist-get (plist-get new-data :title) :title)
                   "标题"))
    (should
     (equal (plist-get (plist-get new-data :hybrid) :html)
            "<p>想法</p>"))
    (should (= (plist-get (plist-get new-data :hybrid) :textLength)
               2))
    (should
     (equal (plist-get (plist-get new-data :media) :medias)
            (plist-get content :medias)))
    (should
     (equal (plist-get (plist-get new-data :topic) :topics)
            topics))
    (dolist
        (article-field
         '(:reprint :creationStatement :contentsTables :hybridInfo
		    :publishSwitch :appreciate))
      (should-not (plist-member new-data article-field)))
    (should (equal (plist-get update :action) "pin"))
    (should (equal (plist-get update-draft :id) "456"))
    (should (eq (plist-get update-draft :isPublished) t))
    (should-not (plist-member update-data :title))
    (should-not (plist-member update-data :topic))))

(ert-deftest zhihu-publish-pin-omits-empty-conditional-sections ()
  (let* ((content
          '(:html "" :text-length 0
		  :medias
		  [(:image
		    (:height 1 :width 1
			     :url "https://picx.zhimg.com/image.png"
			     :originalUrl "https://picx.zhimg.com/image.png"))]))
         (body
          (zhihu-test--publish-pin-body
           nil nil content [] :trace-id "trace"))
         (data (plist-get body :data)))
    (should-not (plist-member data :title))
    (should-not (plist-member data :hybrid))
    (should-not (plist-member data :topic))
    (should (plist-member data :media)))
  (let* ((body
          (zhihu-test--publish-pin-body
           nil nil
           '(:html
             "<p><img eeimg=\"1\" src=\"//www.zhihu.com/equation?tex=x\"></p>"
             :text-length 0
             :medias
             [(:image
               (:height 1 :width 1
			:url "https://picx.zhimg.com/image.png"
			:originalUrl "https://picx.zhimg.com/image.png"))])
           []
           :trace-id "trace"))
         (data (plist-get body :data)))
    (should (plist-member data :hybrid))
    (should (plist-member data :media)))
  (should-error
   (zhihu-test--publish-pin-body
    nil nil
    '(:html
      "<p><img eeimg=\"1\" src=\"//www.zhihu.com/equation?tex=x\"></p>"
      :text-length 0
      :medias [])
    []
    :trace-id "trace"))
  (should-error
   (zhihu-test--publish-pin-body
    nil nil
    '(:html "" :text-length 0 :medias [])
    []
    :trace-id "trace"))
  (should
   (equal
    (zhihu--normalize-pin-title (make-string 50 #x1f600))
    (make-string 50 #x1f600)))
  (should-error
   (zhihu-test--publish-pin-body
    nil (make-string 51 #x1f600)
    '(:html "<p>x</p>" :text-length 1 :medias [])
    []
    :trace-id "trace")))

(ert-deftest zhihu-pin-topic-data-resolves-name-and-id ()
  (cl-letf
      (((symbol-function 'zhihu--resolve-article-topic)
        (lambda (name)
          (should (equal name "Emacs"))
          '(:id 123 :name "Emacs"))))
    (should
     (equal
      (zhihu--pin-topic-data '("Emacs"))
      [(:topic_id "123" :topic_name "#Emacs#")]))))

(ert-deftest zhihu-publish-default-options-feed-payload ()
  (let ((zhihu-publish-defaults
         '((draft_type . "custom-draft")
           (delta_time . 73)
           (can_reward . t)))
        (zhihu-publish-default-reprint-permission "need_payment")
        (zhihu-publish-default-comment-permission "censor"))
    (dolist (body
             (list
              (zhihu-test--publish-answer-body
               "456" "123" "<p>answer</p>")
              (zhihu-test--publish-article-body "789" t)))
      (let* ((data (plist-get body :data))
             (extra (plist-get data :extra_info))
             (pc
              (json-parse-string
               (plist-get extra :pc_business_params)
               :null-object :json-null
               :false-object :json-false
               :object-type 'plist)))
        (should
         (equal
          (plist-get (plist-get data :reprint) :reshipment_settings)
          "need_payment"))
        (should
         (equal
          (plist-get (plist-get data :publishSwitch) :draft_type)
          "custom-draft"))
        (should
         (eq
          (plist-get
           (plist-get data :contentsTables)
           :table_of_contents_enabled)
          :json-false))
        (should
         (equal
          (plist-get
           (plist-get data :commentsPermission)
           :comment_permission)
          "censor"))
        (should
         (eq (plist-get (plist-get data :appreciate) :can_reward) t))
        (should
         (eq (plist-get (plist-get pc :reward_setting) :can_reward) t))
        (should
         (equal (plist-get pc :reshipment_settings)
                "need_payment"))
        (should
         (equal (plist-get pc :comment_permission) "censor"))
        (should
         (eq
          (plist-get pc :table_of_contents_enabled)
          :json-false))))))

(ert-deftest zhihu-publish-default-permissions-must-be-nonempty ()
  (dolist (defaults
           '((nil "all")
             ("" "all")
             ("unknown" "all")
             ("allowed" nil)
             ("allowed" "")
             ("allowed" "unknown")))
    (let ((zhihu-publish-default-reprint-permission (car defaults))
          (zhihu-publish-default-comment-permission (cadr defaults)))
      (should-error (zhihu--resolve-publish-settings)))))

(ert-deftest zhihu-publish-thank-inviter-is-disabled ()
  (dolist (body
           (list
            (zhihu-test--publish-answer-body
             "456" "123" "<p>answer</p>")
            (zhihu-test--publish-article-body "789" t)))
    (let* ((data (plist-get body :data))
           (thanks (plist-get data :thanksInvitation))
           (pc
            (json-parse-string
             (plist-get (plist-get data :extra_info)
                        :pc_business_params)
             :null-object :json-null
             :false-object :json-false
             :object-type 'plist)))
      (dolist (settings (list thanks pc))
        (should
         (equal (plist-get settings :thank_inviter_status) "close"))
        (should (equal (plist-get settings :thank_inviter) ""))))))

(ert-deftest zhihu-creation-statement-feeds-both-payload-locations ()
  (dolist (case
           '((nil "close" "none")
             ("spoiler" "open" "spoiler")
             ("medical_advice" "open" "medical_advice")
             ("fictional_creation" "open" "fictional_creation")
             ("contain_finance" "open" "contain_finance")
             ("ai_creation" "open" "ai_creation")))
    (pcase-let ((`(,statement ,expected-status ,expected-type) case))
      (let* ((body
              (zhihu-test--publish-answer-body
               "456" "123" "<p>answer</p>"
               :creation-statement statement))
             (data (plist-get body :data))
             (outer (plist-get data :creationStatement))
             (pc
              (json-parse-string
               (plist-get (plist-get data :extra_info)
                          :pc_business_params)
               :null-object :json-null
               :false-object :json-false
               :object-type 'plist)))
        (should
         (equal (plist-get outer :disclaimer_status)
                expected-status))
        (should
         (equal (plist-get outer :disclaimer_type)
                expected-type))
        (should
         (equal (plist-get pc :disclaimer_status)
                expected-status))
        (should
         (equal (plist-get pc :disclaimer_type)
                expected-type))))))

(ert-deftest zhihu-content-source-feeds-hybrid-and-business-payloads ()
  (dolist
      (body
       (list
        (zhihu-test--publish-answer-body
         "456" "123" "<p>answer</p>"
         :content-source "officialWebsite")
        (zhihu-test--publish-article-body
         "789" t :content-source "TVMedia")))
    (let* ((data (plist-get body :data))
           (expected
            (if (equal (plist-get body :action) "article")
                "TVMedia"
              "officialWebsite"))
           (hybrid-source
            (plist-get (plist-get data :hybridInfo) :contentSource))
           (pc
            (json-parse-string
             (plist-get (plist-get data :extra_info)
                        :pc_business_params)
             :null-object :json-null
             :false-object :json-false
             :object-type 'plist)))
      (should (equal (plist-get hybrid-source :channel) expected))
      (should
       (equal
        (plist-get (plist-get pc :content_source) :channel)
        expected))))
  (let* ((body
          (zhihu-test--publish-answer-body
           "456" "123" "<p>answer</p>"))
         (data (plist-get body :data))
         (pc
          (json-parse-string
           (plist-get (plist-get data :extra_info) :pc_business_params)
           :object-type 'plist)))
    (should (hash-table-p (plist-get data :hybridInfo)))
    (should-not (plist-member pc :content_source))))

(ert-deftest zhihu-publish-metadata-settings-override-default-options ()
  (let ((zhihu-publish-default-reprint-permission "allowed")
        (zhihu-publish-default-comment-permission "all"))
    (dolist
        (body
         (list
          (zhihu-test--publish-answer-body
           "456" "123" "<p>answer</p>"
           :reprint-permission "disallowed"
           :comment-permission "followee")
          (zhihu-test--publish-article-body
           "789" t
           :toc t
           :reprint-permission "need_payment"
           :comment-permission "nobody")))
      (let* ((article-p (equal (plist-get body :action) "article"))
             (data (plist-get body :data))
             (pc
              (json-parse-string
               (plist-get (plist-get data :extra_info)
                          :pc_business_params)
               :null-object :json-null
               :false-object :json-false
               :object-type 'plist))
             (expected-reprint
              (if article-p "need_payment" "disallowed"))
             (expected-comment
              (if article-p "nobody" "followee"))
             (expected-toc (if article-p t :json-false)))
        (should
         (equal
          (plist-get (plist-get data :reprint) :reshipment_settings)
          expected-reprint))
        (should
         (equal (plist-get pc :reshipment_settings)
                expected-reprint))
        (should
         (equal
          (plist-get
           (plist-get data :commentsPermission)
           :comment_permission)
          expected-comment))
        (should
         (equal (plist-get pc :comment_permission)
                expected-comment))
        (should
         (eq
          (plist-get
           (plist-get data :contentsTables)
           :table_of_contents_enabled)
          expected-toc))
        (should
         (eq (plist-get pc :table_of_contents_enabled)
             expected-toc))))))

(ert-deftest zhihu-publish-accepts-confirmed-201-and-informational-message ()
  (cl-letf (((symbol-function 'zhihu--http-json)
             (lambda (&rest _args)
               '(:status 201
			 :json (:code 0
				      :message "created"
				      :data (:result (:publish (:id "456"))))
			 :body "{\"code\":0}"))))
    (should (equal
             (zhihu--publish-answer "456" "123" "<p>new</p>")
             "456"))))

(ert-deftest zhihu-publish-request-accepts-pin-result-id ()
  (dolist
      (result
       '((:id 456)
         "{\"id\":\"789\"}"))
    (cl-letf (((symbol-function 'zhihu--http-json)
               (lambda (&rest _args)
                 `(:status 200
			   :json (:code 0 :data (:result ,result))
			   :body "{\"code\":0}"))))
      (should
       (equal
        (zhihu--publish-request '(:action "pin" :data nil))
        (if (listp result) "456" "789"))))))

(ert-deftest zhihu-publish-request-combines-unconfirmed-response-errors ()
  (dolist
      (case
       '(((:status 503
		   :json (:code 100 :message "busy")
		   :body "{\"code\":100}"
		   :headers ((zhi-request-id . "req-1")))
          "HTTP 503" "code 100" "busy" "req-1")
         ((:status 200 :json nil :body "not-json")
          "HTTP 200" "code 无" "not-json")
         ((:status 200
		   :json (:code 3 :message "denied")
		   :body "{\"code\":3}")
          "HTTP 200" "code 3" "denied")))
    (let ((response (car case))
          (patterns (cdr case)))
      (cl-letf (((symbol-function 'zhihu--http-json)
                 (lambda (&rest _args) response)))
        (let ((message
               (condition-case err
                   (progn
                     (zhihu--publish-request '(:action "answer"))
                     nil)
                 (error (error-message-string err)))))
          (should message)
          (dolist (pattern patterns)
            (should
             (string-match-p (regexp-quote pattern) message))))))))

(ert-deftest zhihu-publish-article-forwards-xsrf-state ()
  (let ((xsrf-state (zhihu--make-xsrf-state "token"))
        request)
    (cl-letf
        (((symbol-function 'zhihu--http-json)
          (lambda (&rest args)
            (setq request args)
            '(:status 200
		      :json (:code 0 :data (:result (:publish (:id "456"))))
		      :body "{\"code\":0}"))))
      (should
       (equal
        (zhihu--publish-article
         "456" t :xsrf-state xsrf-state)
        "456")))
    (should
     (eq (plist-get request :xsrf-state) xsrf-state))))

(ert-deftest zhihu-publish-article-requires-article-id ()
  (cl-letf (((symbol-function 'zhihu--publish-request)
             (lambda (&rest _args)
               (ert-fail "Missing article-id must fail before publishing"))))
    (should-error (zhihu--publish-article nil t))))

(ert-deftest zhihu-checkpoint-meta-writes-then-refreshes ()
  (let* ((file (make-temp-file "zhihu-checkpoint-" nil ".md" "old"))
         (meta '(:kind answer :question-id "123"))
         (buffer (find-file-noselect file)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'zhihu--write-zhihu-meta)
                     (lambda (written-file written-meta)
                       (should (equal written-file file))
                       (should (equal written-meta meta))
                       (with-temp-file written-file
                         (insert "new")))))
            (zhihu--checkpoint-meta file meta))
          (with-current-buffer buffer
            (should (equal (buffer-string) "new"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest zhihu-existing-answer-publishes-without-metadata-write ()
  (cl-letf (((symbol-function 'zhihu--source-to-html)
             (lambda (_file) "<p>new</p>"))
            ((symbol-function 'zhihu--rewrite-img-srcs)
             (lambda (html _base-dir source)
               (should (equal source "answer"))
               html))
            ((symbol-function 'zhihu--checkpoint-meta)
             (lambda (&rest _args)
               (ert-fail "Existing answer metadata did not change")))
            ((symbol-function 'zhihu--publish-answer)
             (lambda (answer-id question-id html &rest args)
               (should (equal answer-id "456"))
               (should (equal question-id "123"))
               (should (equal html "<p>new</p>"))
               (should
                (equal (plist-get args :creation-statement) "spoiler"))
               (should
                (equal (plist-get args :content-source) "newsReport"))
               (should
                (equal (plist-get args :reprint-permission) "disallowed"))
               (should
                (equal (plist-get args :comment-permission) "followee"))
               answer-id)))
    (should
     (equal
      (zhihu--publish-answer-file
       "/tmp/existing-answer.md"
       (list :kind 'answer
             :question-id "123"
             :answer-id "456"
             :creation-statement "spoiler"
             :content-source "newsReport"
             :reprint-permission "disallowed"
             :comment-permission "followee"))
      "456"))))

(ert-deftest zhihu-package-publish-content-separates-only-type-policy ()
  (let ((html "<p>Shared HTML</p>")
        calls)
    (cl-letf
        (((symbol-function 'zhihu--rewrite-img-srcs)
          (lambda (actual-html base-dir source)
            (should (equal actual-html html))
            (should (equal base-dir "/tmp/"))
            (push (list 'rewrite source) calls)
            (format "%s:%s" source actual-html)))
         ((symbol-function 'zhihu--append-article-cc-statement)
          (lambda (actual-html)
            (push (list 'cc actual-html) calls)
            (concat actual-html ":cc")))
         ((symbol-function 'zhihu--pin-content-from-html)
          (lambda (actual-html base-dir)
            (should (equal actual-html html))
            (should (equal base-dir "/tmp/"))
            (push '(pin) calls)
            `(:html ,actual-html :text-length 11 :medias []))))
      (should
       (equal
        (zhihu--package-publish-content
         'answer html "/tmp/")
        '(:html "answer:<p>Shared HTML</p>")))
      (should
       (equal
        (zhihu--package-publish-content
         'article html "/tmp/")
        '(:html "article:<p>Shared HTML</p>:cc")))
      (should
       (equal
        (zhihu--package-publish-content
         'pin html "/tmp/")
        '(:html "<p>Shared HTML</p>" :text-length 11 :medias [])))
      (should-error
       (zhihu--package-publish-content
        'unknown html "/tmp/")))
    (should
     (equal
      (nreverse calls)
      '((rewrite "answer")
        (rewrite "article")
        (cc "article:<p>Shared HTML</p>")
        (pin))))))

(ert-deftest zhihu-inline-images-degrade-to-alt-and-block-images-remain ()
  (let* ((html
          (concat
           "<p>甲<img src=\"inline.png\" alt=\"替代\">"
           "乙<img src=\"decorative.png\" alt=\"\">丙</p>"
           "<p> \n<img src=\"standalone.png\" alt=\"独占\">\n </p>"
           "<p><a href=\"https://example.com/\">"
           "<img src=\"linked.png\" alt=\"链接文字\"></a></p>"
           "<p>公式<span class=\"math inline\">\\(x\\)</span>继续</p>"
           "<blockquote><p><img src=\"nested.png\" alt=\"引用图\"></p>"
           "</blockquote>"
           "<img src=\"top.png\" alt=\"顶层文字\">"))
         (converted (zhihu--zhihuify-html html))
         (dom
          (zhihu--parse-html
           (concat "<html><body>" converted "</body></html>")))
         (body (car (dom-by-tag dom 'body)))
         (images (dom-by-tag body 'img))
         (ordinary (cl-remove-if #'zhihu--pin-equation-image-p images))
         (equations (cl-remove-if-not #'zhihu--pin-equation-image-p images))
         (anchor (car (dom-by-tag body 'a))))
    (should
     (equal
      (mapcar (lambda (image) (dom-attr image 'src)) ordinary)
      '("standalone.png" "nested.png")))
    (should (= (length equations) 1))
    (should (equal (dom-inner-text anchor) "链接文字"))
    (should (string-match-p "甲替代乙丙" (dom-inner-text body)))
    (should (string-match-p "公式继续" (dom-inner-text body)))
    (should (string-match-p "顶层文字" (dom-inner-text body)))))

(ert-deftest zhihu-new-answer-checkpoints-only-the-new-id ()
  (let (checkpoints)
    (cl-letf (((symbol-function 'zhihu--source-to-html)
               (lambda (_file) "<p>new</p>"))
              ((symbol-function 'zhihu--rewrite-img-srcs)
               (lambda (html _base-dir _source) html))
              ((symbol-function 'zhihu--checkpoint-meta)
               (lambda (_file meta)
                 (push (list (plist-get meta :question-id)
                             (plist-get meta :answer-id))
                       checkpoints)))
              ((symbol-function 'zhihu--publish-answer)
               (lambda (answer-id question-id html &rest args)
                 (should-not answer-id)
                 (should (equal question-id "123"))
                 (should (equal html "<p>new</p>"))
                 (should-not (plist-get args :creation-statement))
                 (should-not (plist-get args :reprint-permission))
                 (should-not (plist-get args :comment-permission))
                 "789")))
      (zhihu--publish-answer-file
       "/tmp/new-answer.md"
       (list :kind 'answer
             :question-id "123"
             :answer-id nil)))
    (setq checkpoints (nreverse checkpoints))
    (should (equal checkpoints '(("123" "789"))))))

(ert-deftest zhihu-pin-file-checkpoints-only-the-new-id ()
  (dolist (initial-id '(nil "456"))
    (let (checkpoints)
      (cl-letf
          (((symbol-function 'zhihu--source-to-html)
            (lambda (_file) "<p>new</p><p><img src=\"image.png\"></p>"))
           ((symbol-function 'zhihu--pin-topic-data)
            (lambda (topics)
              (should (equal topics '("Emacs")))
              [(:topic_id "1" :topic_name "#Emacs#")]))
           ((symbol-function 'zhihu--pin-content-from-html)
            (lambda (html base-dir)
              (should
               (equal html "<p>new</p><p><img src=\"image.png\"></p>"))
              (should (equal base-dir "/tmp/"))
              '(:html "<p>new</p>"
                      :text-length 3
                      :medias
                      [(:image
			(:height 1 :width 1
				 :url "https://picx.zhimg.com/uploaded.png"
				 :originalUrl
				 "https://picx.zhimg.com/uploaded.png"))])))
           ((symbol-function 'zhihu--checkpoint-meta)
            (lambda (_file meta)
              (push
               (list
                (and (plist-member meta :thought-id) t)
                (plist-get meta :thought-id))
               checkpoints)))
           ((symbol-function 'zhihu--publish-pin)
            (lambda (thought-id title content topics &rest options)
              (should (equal thought-id initial-id))
              (should (equal title "Title"))
              (should (= (plist-get content :text-length) 3))
              (should (= (length topics) 1))
              (should
               (equal
                (plist-get options :comment-permission)
                "follower_n_days"))
              (or thought-id "789")))
           ((symbol-function 'zhihu--rewrite-img-srcs)
            (lambda (&rest _args)
              (ert-fail "Pin must not use article/answer image rewriting")))
           ((symbol-function 'zhihu--append-article-cc-statement)
            (lambda (&rest _args)
              (ert-fail "Pin must not append the article CC statement"))))
        (should
         (equal
          (zhihu--publish-pin-file
           "/tmp/pin.md"
           (list :kind 'pin
                 :thought-id initial-id
                 :title "Title"
                 :topics '("Emacs")
                 :comment-permission "follower_n_days"))
          (or initial-id "789"))))
      (setq checkpoints (nreverse checkpoints))
      (should
       (equal
        (if initial-id
            nil
          '((t "789")))
        checkpoints)))))

(ert-deftest zhihu-article-banner-uploads-and-patches-draft ()
  (let* ((directory (make-temp-file "zhihu-test-banner-" t))
         (article-file (expand-file-name "article.md" directory))
         (banner-path "./images/banner.png")
         (banner-file (expand-file-name banner-path directory))
         (banner-url "https://picx.zhimg.com/banner.png"))
    (unwind-protect
        (progn
          (make-directory (file-name-directory banner-file))
          (with-temp-file banner-file
            (insert "banner bytes"))
          (let ((bytes (zhihu--read-file-bytes banner-file))
                (uploads 0)
                patch-body)
            (cl-letf
                (((symbol-function 'zhihu--source-to-html)
                  (lambda (_file) "<p>body</p>"))
                 ((symbol-function 'zhihu--rewrite-img-srcs)
                  (lambda (html _base-dir source)
                    (should (equal source "article"))
                    html))
                 ((symbol-function 'zhihu--upload-bytes)
                  (lambda (uploaded mime source)
                    (cl-incf uploads)
                    (should (equal uploaded bytes))
                    (should (equal mime "image/png"))
                    (should (equal source "article"))
                    banner-url))
                 ((symbol-function 'zhihu--checkpoint-meta)
                  (lambda (&rest _args)
                    (ert-fail "Existing published article state did not change")))
                 ((symbol-function 'zhihu--http-json)
                  (lambda (method _url &rest args)
                    (should (equal method "PATCH"))
                    (setq patch-body (plist-get args :body))
                    '(:status 200 :json nil :body "")))
                 ((symbol-function 'zhihu--sync-article-topics)
                  (lambda (state article-id topics)
                    (should (zhihu--xsrf-state-p state))
                    (should (equal article-id "456"))
                    (should-not topics)))
                 ((symbol-function 'zhihu--publish-article)
                  (lambda (article-id is-published &rest _args)
                    (should (equal article-id "456"))
                    (should is-published)
                    article-id)))
              (should
               (equal
                (zhihu--publish-article-file
                 (zhihu--make-xsrf-state "token")
                 article-file
                 (list :kind 'article
                       :title "Title"
                       :article-id "456"
                       :banner banner-path))
                "456")))
            (should (= uploads 1))
            (should
             (equal (plist-get patch-body :titleImage) banner-url))
            (should
             (eq
              (plist-get patch-body :isTitleImageFullScreen)
              :json-false))))
      (delete-directory directory t))))

(ert-deftest zhihu-article-banner-rejects-nonlocal-sources ()
  (dolist
      (banner
       '("https://example.com/banner.png"
         "//example.com/banner.png"
         "data:image/png;base64,Y292ZXI="))
    (cl-letf (((symbol-function 'zhihu--source-to-html)
               (lambda (_file) "<p>body</p>"))
              ((symbol-function 'zhihu--rewrite-img-srcs)
               (lambda (html _base-dir _source) html)))
      (should-error
       (zhihu--publish-article-file
        (zhihu--make-xsrf-state "token")
        "/tmp/article.md"
        (list :kind 'article
              :title "Title"
              :article-id "456"
              :banner banner))))))

(ert-deftest zhihu-article-one-command-publish-rewrites-links-after-id-known ()
  (dolist (initial-id '(nil "456"))
    (let* ((zhihu-article-cc-statement nil)
           (source-html
            (concat
             "<h2 id=\"start\">Start</h2>"
             "<p><a href=\"#finish\">Finish</a></p>"
             "<h3 id=\"finish\">Finish</h3>"))
           (final-id (or initial-id "789"))
           (xsrf-state (zhihu--make-xsrf-state "token"))
           created-content
           patched-content
           checkpoints
           events)
      (cl-letf
          (((symbol-function 'zhihu--source-to-html)
            (lambda (file)
              (should (equal file "/tmp/article.md"))
              source-html))
           ((symbol-function 'zhihu--rewrite-img-srcs)
            (lambda (html base-dir source)
              (should (equal html source-html))
              (should (equal base-dir "/tmp/"))
              (should (equal source "article"))
              html))
           ((symbol-function 'zhihu--create-article-draft)
            (lambda (state title html)
              (when initial-id
                (ert-fail "Existing article must not create a new draft"))
              (push 'create events)
              (should (eq state xsrf-state))
              (should (equal title "Title"))
              (setq created-content html)
              "789"))
           ((symbol-function 'zhihu--checkpoint-meta)
            (lambda (file meta)
              (should (equal file "/tmp/article.md"))
              (push (plist-get meta :article-id) checkpoints)))
           ((symbol-function 'zhihu--zhuanlan-mutation-request)
            (lambda (state method url referer &rest args)
              (push 'patch events)
              (should (eq state xsrf-state))
              (should (equal method "PATCH"))
              (should
               (equal
                url
                (format
                 "https://zhuanlan.zhihu.com/api/articles/%s/draft"
                 final-id)))
              (should
               (equal
                referer
                (format
                 "https://zhuanlan.zhihu.com/p/%s/edit"
                 final-id)))
              (let ((body (plist-get args :body)))
                (setq patched-content (plist-get body :content))
                (should (eq (plist-get body :table_of_contents) t)))
              '(:status 200 :json nil :body "")))
           ((symbol-function 'zhihu--sync-article-topics)
            (lambda (state article-id topics)
              (push 'sync events)
              (should (eq state xsrf-state))
              (should (equal article-id final-id))
              (should-not topics)))
           ((symbol-function 'zhihu--publish-article)
            (lambda (article-id is-published &rest options)
              (push 'publish events)
              (should (equal article-id final-id))
              (should (eq is-published (and initial-id t)))
              (should (eq (plist-get options :xsrf-state) xsrf-state))
              (should (eq
                       (plist-get options :toc)
                       t))
              article-id)))
        (should
         (equal
          (zhihu--publish-article-file
           xsrf-state
           "/tmp/article.md"
           (list :kind 'article
                 :title "Title"
                 :article-id initial-id
                 :toc t))
          final-id)))
      (when created-content
        (should
         (equal
          (dom-attr (zhihu-test--only-anchor created-content) 'href)
          "#finish")))
      (should
       (equal
        (dom-attr (zhihu-test--only-anchor patched-content) 'href)
        (format "#h_%s_1" final-id)))
      (should
       (equal
        (nreverse events)
        (if initial-id
            '(patch sync publish)
          '(create patch sync publish))))
      (should
       (equal
        (nreverse checkpoints)
        (if initial-id
            nil
          '("789")))))))

(ert-deftest zhihu-article-section-link-preflight-precedes-all-side-effects ()
  (let ((source-html
         (concat
          "<h2 id=\"target\">Target</h2>"
          "<p><a href=\"#target\">Target</a>"
          "<img src=\"body.png\"></p>")))
    (cl-letf
        (((symbol-function 'zhihu--source-to-html)
          (lambda (_file) source-html))
         ((symbol-function 'zhihu--rewrite-img-srcs)
          (lambda (&rest _args)
            (ert-fail "Body image upload must follow section-link preflight")))
         ((symbol-function 'zhihu--img-bytes-and-mime)
          (lambda (&rest _args)
            (ert-fail "Banner read must follow section-link preflight")))
         ((symbol-function 'zhihu--upload-bytes)
          (lambda (&rest _args)
            (ert-fail "Banner upload must follow section-link preflight")))
         ((symbol-function 'zhihu--create-article-draft)
          (lambda (&rest _args)
            (ert-fail "Draft creation must follow section-link preflight")))
         ((symbol-function 'zhihu--zhuanlan-mutation-request)
          (lambda (&rest _args)
            (ert-fail "Draft PATCH must follow section-link preflight")))
         ((symbol-function 'zhihu--sync-article-topics)
          (lambda (&rest _args)
            (ert-fail "Topic sync must follow section-link preflight")))
         ((symbol-function 'zhihu--publish-article)
          (lambda (&rest _args)
            (ert-fail "Publish must follow section-link preflight")))
         ((symbol-function 'zhihu--checkpoint-meta)
          (lambda (&rest _args)
            (ert-fail "Metadata writes must follow section-link preflight")))
         ((symbol-function 'zhihu--http-json)
          (lambda (&rest _args)
            (ert-fail "HTTP must follow section-link preflight")))
         ((symbol-function 'zhihu--http)
          (lambda (&rest _args)
            (ert-fail "HTTP must follow section-link preflight"))))
      (let ((err
             (should-error
              (zhihu--publish-article-file
               (zhihu--make-xsrf-state "token")
               "/tmp/article.md"
               (list :kind 'article
                     :title "Title"
                     :article-id nil
                     :banner "./cover.png"
                     :toc nil))
              :type 'user-error)))
        (should
         (string-match-p
          (regexp-quote "目录")
          (error-message-string err)))))))

(ert-deftest zhihu-article-checkpoints-required-state-transitions ()
  (dolist (case
	   '((:article-id "456"
			  :toc nil
			  :checkpoints nil)
	     (:article-id nil
			  :toc t
			  :checkpoints ((t "789")))))
    (let* ((zhihu-article-cc-statement 'by-nc-sa)
           (expected-html
            (zhihu--append-article-cc-statement "<p>new</p>"))
	   (initial-id (plist-get case :article-id))
           (toc
            (plist-get case :toc))
           (expected-checkpoints (plist-get case :checkpoints))
           (final-id (or initial-id "789"))
           (topics '("A" "B"))
           (xsrf-state (zhihu--make-xsrf-state "token"))
           (draft-creations 0)
           (patch-requests 0)
           events
           checkpoints)
      (cl-letf
          (((symbol-function 'zhihu--source-to-html)
            (lambda (_file) "<p>new</p>"))
           ((symbol-function 'zhihu--rewrite-img-srcs)
            (lambda (html _base-dir source)
              (should (equal source "article"))
              html))
           ((symbol-function 'zhihu--checkpoint-meta)
            (lambda (_file meta)
              (should
               (equal (plist-get meta :creation-statement) "contain_finance"))
              (should
               (equal (plist-get meta :content-source) "printMedia"))
              (should
               (eq (plist-get meta :toc)
                   toc))
              (should
               (equal
                (plist-get meta :reprint-permission) "need_payment"))
              (should
               (equal (plist-get meta :comment-permission) "nobody"))
	      (push (list (and (plist-member meta :article-id) t)
			  (plist-get meta :article-id))
                    checkpoints)))
           ((symbol-function 'zhihu--create-article-draft)
            (lambda (state title html)
              (should (eq state xsrf-state))
              (should (equal title "Title"))
              (should (equal html expected-html))
              (cl-incf draft-creations)
              "789"))
           ((symbol-function 'zhihu--zhuanlan-mutation-request)
            (lambda (state method url referer &rest args)
              (push 'patch events)
              (cl-incf patch-requests)
              (should (eq state xsrf-state))
              (should (equal method "PATCH"))
              (should
               (equal
                url
                (format
                 "https://zhuanlan.zhihu.com/api/articles/%s/draft"
                 final-id)))
              (should
               (equal
                referer
                (format
                 "https://zhuanlan.zhihu.com/p/%s/edit"
                 final-id)))
              (let ((body (plist-get args :body)))
                (should (equal (plist-get body :title) "Title"))
                (should (equal (plist-get body :content) expected-html))
                (should (plist-member body :titleImage))
                (should (equal (plist-get body :titleImage) ""))
                (should (plist-member body :isTitleImageFullScreen))
                (should
                 (eq (plist-get body :isTitleImageFullScreen) :json-false))
                (should
                 (eq
                  (plist-get body :table_of_contents)
                  (if toc t :json-false))))
              '(:status 200 :json nil :body "")))
           ((symbol-function 'zhihu--sync-article-topics)
            (lambda (state article-id actual-topics)
              (push 'sync events)
              (should (eq state xsrf-state))
              (should (equal article-id final-id))
              (should (equal actual-topics topics))))
           ((symbol-function 'zhihu--publish-article)
            (lambda (article-id is-published &rest args)
              (push 'publish events)
              (should (eq (plist-get args :xsrf-state) xsrf-state))
              (should (equal article-id final-id))
              (should (eq is-published (and initial-id t)))
              (should
               (equal (plist-get args :creation-statement) "contain_finance"))
              (should
               (equal (plist-get args :content-source) "printMedia"))
              (should
               (eq (plist-get args :toc)
                   toc))
              (should
               (equal
                (plist-get args :reprint-permission) "need_payment"))
              (should
               (equal (plist-get args :comment-permission) "nobody"))
              article-id)))
	(let ((meta
	       (list :kind 'article
		     :title "Title"
		     :creation-statement "contain_finance"
		     :content-source "printMedia"
		     :toc toc
		     :reprint-permission "need_payment"
		     :comment-permission "nobody"
		     :topics topics)))
	  (setq meta (plist-put meta :article-id initial-id))
	  (zhihu--publish-article-file
	   xsrf-state "/tmp/article.md" meta)))
      (should (= draft-creations (if initial-id 0 1)))
      (should (= patch-requests 1))
      (should (equal (nreverse events) '(patch sync publish)))
      (setq checkpoints (nreverse checkpoints))
      (should
       (equal
	checkpoints
        expected-checkpoints)))))

(ert-deftest zhihu-article-column-errors-retain-column-without-checkpoint ()
  (dolist (phase '(query add))
    (let ((state (zhihu--make-xsrf-state "token"))
          published
          add-called)
      (cl-letf
          (((symbol-function 'zhihu--source-to-html)
            (lambda (_file) "<p>body</p>"))
           ((symbol-function 'zhihu--rewrite-img-srcs)
            (lambda (html &rest _args) html))
           ((symbol-function 'zhihu--checkpoint-meta)
            (lambda (&rest _args)
              (ert-fail "Existing article must not checkpoint metadata")))
           ((symbol-function 'zhihu--zhuanlan-mutation-request)
            (lambda (actual-state method _url _referer &rest _args)
              (should (eq actual-state state))
              (should (equal method "PATCH"))
              '(:status 200 :json nil :body "")))
           ((symbol-function 'zhihu--sync-article-topics)
            (lambda (&rest _args) nil))
           ((symbol-function 'zhihu--publish-article)
            (lambda (article-id _is-published &rest _args)
              (setq published t)
              article-id))
           ((symbol-function 'zhihu--article-in-column-p)
            (lambda (&rest _args)
              (if (eq phase 'query)
                  (error "查询文章专栏失败：query failed")
                nil)))
           ((symbol-function 'zhihu--add-article-to-column)
            (lambda (&rest _args)
              (setq add-called t)
              (error "收录进专栏 writers 失败：add failed"))))
        (let ((message
               (condition-case err
                   (progn
                     (zhihu--publish-article-file
                      state
                      "/tmp/article.md"
                      (list
                       :kind 'article
                       :title "Title"
                       :article-id "456"
                       :column-id "writers"))
                     nil)
                 (error (error-message-string err)))))
          (should message)
          (should
           (string-match-p
            (regexp-quote
             "文章已发布为 p/456，但处理专栏 writers 失败")
            message))
          (should
           (string-match-p
            (regexp-quote
             (if (eq phase 'query)
                 "查询文章专栏失败"
               "收录进专栏 writers 失败"))
            message))
          (should
           (string-match-p
            (regexp-quote
             "column-id 已保留，再次发布即可重试")
            message))))
      (should published)
      (should (eq (not (null add-called)) (eq phase 'add))))))

(ert-deftest zhihu-article-topic-sync-failure-stops-publish ()
  (let ((published nil))
    (cl-letf
        (((symbol-function 'zhihu--source-to-html)
          (lambda (_file) "<p>body</p>"))
         ((symbol-function 'zhihu--rewrite-img-srcs)
          (lambda (html &rest _args) html))
         ((symbol-function 'zhihu--http-json)
          (lambda (method _url &rest _args)
            (should (equal method "PATCH"))
            '(:status 200 :json nil :body "")))
         ((symbol-function 'zhihu--sync-article-topics)
          (lambda (&rest _args) (error "topic sync failed")))
         ((symbol-function 'zhihu--publish-article)
          (lambda (&rest _args) (setq published t))))
      (should-error
       (zhihu--publish-article-file
        (zhihu--make-xsrf-state "token")
        "/tmp/article.md"
        (list :kind 'article
              :title "Title"
              :article-id "456"
              :topics nil))))
    (should-not published)))

(provide 'zhihu-test)
;;; zhihu-test.el ends here
