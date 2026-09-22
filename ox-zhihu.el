;;; ox-zhihu.el --- Org export backend for Zhihu  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Dzming Li

;; Author: Dzming Li <i@dzming.li>
;; Maintainer: Dzming Li <i@dzming.li>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: outlines, hypermedia
;; URL: https://github.com/DzmingLi/zhihu.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 把 Org 文档导出成知乎方言 HTML。本层只做纯导出：不读 cookie、不访问
;; 网络、不上传图片。发布由 `zhihu.el' 在导出结果之上实现。
;;
;; 主要入口：
;;   - Org 导出后端 `zhihu'，用于 `org-export-as'/`org-export-to-file'；
;;   - `org-zhihu-export-as-html' 与 `org-zhihu-export-to-html'；
;;   - `zhihu--org->html'：字符串进、知乎方言 HTML fragment 出。
;;
;; 可选使用 `org-typst-math' 处理 `#+TYPST_MATH: t' 文档里的 Typst 数学。
;; 知乎的 equation 接口只接受 TeX，因此这里先把 Typst 数学转成 LaTeX。

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-util)
(require 'dom)
(require 'xml)
(require 'ox-html)

(declare-function org-element-map "org-element" (data types fun &rest args))
(declare-function org-element-property
                  "org-element" (property node &optional dflt force-undefer))
(declare-function org-element-put-property
                  "org-element" (node property value))
(declare-function org-typst-math-fragment "org-typst-math" (element))
(declare-function typst-client-convert
                  "typst-client" (directory preamble items target))


;;;; HTML conversion

;; External process execution

(defun zhihu--shell-convert (program args input)
  "对 INPUT 跑 PROGRAM ARGS，返回 stdout。非零 exit 抛错。"
  ;; `call-process-region' 的 STDERR-FILE 只能是文件名/t/nil，不能是 buffer。
  ;; 单独捕获 stderr 可以避免子进程的 warning 污染 stdout。
  (let ((stderr-file (make-temp-file "zhihu-stderr-"))
        (stdout-buffer (generate-new-buffer " *zhihu-stdout*")))
    (unwind-protect
        (with-temp-buffer
          (insert input)
          (let ((coding-system-for-write 'utf-8)
                (coding-system-for-read 'utf-8)
                (rc (apply #'call-process-region (point-min) (point-max) program
                           nil (list stdout-buffer stderr-file) nil args)))
            (if (eq rc 0)
                (with-current-buffer stdout-buffer (buffer-string))
              (let ((err (with-temp-buffer
                           (insert-file-contents stderr-file)
                           (string-trim (buffer-string)))))
                (error "zhihu: %s 退出 %s%s"
                       program rc
                       (if (string-empty-p err) "" (concat ": " err)))))))
      (when (buffer-live-p stdout-buffer) (kill-buffer stdout-buffer))
      (ignore-errors (delete-file stderr-file)))))

;; HTML parsing and Zhihu dialect
;;
;; 仅用于把本地文章生成的 HTML 规范化为知乎接受的节点结构。

(defun zhihu--parse-html (html)
  "把 HTML 字符串过 libxml-parse-html-region。返回 dom 节点。"
  (with-temp-buffer
    (insert html)
    (libxml-parse-html-region (point-min) (point-max))))

(defun zhihu--node-has-class-p (node cls)
  "NODE 的 class 属性是否（按空格分割后）包含 CLS。
比 `dom-by-class' 严格——后者是子串匹配，会误中 `RichText-foo'。"
  (let ((c (dom-attr node 'class)))
    (and c (member cls (split-string c "[ \t\n]+" t)))))

(defun zhihu--inner-html (node)
  "把 NODE 的子节点序列化为 HTML 字符串（不含 NODE 自己那层包裹标签）。
这是要发给知乎服务端的实际正文格式。"
  (with-temp-buffer
    (dolist (child (dom-children node))
      (cond
       ((stringp child) (insert (xml-escape-string child)))
       ((consp child)   (dom-print child nil nil))))
    (buffer-string)))

(defun zhihu--outer-html (node)
  "把单个 DOM NODE 连同自身序列化为 HTML 字符串。"
  (with-temp-buffer
    (if (stringp node)
        (insert (xml-escape-string node))
      (dom-print node nil nil))
    (buffer-string)))

(defconst zhihu--document-reference-roles
  '("doc-biblioref" "doc-noteref" "doc-backlink"
    "doc-bibliography" "doc-endnotes" "doc-footnote")
  "不应当作文章章节链接处理的 HTML 文档引用 role。")

(defun zhihu--document-reference-role-p (role)
  "ROLE 是否包含不应改写的文档引用语义。"
  (and
   (stringp role)
   (cl-some
    (lambda (token)
      (member token zhihu--document-reference-roles))
    (split-string role "[[:space:]]+" t))))

(defun zhihu--decode-fragment-id (fragment)
  "把 URL 中的 FRAGMENT 解码为 UTF-8 HTML id。
无法解码时返回原字符串，让调用方给出找不到目标的错误。"
  (condition-case nil
      (decode-coding-string (url-unhex-string fragment) 'utf-8 t)
    (error fragment)))

(defun zhihu--article-section-link-targets (html toc)
  "分析 HTML 中需要改写的文章章节链接。
返回由原始 `href' 到知乎目录标题序号组成的 alist。目录序号按所有 `h2'
和 `h3' 的文档顺序从零计算，没有 id 的标题也占序号。

只有纯 fragment 链接会被处理；脚注、参考文献等标准文档引用会被忽略。
存在章节链接时，TOC 必须非 nil，且每个目标必须唯一
指向 `h2' 或 `h3'。"
  (let* ((dom
          (zhihu--parse-html
           (concat "<html><body>" html "</body></html>")))
         (body (car (dom-by-tag dom 'body)))
         (headings (make-hash-table :test #'equal))
         (all-ids (make-hash-table :test #'equal))
         (heading-index 0)
         links)
    (cl-labels
        ((walk
          (node in-document-reference)
          (when (consp node)
            (let* ((tag (dom-tag node))
                   (role (dom-attr node 'role))
                   (document-reference
                    (or in-document-reference
                        (zhihu--document-reference-role-p role)))
                   (identifier (dom-attr node 'id)))
              (when
                  (and (stringp identifier)
                       (not (string-empty-p identifier)))
                (puthash
                 identifier
                 (1+ (gethash identifier all-ids 0))
                 all-ids))
              (when (memq tag '(h2 h3))
                (when
                    (and (stringp identifier)
                         (not (string-empty-p identifier)))
                  (puthash
                   identifier
                   (cons heading-index (gethash identifier headings))
                   headings))
                (cl-incf heading-index))
              (when (and (eq tag 'a) (not document-reference))
                (let ((href (dom-attr node 'href)))
                  (when
                      (and (stringp href)
                           (string-match-p
                            "\\`#[^#[:space:]]+\\'" href))
                    (push href links))))
              (dolist (child (dom-children node))
                (walk child document-reference))))))
      (walk body nil))
    (setq links (nreverse links))
    (when (and links (not toc))
      (user-error
       (concat
        "zhihu: 文章内标题链接依赖原生目录；"
        "请启用当前源格式的目录请求")))
    (let (targets)
      (dolist (href links)
        (let* ((raw (substring href 1))
               (decoded (zhihu--decode-fragment-id raw))
               ;; URL fragment 按浏览器语义先解码；原样 id 是无需解码或
               ;; 遇到异常导出结果时的 fallback。
               (identifier
                (cond
                 ((gethash decoded all-ids) decoded)
                 ((gethash raw all-ids) raw)
                 (t decoded)))
               (indices (gethash identifier headings))
               (id-count (gethash identifier all-ids)))
          (cond
           ((null id-count)
            (user-error
             "zhihu: 文章内链接 %s 找不到目标 id=%s"
             href identifier))
           ((cdr indices)
            (user-error
             "zhihu: 文章内链接 %s 的标题 id 重复"
             href))
           ((> id-count 1)
            (user-error
             "zhihu: 文章内链接 %s 的目标 id 重复"
             href))
           ((null indices)
            (user-error
             "zhihu: 文章内链接 %s 的目标不是 h2/h3 标题"
             href)))
          (unless (assoc href targets)
            (push (cons href (car indices)) targets))))
      (nreverse targets))))

(defun zhihu--rewrite-article-section-links (html article-id targets)
  "按 TARGETS 把 HTML 中的章节链接改写为 ARTICLE-ID 的知乎目录链接。
TARGETS 是 `zhihu--article-section-link-targets' 返回的 alist。"
  (if (null targets)
      html
    (setq article-id (zhihu--value-string article-id))
    (unless
        (and article-id
             (string-match-p "\\`[0-9]+\\'" article-id))
      (error "zhihu: 无效的 article-id：%s" article-id))
    (let* ((dom
            (zhihu--parse-html
             (concat "<html><body>" html "</body></html>")))
           (body (car (dom-by-tag dom 'body)))
           (seen (make-hash-table :test #'equal)))
      (cl-labels
          ((walk
            (node in-document-reference)
            (when (consp node)
              (let* ((role (dom-attr node 'role))
                     (document-reference
                      (or in-document-reference
                          (zhihu--document-reference-role-p role))))
                (when
                    (and (eq (dom-tag node) 'a)
                         (not document-reference))
                  (let* ((href (dom-attr node 'href))
                         (target
                          (and (stringp href) (assoc href targets))))
                    (when target
                      (dom-set-attribute
                       node 'href
                       (format "#h_%s_%d" article-id (cdr target)))
                      (puthash href t seen))))
                (dolist (child (dom-children node))
                  (walk child document-reference))))))
        (walk body nil))
      (dolist (target targets)
        (unless (gethash (car target) seen)
          (error
           "zhihu: 打包后的正文缺少已校验章节链接 %s"
           (car target))))
      (zhihu--inner-html body))))

(defun zhihu--normalize-reference-text (text)
  "把原生引用说明 TEXT 归一化为单行纯文本。"
  (string-trim
   (replace-regexp-in-string "[[:space:]]+" " " text)))

(defun zhihu--web-url-p (value)
  "VALUE 是否是含 host、且不含空白或控制字符的绝对 HTTP(S) URL。"
  (and
   (stringp value)
   (not (string-match-p "[[:space:][:cntrl:]]" value))
   (condition-case nil
       (let* ((parsed (url-generic-parse-url value))
              (scheme (url-type parsed))
              (host (url-host parsed)))
         (and (member (downcase (or scheme "")) '("http" "https"))
              (stringp host)
              (not (string-empty-p host))))
     (error nil))))

(defun zhihu--reference-data (text url context &optional canonical)
  "校验并归一化引用 TEXT/URL，错误信息使用 CONTEXT。
CANONICAL 非 nil 时要求 TEXT 已经是规范形式。"
  (let ((normalized
         (and (stringp text) (zhihu--normalize-reference-text text))))
    (unless
        (and normalized
             (not (string-empty-p normalized))
             (not (string-match-p "[[:cntrl:]]" normalized))
             (or (not canonical) (equal text normalized)))
      (error "zhihu: %s文字必须是非空的规范纯文本" context))
    (unless
        (and (stringp url)
             (or (string-empty-p url) (zhihu--web-url-p url)))
      (error "zhihu: %s URL 必须为空或为含 host 的 HTTP(S) URL" context))
    (list :text normalized :url url)))

(defun zhihu--dom-nodes-with-attribute (dom attribute value)
  "返回 DOM 中 ATTRIBUTE 等于 VALUE 的所有元素。"
  (dom-search
   dom
   (lambda (node)
     (and (consp node)
          (equal (dom-attr node attribute) value)))))

(defun zhihu--dom-significant-children (node)
  "返回 NODE 的子节点，忽略只含空白的文本节点。"
  (cl-remove-if
   (lambda (child)
     (and (stringp child) (string-empty-p (string-trim child))))
   (dom-children node)))

(defun zhihu--dom-trim-blank-children (node)
  "返回 NODE 去掉首尾空白文本后的子节点，并保留内部空白。"
  (let ((children (copy-sequence (dom-children node))))
    (while
        (and (stringp (car children))
             (string-empty-p (string-trim (car children))))
      (setq children (cdr children)))
    (setq children (nreverse children))
    (while
        (and (stringp (car children))
             (string-empty-p (string-trim (car children))))
      (setq children (cdr children)))
    (nreverse children)))

(defun zhihu--single-fragment-link-target (node context)
  "返回 NODE 内唯一链接指向的 fragment ID；错误信息使用 CONTEXT。"
  (let ((children (zhihu--dom-significant-children node)))
    (unless
        (and (= (length children) 1)
             (consp (car children))
             (eq (dom-tag (car children)) 'a))
      (error "zhihu: %s必须只包含一个链接" context))
    (let ((href (dom-attr (car children) 'href)))
      (unless
          (and (stringp href)
               (string-match "\\`#\\([^#[:space:]]+\\)\\'" href))
        (error "zhihu: %s缺少有效的 fragment 链接" context))
      (match-string 1 href))))

(defun zhihu--reference-marker (data numero)
  "从已校验的引用 DATA 和正整数 NUMERO 构造内部 marker。"
  `(span
    ((data-zhihu-reference . "true")
     (data-text . ,(plist-get data :text))
     (data-url . ,(plist-get data :url))
     (data-numero . ,(number-to-string numero)))
    ,(format "[%d]" numero)))

(defun zhihu--org-footnote-reference-data (definition)
  "从 ox-html 脚注 DEFINITION 提取知乎引用的 (:text ... :url ...)。"
  (let* ((children (zhihu--dom-trim-blank-children definition))
         (number (car children))
         (footpara (cadr children)))
    (unless
        (and
         (= (length children) 2)
         (consp number)
         (eq (dom-tag number) 'sup)
         (consp footpara)
         (eq (dom-tag footpara) 'div)
         (zhihu--node-has-class-p footpara "footpara")
         (equal (dom-attr footpara 'role) "doc-footnote"))
      (error "zhihu: Org 脚注定义结构不受支持"))
    (let* ((number-link
            (car
             (cl-remove-if-not
              (lambda (node)
                (and
                 (consp node)
                 (eq (dom-tag node) 'a)
                 (zhihu--node-has-class-p node "footnum")))
              (zhihu--dom-significant-children number))))
           (paragraphs (zhihu--dom-trim-blank-children footpara)))
      (unless
          (and
           number-link
           (= (length paragraphs) 1)
           (consp (car paragraphs))
           (eq (dom-tag (car paragraphs)) 'p))
        (error "zhihu: Org 脚注必须只包含一个段落"))
      (let ((nodes
             (zhihu--dom-trim-blank-children (car paragraphs)))
            (link-count 0)
            (url ""))
        (cl-labels
            ((validate
              (node)
              (cond
               ((stringp node) nil)
               ((not (consp node))
                (error "zhihu: Org 脚注包含无法识别的节点"))
               ((eq (dom-tag node) 'a)
                (cl-incf link-count)
                (when (> link-count 1)
                  (error "zhihu: 一条 Org 脚注最多只能包含一个链接"))
                (setq url (dom-attr node 'href))
                (mapc #'validate (dom-children node)))
               ((memq (dom-tag node)
                      '(em strong b i s strike del code span small sub sup mark))
                (mapc #'validate (dom-children node)))
               (t
                (error "zhihu: Org 脚注不支持 %s 节点"
                       (dom-tag node))))))
          (mapc #'validate nodes))
        (zhihu--reference-data
         (mapconcat
          (lambda (node)
            (if (stringp node) node (dom-inner-text node)))
          nodes "")
         url "Org 脚注")))))

(defun zhihu--org-footnote-definition-id (definition)
  "返回 ox-html 脚注 DEFINITION 的 fragment ID。"
  (let* ((number (car (zhihu--dom-trim-blank-children definition)))
         (links
          (and
           (consp number)
           (cl-remove-if-not
            (lambda (node)
              (and
               (consp node)
               (eq (dom-tag node) 'a)
               (zhihu--node-has-class-p node "footnum")))
            (zhihu--dom-significant-children number))))
         (id (and (= (length links) 1) (dom-attr (car links) 'id))))
    (unless (and (stringp id) (not (string-empty-p id)))
      (error "zhihu: Org 脚注定义缺少唯一 footnum id"))
    id))

(defun zhihu--org-footnote-reference-target (node)
  "若 NODE 是 ox-html 脚注引用 SUP，则返回 definition fragment ID。"
  (when (and (consp node) (eq (dom-tag node) 'sup))
    (let ((children (zhihu--dom-significant-children node)))
      (when
          (and
           (= (length children) 1)
           (consp (car children))
           (eq (dom-tag (car children)) 'a)
           (zhihu--node-has-class-p (car children) "footref"))
        (let ((href (dom-attr (car children) 'href)))
          (unless
              (and
               (stringp href)
               (string-match "\\`#\\([^#[:space:]]+\\)\\'" href))
            (error "zhihu: Org 脚注引用缺少有效的 fragment 链接"))
          (match-string 1 href))))))

(defun zhihu--org-rewrite-footnotes (html)
  "把 ox-html 脚注改写为共用 HTML 层可识别的知乎引用 marker。"
  (let* ((dom
          (zhihu--parse-html (concat "<html><body>" html "</body></html>")))
         (body (car (dom-by-tag dom 'body)))
         (sentinels
          (dom-search
           body
           (lambda (node)
             (and (consp node)
                  (assq 'data-zhihu-reference (cadr node))))))
         (sections
          (dom-search
           body
           (lambda (node)
             (and
              (consp node)
              (eq (dom-tag node) 'div)
              (equal (dom-attr node 'id) "footnotes")))))
         (references
          (dom-search body #'zhihu--org-footnote-reference-target)))
    (when sentinels
      (error "zhihu: Org HTML 不能预先包含内部引用 marker"))
    (if (and (null sections) (null references))
        html
      (unless (and (= (length sections) 1) references)
        (error "zhihu: Org 脚注引用与定义结构不完整"))
      (let* ((section (car sections))
             (definitions (make-hash-table :test 'equal))
             (reference-data (make-hash-table :test 'equal))
             (numbers (make-hash-table :test 'equal))
             (next-number 0))
        (dolist
            (definition
             (dom-search
              section
              (lambda (node)
                (and
                 (consp node)
                 (eq (dom-tag node) 'div)
                 (zhihu--node-has-class-p node "footdef")))))
          (let ((id (zhihu--org-footnote-definition-id definition)))
            (when (gethash id definitions)
              (error "zhihu: Org 脚注定义 id 重复：%s" id))
            (puthash id definition definitions)))
        (cl-labels
            ((rewrite
              (node)
              (cond
               ((stringp node) node)
               ((not (consp node)) node)
               ((eq node section) nil)
               ((zhihu--org-footnote-reference-target node)
                (let* ((target (zhihu--org-footnote-reference-target node))
                       (definition (gethash target definitions)))
                  (unless definition
                    (error "zhihu: Org 脚注引用缺少定义：%s" target))
                  (let ((data
                         (or
                          (gethash target reference-data)
                          (puthash
                           target
                           (zhihu--org-footnote-reference-data definition)
                           reference-data)))
                        (numero
                         (or
                          (gethash target numbers)
                          (puthash target (cl-incf next-number) numbers))))
                    (zhihu--reference-marker data numero))))
               (t
                (cons
                 (dom-tag node)
                 (cons
                  (copy-tree (cadr node))
                  (delq nil (mapcar #'rewrite (dom-children node)))))))))
          (zhihu--inner-html
           `(body nil ,@(delq nil (mapcar #'rewrite (dom-children body))))))))))

(defun zhihu--org-html-latex-fragment-filter (text _backend _info)
  "把 ox-html 的 LaTeX fragment TEXT 包装成通用 math span。"
  (let* ((leading
          (and (string-match "\\`[ \t\r\n]+" text) (match-string 0 text)))
         (trailing
          (and (string-match "[ \t\r\n]+\\'" text) (match-string 0 text)))
         (formula (string-trim text)))
    (format
     "%s<span class=\"math %s\">%s</span>%s"
     (or leading "")
     (if (string-prefix-p "\\[" formula) "display" "inline")
     (org-html-encode-plain-text formula)
     (or trailing ""))))

(defun zhihu--org-html-latex-environment-filter (text _backend _info)
  "把 ox-html 的 LaTeX environment TEXT 包装成通用 display math span。"
  (format
   "<p><span class=\"math display\">%s</span></p>"
   (org-html-encode-plain-text text)))

(defun zhihu--org-structural-container-p (node)
  "若 NODE 只是 ox-html 添加的结构包装，则返回非 nil。"
  (and
   (consp node)
   (eq (dom-tag node) 'div)
   (cl-some
    (lambda (class)
      (or
       (equal class "org-src-container")
       (string-prefix-p "outline-" class)
       (string-prefix-p "outline-text-" class)))
    (split-string (or (dom-attr node 'class) "") "[ \t\r\n]+" t))))

(defun zhihu--org-normalize-node (node)
  "去掉 ox-html NODE 中的结构包装，返回零个或多个节点。"
  (cond
   ((stringp node) (list node))
   ((not (consp node)) nil)
   ((zhihu--org-structural-container-p node)
    (cl-mapcan #'zhihu--org-normalize-node (dom-children node)))
   (t
    (let ((children
           (cl-mapcan #'zhihu--org-normalize-node (dom-children node))))
      (when (eq (dom-tag node) 'p)
        (while
            (and
             (stringp (car children))
             (string-empty-p (string-trim (car children))))
          (setq children (cdr children)))
        (when (stringp (car children))
          (setcar children (string-trim-left (car children))))
        (while
            (and
             children
             (stringp (car (last children)))
             (string-empty-p (string-trim (car (last children)))))
          (setq children (butlast children)))
        (when-let* ((tail (last children))
                    (text (and (stringp (car tail)) (car tail))))
          (setcar tail (string-trim-right text))))
      (list
       (cons
        (dom-tag node)
        (cons (copy-tree (dom-attributes node)) children)))))))

(defun zhihu--org-normalize-structure (html)
  "从 HTML 中移除只供 ox-html 页面布局使用的包装元素。"
  (let* ((dom
          (zhihu--parse-html (concat "<html><body>" html "</body></html>")))
         (body (car (dom-by-tag dom 'body))))
    (zhihu--inner-html
     `(body nil
            ,@(cl-mapcan #'zhihu--org-normalize-node
                         (dom-children body))))))

(defun zhihu--math-span-p (node)
  "NODE 是否是通用 math span。"
  (and (consp node)
       (eq (dom-tag node) 'span)
       (zhihu--node-has-class-p node "math")))

(defun zhihu--math-span-tex (node)
  "从 math span NODE 取出 TeX，去掉 \\(…\\) / \\[…\\]。"
  (let ((text (string-trim (dom-inner-text node))))
    (cond
     ((and (string-prefix-p "\\(" text) (string-suffix-p "\\)" text))
      (substring text 2 -2))
     ((and (string-prefix-p "\\[" text) (string-suffix-p "\\]" text))
      (substring text 2 -2))
     (t text))))

(defun zhihu--reference-marker-p (node)
  "NODE 是否是内部的知乎原生引用 marker。"
  (and (consp node)
       (eq (dom-tag node) 'span)
       (equal (dom-attr node 'data-zhihu-reference) "true")))

(defun zhihu--native-reference-node (marker)
  "严格校验 MARKER，并返回知乎原生 reference `sup' 节点。"
  (let* ((attrs (cadr marker))
         (expected
         '(data-zhihu-reference data-text data-url data-numero))
         (names (mapcar #'car attrs))
         (text (dom-attr marker 'data-text))
         (url (dom-attr marker 'data-url))
         (numero (dom-attr marker 'data-numero)))
    (unless
        (and (= (length names) (length expected))
             (cl-every (lambda (name) (memq name expected)) names)
             (cl-every (lambda (name) (assq name attrs)) expected))
      (error "zhihu: 原生引用 marker 含无效属性"))
    (zhihu--reference-data text url "原生引用" t)
    (unless
        (and (stringp numero)
             (string-match-p "\\`[1-9][0-9]*\\'" numero))
      (error "zhihu: 原生引用 numero 必须是正十进制整数"))
    (unless
        (equal (dom-children marker) (list (format "[%s]" numero)))
      (error "zhihu: 原生引用 marker 的显示文字与 numero 不一致"))
    `(sup
      ((data-text . ,text)
       (data-url . ,url)
       (data-draft-node . "inline")
       (data-draft-type . "reference")
       (data-numero . ,numero))
      ,(format "[%s]" numero))))

(defun zhihu--code-language (pre code)
  "从 PRE/CODE 的属性中提取代码语言。"
  (let ((classes
         (append
          (split-string (or (dom-attr pre 'class) "") "[ \t\n]+" t)
          (split-string (or (dom-attr code 'class) "") "[ \t\n]+" t))))
    (or (dom-attr pre 'lang)
        (dom-attr pre 'data-lang)
        (dom-attr code 'data-lang)
        (cl-loop for cls in classes
                 when (string-prefix-p "language-" cls)
                 return (substring cls (length "language-")))
        (cl-loop for cls in classes
                 when (string-prefix-p "src-" cls)
                 return (substring cls (length "src-")))
        (cl-loop for cls in classes
                 unless (member cls '("sourceCode" "code" "src")) return cls)
        "")))

(defun zhihu--captioned-figure-parts (node)
  "若 NODE 是单图图注 figure，返回 (IMAGE . FIGCAPTION)，否则返回 nil。
只识别忽略空白后恰好含一个直接 `img' 和一个直接 `figcaption' 的结构。"
  (when (and (consp node) (eq (dom-tag node) 'figure))
    (let ((children (zhihu--dom-significant-children node))
          image
          caption
          invalid)
      (dolist (child children)
        (cond
         ((and (consp child) (eq (dom-tag child) 'img) (not image))
          (setq image child))
         ((and (consp child) (eq (dom-tag child) 'figcaption) (not caption))
          (setq caption child))
         (t
          (setq invalid t))))
      (and (not invalid) image caption (cons image caption)))))

(defun zhihu--normalize-caption-text (text)
  "把图片注释 TEXT 归一化为知乎提交使用的单行纯文本。"
  (string-trim
   (replace-regexp-in-string
    "[[:space:]]+" " " text)))

(defun zhihu--caption-text (caption)
  "把 FIGCAPTION 节点 CAPTION 归一化为知乎图片注释纯文本。"
  (zhihu--normalize-caption-text (dom-inner-text caption)))

(defun zhihu--native-captioned-image-node (figure)
  "把单图 FIGURE 转成知乎提交使用的段落图片与 `data-caption'。"
  (pcase-let* ((`(,source-image . ,caption-node)
               (or
                 (zhihu--captioned-figure-parts figure)
                 (error "zhihu: 无法识别图片 figure 的图注结构")))
               (caption (zhihu--caption-text caption-node))
               (image (zhihu--zhihuify-node source-image t))
               (attrs (copy-tree (dom-attributes image)))
               (existing (alist-get 'data-caption attrs))
               (normalized-existing
                (and
                 (stringp existing)
                 (zhihu--normalize-caption-text existing))))
    (when
        (and
         normalized-existing
         (not (string-empty-p normalized-existing))
         (not (equal normalized-existing caption)))
      (error "zhihu: 图片的 data-caption 与 figcaption 冲突"))
    (setf (alist-get 'data-caption attrs) caption)
    (unless (alist-get 'data-size attrs)
      (setf (alist-get 'data-size attrs)
            (or (dom-attr figure 'data-size) "normal")))
    `(p nil (img ,attrs))))

(defun zhihu--zhihuify-node (node &optional standalone-image)
  "把规范 HTML 的 NODE 递归转换为知乎方言 DOM 节点。
STANDALONE-IMAGE 非 nil 表示 NODE 是独占段落或带图注的普通图片。"
  (cond
   ((stringp node) node)
   ((not (consp node)) node)
   ((and
     (eq (dom-tag node) 'img)
     (not standalone-image))
    (let ((alt (dom-attr node 'alt)))
      (if (stringp alt) alt "")))
   ((zhihu--reference-marker-p node)
    (zhihu--native-reference-node node))
   ((zhihu--math-span-p node)
    (let* ((display (zhihu--node-has-class-p node "display"))
           (tex (zhihu--math-span-tex node)))
      `(img ((eeimg . ,(if display "2" "1"))
             (src . ,(concat "//www.zhihu.com/equation?tex="
                             (url-hexify-string tex)))
             (alt . ,(replace-regexp-in-string "[\n\r]+" " " tex))))))
   ((zhihu--captioned-figure-parts node)
    (zhihu--native-captioned-image-node node))
   ((eq (dom-tag node) 'pre)
    (let ((code (car (dom-by-tag node 'code))))
      (if code
          `(pre ((lang . ,(zhihu--code-language node code))) ,(dom-inner-text code))
        `(pre ((lang . ,(or (dom-attr node 'lang) ""))) ,(dom-inner-text node)))))
   ((and (eq (dom-tag node) 'a)
         (let ((title (dom-attr node 'title)))
           (and (stringp title)
                (string-prefix-p "member_mention_" title))))
    (let ((title (dom-attr node 'title))
          (href (dom-attr node 'href)))
      (unless (string-match
               "\\`member_mention_\\([[:xdigit:]]\\{32\\}\\)\\'" title)
        (error "zhihu: 无效的知乎用户 mention 标记：%s" title))
      (let ((hash (match-string 1 title)))
        (when
            (and
             (stringp href)
             (string-match
              "\\`https://www\\.zhihu\\.com/people/\\([^/?#[:space:]]+\\)\\'"
              href))
          (setq href (concat "/people/" (match-string 1 href))))
        `(a ((class . "member_mention")
             (href . ,href)
             (data-hash . ,hash))
            ,@(mapcar #'zhihu--zhihuify-node (dom-children node))))))
   (t
    (let* ((tag (dom-tag node))
           (significant (zhihu--dom-significant-children node))
           (standalone-child
            (and
             (eq tag 'p)
             (= (length significant) 1)
             (consp (car significant))
             (eq (dom-tag (car significant)) 'img)
             (car significant)))
           ;; 知乎会过滤 style；保留语义/data 属性，class 仅在普通节点保留。
           (attrs
            (cl-remove-if
             (lambda (a) (eq (car a) 'style))
             (copy-tree (cadr node))))
           (children
            (mapcar
             (lambda (child)
               (zhihu--zhihuify-node
                child (eq child standalone-child)))
             (dom-children node))))
      (when (eq tag 'table)
        (dolist (a '((data-draft-node . "block")
                     (data-draft-type . "table")
                     (data-size . "normal")))
          (setf (alist-get (car a) attrs) (cdr a))))
      (cons tag (cons attrs children))))))

(defun zhihu--zhihuify-html (html)
  "把 HTML fragment 转成知乎可接受的公式、代码、表格和图片。"
  (let* ((dom (zhihu--parse-html (concat "<html><body>" html "</body></html>")))
         (body (car (dom-by-tag dom 'body)))
         (new-body `(body nil ,@(mapcar #'zhihu--zhihuify-node
                                        (dom-children body)))))
    (zhihu--inner-html new-body)))

(defun zhihu--png-bytes-p (bytes)
  "BYTES 是否以 PNG signature 开头。"
  (and
   (stringp bytes)
   (>= (length bytes) 8)
   (equal
    (substring bytes 0 8)
    (unibyte-string #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a))))

(defun zhihu--render-mermaid-png (source)
  "用 Mermaid CLI 把 SOURCE 渲染为 PNG 字节。
只有源稿中实际出现 Mermaid code block 时才要求 PATH 中存在 `mmdc'。"
  (when (string-empty-p (string-trim source))
    (error "zhihu: Mermaid code block 不能为空"))
  (unless (executable-find "mmdc")
    (user-error
     (concat
      "zhihu: Mermaid 图需要 mmdc；"
      "请安装 @mermaid-js/mermaid-cli")))
  (let* ((directory (make-temp-file "zhihu-mermaid-" t))
         (output (expand-file-name "diagram.png" directory)))
    (unwind-protect
        (progn
          ;; mmdc 从 stdin 读取 Mermaid，输出扩展名固定选择 PNG。
          (zhihu--shell-convert
           "mmdc" (list "--input" "-" "--output" output) source)
          (unless (and (file-regular-p output)
                       (> (file-attribute-size
                           (file-attributes output))
                          0))
            (error "zhihu: mmdc 没有生成 PNG"))
          (let ((bytes (zhihu--read-file-bytes output)))
            (unless (zhihu--png-bytes-p bytes)
              (error "zhihu: mmdc 输出不是有效的 PNG"))
            bytes))
      (ignore-errors (delete-directory directory t)))))

(defun zhihu--mermaid-node (node)
  "递归把知乎 HTML 中的 Mermaid PRE NODE 改成 PNG 图片。"
  (cond
   ((stringp node) node)
   ((not (consp node)) node)
   ((and (eq (dom-tag node) 'pre)
         (equal (dom-attr node 'lang) "mermaid"))
    (let* ((source (dom-inner-text node))
           (bytes (zhihu--render-mermaid-png source))
           (data-url
            (concat
             "data:image/png;base64,"
             (base64-encode-string bytes t))))
      `(p nil
          (img ((src . ,data-url)
                (alt . "Mermaid diagram"))))))
   (t
    (cons
     (dom-tag node)
     (cons
      (copy-tree (cadr node))
      (mapcar #'zhihu--mermaid-node (dom-children node)))))))

(defun zhihu--render-mermaid-blocks (html)
  "把 HTML 中的 Mermaid code block 渲染为 PNG data URL。"
  (let* ((dom (zhihu--parse-html (concat "<html><body>" html "</body></html>")))
         (body (car (dom-by-tag dom 'body)))
         (new-body
          `(body nil
                 ,@(mapcar
                    #'zhihu--mermaid-node
                    (dom-children body)))))
    (zhihu--inner-html new-body)))

(defun zhihu--org-typst-math-prepare (tree _backend info)
  "把 TREE 中的 Typst 数学转换为 LaTeX，并改写各 fragment 的 `:value'。"
  (when (let ((value (plist-get info :typst-math)))
          (or (eq value t)
              (and (stringp value)
                   (string-equal (downcase (string-trim value)) "t"))))
    (unless (require 'org-typst-math nil t)
      (user-error
       "zhihu: 使用 #+TYPST_MATH: t 需要安装 org-typst-math"))
    (let ((preamble (or (plist-get info :typst-header) ""))
          elements fragments items)
      (org-element-map tree 'latex-fragment
        (lambda (element)
          (when-let* ((fragment (org-typst-math-fragment element)))
            (push element elements)
            (push fragment fragments)
            (push (list :id (number-to-string (length elements))
                        :source (plist-get fragment :source)
                        :display (plist-get fragment :display))
                  items))
          nil))
      (when items
        (setq elements (nreverse elements)
              fragments (nreverse fragments)
              items (nreverse items))
        (let* ((result (typst-client-convert
                        default-directory preamble items "latex"))
               (results (append (plist-get result :items) nil))
               failed diagnostics)
          (cl-mapc
           (lambda (element fragment result)
             (if-let* ((artifact (plist-get result :artifact)))
                 (let* ((latex (or (plist-get artifact :latex) ""))
                        (display (eq (plist-get fragment :display) t)))
                   (org-element-put-property
                    element :value
                    (concat (if display "\\[" "\\(")
                            latex
                            (if display "\\]" "\\)"))))
               (setq failed t))
             (dolist (diagnostic
                      (append (plist-get result :diagnostics) nil))
               (push (or (plist-get diagnostic :message)
                         "Typst 数学转换失败")
                     diagnostics)))
           elements fragments results)
          (when failed
            (user-error
             "zhihu: Typst 数学转换失败：%s"
             (mapconcat #'identity (nreverse diagnostics) "；"))))))
    tree))

(defun zhihu--org-export-body (body _backend _info)
  "把 ox-html BODY 规范化为知乎方言 HTML。"
  (zhihu--render-mermaid-blocks
   (zhihu--zhihuify-html
    (zhihu--org-normalize-structure
     (zhihu--org-rewrite-footnotes body)))))

(defun zhihu--org-export-block (export-block contents info)
  "导出 EXPORT-BLOCK。
`zhihu' 块原样保留，其余交给 `org-html-export-block'。"
  (if (member (org-element-property :type export-block) '("ZHIHU"))
      (org-remove-indentation (org-element-property :value export-block))
    (org-html-export-block export-block contents info)))

(org-export-define-derived-backend 'zhihu 'html
  :options-alist '((:typst-math "TYPST_MATH" nil nil t)
                   (:typst-header "TYPST_HEADER" nil nil newline))
  :translate-alist '((export-block . zhihu--org-export-block))
  :filters-alist
  '((:filter-parse-tree . zhihu--org-typst-math-prepare)
    (:filter-latex-fragment . zhihu--org-html-latex-fragment-filter)
    (:filter-latex-environment . zhihu--org-html-latex-environment-filter)
    (:filter-body . zhihu--org-export-body))
  :menu-entry
  '(?z "To Zhihu"
       ((?h "As HTML buffer" org-zhihu-export-as-html)
        (?f "As HTML file" org-zhihu-export-to-html))))

;;;###autoload
(defun org-zhihu-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Zhihu HTML buffer.
ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY and EXT-PLIST have the same
meaning as in `org-export-as'.  Export is done in a buffer named
*Org Zhihu Export*, displayed according to
`org-export-show-temporary-export-buffer'."
  (interactive)
  (org-export-to-buffer 'zhihu "*Org Zhihu Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-zhihu-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a Zhihu HTML file.
ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY and EXT-PLIST have the same
meaning as in `org-export-to-file'."
  (interactive)
  (let ((outfile (org-export-output-file-name ".html" subtreep)))
    (org-export-to-file 'zhihu outfile async subtreep visible-only
                        body-only ext-plist)))

;;;; Source conversion

(defun zhihu--org->html (org-text)
  "ORG-TEXT → 知乎方言 HTML fragment。"
  (let ((org-export-use-babel nil)
        (org-confirm-babel-evaluate t)
        (org-export-with-section-numbers nil)
        ;; 禁用自动目录；显式 #+TOC 仍由 ox-html 导出。
        (org-export-with-toc nil)
        (org-html-htmlize-output-type nil)
        (org-html-head-include-default-style nil)
        (org-html-head-include-scripts nil))
    (with-temp-buffer
      (insert org-text)
      (delay-mode-hooks (org-mode))
      (org-export-as 'zhihu nil nil t
                     '(:section-numbers nil :with-toc nil)))))

(defun zhihu--source-to-html (file)
  "把 Org FILE 转为知乎方言 HTML fragment。"
  (let ((default-directory
         (file-name-directory (expand-file-name file))))
    (zhihu--org->html
     (with-temp-buffer
       (insert-file-contents file)
       (buffer-string)))))

;;;; Utilities

(defun zhihu--value-string (value)
  "把字符串或整数 VALUE 归一化成非空字符串。"
  (let ((s (cond ((stringp value) value)
                 ((integerp value) (number-to-string value)))))
    (and s (not (string-empty-p s)) s)))

(defun zhihu--read-file-bytes (path)
  "PATH 读为 unibyte 字节字符串。"
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun zhihu--write-file-bytes (path bytes)
  "把 BYTES 原样写入 PATH。"
  (unless (stringp bytes)
    (error "zhihu: 待写入的图片数据不是字符串"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert
     (if (multibyte-string-p bytes)
         (encode-coding-string bytes 'utf-8 t)
       bytes))
    (let ((coding-system-for-write 'no-conversion))
      (write-region (point-min) (point-max) path nil 'silent))))

(provide 'ox-zhihu)
;;; ox-zhihu.el ends here
