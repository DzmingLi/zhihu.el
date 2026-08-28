;;; zhihu.el --- Write and publish on Zhihu  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Dzming Li

;; Author: Dzming Li <i@dzming.li>
;; Maintainer: Dzming Li <i@dzming.li>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1") (plz "0.10-pre"))
;; Keywords: convenience, hypermedia, tools
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

;; The ZSE v4 signing implementation includes code adapted from
;; zhihu-sign-kt <https://github.com/zly2006/zhihu-sign-kt>:
;;
;; MIT License
;;
;; Copyright (c) 2026 zly2006
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; 在 Emacs 中撰写并发布知乎回答和文章。
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-util)
(require 'json)
(require 'dom)
(require 'xml)
(require 'sqlite)
(require 'mailcap)
(require 'plz)
(require 'ox-html)

(define-error
  'zhihu-create-result-unknown
  "无法确认知乎创建请求的结果")

(declare-function secrets-search-item-paths "secrets"
                  (collection &rest attributes))
(declare-function secrets-get-secret "secrets" (collection item))

(defgroup zhihu nil
  "在Emacs中编辑，同步和发布知乎文章"
  :group 'applications
  :prefix "zhihu-")

;;;; Customization

(defcustom zhihu-publish-defaults
  '((draft_type . "normal")
    (delta_time . 30)
    (can_reward . :json-false))
  "发布答案/文章时的全局默认 settings。
这些字段对应知乎创作中心
https://www.zhihu.com/creator/editor-setting 的设置项。"
  :type '(alist :key-type symbol :value-type sexp)
  :group 'zhihu)

(defcustom zhihu-publish-default-reprint-permission "allowed"
  "稿件未指定 `reprint-permission' 时使用的转载权限。"
  :type '(choice
          (const :tag "允许转载" "allowed")
          (const :tag "禁止转载" "disallowed")
          (const :tag "付费转载" "need_payment"))
  :group 'zhihu)

(defcustom zhihu-publish-default-comment-permission "all"
  "稿件未指定 `comment-permission' 时使用的评论权限。"
  :type '(choice
          (const :tag "允许所有人评论" "all")
          (const :tag "评论需要审核" "censor")
          (const :tag "仅允许我关注的人评论" "followee")
          (const :tag "禁止评论" "nobody"))
  :group 'zhihu)



(defcustom zhihu-article-cc-statement nil
  "发布文章时在正文末尾追加的 Creative Commons 声明。
nil 表示不追加；其它值选择一种 CC 4.0 国际许可或 CC0 1.0 公共领域
贡献。声明只覆盖未另行声明的原创内容，不作用于回答，也不修改源稿。
CC 许可和 CC0 均不可撤销，启用前应确认自己拥有或控制相关内容的版权。"
  :type '(choice
          (const :tag "不追加 CC 声明" nil)
          (const :tag "CC0 1.0（公共领域贡献）" cc0)
          (const :tag "CC BY 4.0（署名）" by)
          (const :tag "CC BY-SA 4.0（署名—相同方式共享）" by-sa)
          (const :tag "CC BY-ND 4.0（署名—禁止演绎）" by-nd)
          (const :tag "CC BY-NC 4.0（署名—非商业性使用）" by-nc)
          (const
           :tag "CC BY-NC-SA 4.0（署名—非商业性使用—相同方式共享）"
           by-nc-sa)
          (const
           :tag "CC BY-NC-ND 4.0（署名—非商业性使用—禁止演绎）"
           by-nc-nd))
  :group 'zhihu)

;;;; Cookies

(defcustom zhihu-cookie-browser 'firefox
  "提供知乎登录 Cookie 的浏览器。
Firefox 直接读取 profile 的 SQLite 数据库；Chromium 系浏览器还会通过
系统凭据存储取得 Cookie 解密密钥。"
  :type '(choice
          (const :tag "Firefox" firefox)
          (const :tag "Chromium" chromium)
          (const :tag "Google Chrome" chrome)
          (const :tag "Microsoft Edge" edge))
  :group 'zhihu)

(defcustom zhihu-cookie-profile-directory nil
  "用于读取知乎 Cookie 的浏览器 profile 目录。
必须显式设置；本包不会扫描或猜测 profile。Firefox 应指向包含
cookies.sqlite 的目录，Chromium/Chrome/Edge 应指向包含 Network/Cookies
的目录。"
  :type '(choice (const :tag "未配置" nil)
                 (directory :must-match t))
  :group 'zhihu)

(cl-defstruct
    (zhihu--xsrf-state
     (:constructor zhihu--make-xsrf-state (&optional xsrf-token)))
  "一次文章发布操作中的 `_xsrf' 状态。"
  xsrf-token)

(cl-defstruct
    (zhihu--cookie-record
     (:constructor zhihu--make-cookie-record))
  "从浏览器数据库读取、尚未按请求 URL 筛选的 Cookie。"
  name value domain path expires secure creation)

(cl-defstruct
    (zhihu--completion-session
     (:constructor zhihu--make-completion-session))
  "一次动态补全的源稿位置、候选和最终提交规则。"
  source-buffer
  start-marker
  end-marker
  format
  suffix
  seen
  source-function)

(declare-function
 dbus-call-method "dbus"
 (bus service path interface method &rest args))

(defconst zhihu--chromium-browser-specs
  '((chromium
     :secret-applications ("chromium")
     :kwallet-folder "Chromium Keys"
     :kwallet-key "Chromium Safe Storage")
    (chrome
     :secret-applications ("chrome")
     :kwallet-folder "Chrome Keys"
     :kwallet-key "Chrome Safe Storage")
    (edge
     :secret-applications ("chromium")
     :kwallet-folder "Chromium Keys"
     :kwallet-key "Chromium Safe Storage"))
  "Chromium 系浏览器的系统凭据存储标识。")

(defconst zhihu--chromium-linux-v10-key
  (unibyte-string
   #xfd #x62 #x1f #xe5 #xa2 #xb4 #x02 #x53
   #x9d #xfa #x14 #x7c #xa9 #x27 #x27 #x78)
  "Chromium Linux basic password store 的 AES-128 key。")

(defconst zhihu--chromium-aes-cbc-iv
  (encode-coding-string (make-string 16 ?\s) 'us-ascii t)
  "Chromium v10/v11 AES-CBC 使用的固定 IV。")

(defconst zhihu--chromium-time-epoch-offset 11644473600000000
  "Unix epoch 之前的 Chromium 微秒数。")

(defun zhihu--cookie-store-file (browser)
  "返回 BROWSER 的显式 profile 中唯一的 Cookie 存储文件。"
  (let* ((relative
          (pcase browser
            ('firefox "cookies.sqlite")
            ((or 'chromium 'chrome 'edge) "Network/Cookies")
            (_ (error "zhihu: 不支持的 Cookie 浏览器：%s" browser))))
         (profile zhihu-cookie-profile-directory))
    (unless (and (stringp profile)
                 (not (string-empty-p (string-trim profile))))
      (error
       "zhihu: 请设置 zhihu-cookie-profile-directory；本包不会自动选择浏览器 profile"))
    (setq profile (expand-file-name profile))
    (unless (and (file-directory-p profile)
                 (file-readable-p profile))
      (error "zhihu: Cookie profile 目录不可读：%s" profile))
    (let ((file (expand-file-name relative profile)))
      (unless (and (file-regular-p file)
                   (file-readable-p file))
        (error "zhihu: Cookie profile 中缺少可读的 %s：%s"
               relative profile))
      file)))

(defun zhihu--cookie-url-parts (url)
  "解析 URL，返回 (HOST PATH SECURE)。
URL 必须是带 host 的绝对 HTTP(S) URL。"
  (let* ((parsed (url-generic-parse-url url))
         (host (url-host parsed))
         (scheme (downcase (or (url-type parsed) "")))
         (filename (or (url-filename parsed) "/"))
         (path (car (split-string filename "[?#]" t))))
    (unless (and (stringp host) (not (string-empty-p host))
                 (member scheme '("http" "https")))
      (error "zhihu: 无法从 URL 读取浏览器 Cookie：%s" url))
    (list (downcase host)
          (if (and path (string-prefix-p "/" path)) path "/")
          (string-equal scheme "https"))))

(defun zhihu--cookie-domain-matches-p (domain host)
  "返回 DOMAIN Cookie 是否适用于 HOST。"
  (let ((domain (downcase domain))
        (host (downcase host)))
    (if (string-prefix-p "." domain)
        (let ((bare (substring domain 1)))
          (or (string-equal host bare)
              (string-suffix-p (concat "." bare) host)))
      (string-equal domain host))))

(defun zhihu--cookie-domain-candidates (host)
  "返回 HOST 可能匹配的浏览器 Cookie domain 候选。"
  (let ((candidates (list host (concat "." host)))
        (start 0))
    (while (string-match "\\." host start)
      (push (substring host (match-beginning 0)) candidates)
      (setq start (match-end 0)))
    (delete-dups candidates)))

(defun zhihu--cookie-path-matches-p (cookie-path request-path)
  "返回 COOKIE-PATH 是否适用于 REQUEST-PATH。"
  (let ((cookie-path (if (string-empty-p cookie-path) "/" cookie-path)))
    (or (string-equal cookie-path request-path)
        (and (string-prefix-p cookie-path request-path)
             (or (string-suffix-p "/" cookie-path)
                 (and (> (length request-path) (length cookie-path))
                      (eq (aref request-path (length cookie-path)) ?/)))))))

(defun zhihu--cookie-records-to-alist (records)
  "按 RFC 6265 发送顺序把 RECORDS 转成 ((NAME . VALUE) ...)。"
  (mapcar
   (lambda (record)
     (cons (zhihu--cookie-record-name record)
           (zhihu--cookie-record-value record)))
   (cl-stable-sort
    (copy-sequence records)
    (lambda (left right)
      (let ((left-length
             (length (zhihu--cookie-record-path left)))
            (right-length
             (length (zhihu--cookie-record-path right))))
        (if (= left-length right-length)
            (< (or (zhihu--cookie-record-creation left) 0)
               (or (zhihu--cookie-record-creation right) 0))
          (> left-length right-length)))))))

(defun zhihu--cookie-records-for-url (records url)
  "筛选并排序适用于 URL 的 RECORDS，返回 ((NAME . VALUE) ...)。"
  (pcase-let* ((`(,host ,path ,secure) (zhihu--cookie-url-parts url))
               (now (float-time))
               (applicable
                (cl-remove-if-not
                 (lambda (record)
                   (and
                    (zhihu--cookie-domain-matches-p
                     (zhihu--cookie-record-domain record) host)
                    (zhihu--cookie-path-matches-p
                     (zhihu--cookie-record-path record) path)
                    (or (not (zhihu--cookie-record-secure record)) secure)
                    (or (null (zhihu--cookie-record-expires record))
                        (> (zhihu--cookie-record-expires record) now))))
                 records)))
    (zhihu--cookie-records-to-alist applicable)))

;; Firefox cookie database

(defun zhihu--select-firefox-cookies (db table url)
  "从 DB 的可信 TABLE 中选出适用于 URL 的默认容器 Cookie。"
  (pcase-let* ((`(,host ,_path ,_secure)
                 (zhihu--cookie-url-parts url))
               (domains (zhihu--cookie-domain-candidates host))
               (placeholders
                (mapconcat (lambda (_domain) "?") domains ","))
               (records
                (mapcar
                 (lambda (row)
                   (let ((expiry (nth 4 row)))
                     (zhihu--make-cookie-record
                      :name (nth 0 row)
                      :value (nth 1 row)
                      :domain (nth 2 row)
                      :path (or (nth 3 row) "/")
                      ;; Firefox profiles in the wild use both seconds and
                      ;; milliseconds for `expiry'.
                      :expires (and (numberp expiry)
                                    (if (> expiry 100000000000)
                                        (/ expiry 1000.0)
                                      expiry))
                      :secure (not (zerop (or (nth 5 row) 0)))
                      :creation (nth 6 row))))
                 (sqlite-select
                  db
                  (concat
                   "SELECT name, value, host, path, expiry, isSecure, "
                   "creationTime FROM " table " "
                   "WHERE host IN (" placeholders
                   ") AND originAttributes = ?")
                  (append domains (list ""))))))
    (zhihu--cookie-records-for-url records url)))

(defun zhihu--sqlite-readonly-uri (path)
  "把 PATH 转成不会误解析文件名中 URI 保留字符的只读 SQLite URI。"
  (concat "file:"
          (url-hexify-string
           (expand-file-name path)
           (cons ?/ url-unreserved-chars))
          "?mode=ro&cache=private"))

(defun zhihu--query-cookie-database (path label query)
  "查询 Cookie 数据库 PATH，并以 LABEL 生成错误信息。
QUERY 接收数据库和 schema 名；只读查询发生 SQLite 错误时改读临时
数据库/WAL 副本。"
  (let ((run-query
         (lambda (db schema)
           (let (transaction)
             (unwind-protect
                 (progn
                   (sqlite-execute db "BEGIN")
                   (setq transaction t)
                   (prog1 (funcall query db schema)
                     (sqlite-execute db "COMMIT")
                     (setq transaction nil)))
               (when transaction
                 (ignore-errors (sqlite-execute db "ROLLBACK"))))))))
    (condition-case readonly-error
        (let (db)
          (unwind-protect
              (progn
                ;; `sqlite-open' 没有 readonly 参数，因此只创建内存主库，
                ;; 再以只读 URI ATTACH 浏览器数据库。
                (setq db (sqlite-open))
                (sqlite-execute
                 db "ATTACH DATABASE ? AS cookies"
                 (list (zhihu--sqlite-readonly-uri path)))
                (funcall run-query db "cookies"))
            (when db
              (ignore-errors (sqlite-close db)))))
      (sqlite-error
       (condition-case copy-error
           (let (snapshot-directory db)
             (unwind-protect
                 (let ((snapshot-name (file-name-nondirectory path)))
                   (setq snapshot-directory
                         (make-temp-file "zhihu-cookie-snapshot-" t))
                   (dolist (suffix '("" "-wal"))
                     (let ((source (concat path suffix)))
                       (when (file-readable-p source)
                         (copy-file
                          source
                          (expand-file-name
                           (concat snapshot-name suffix)
                           snapshot-directory)
                          t))))
                   (setq db
                         (sqlite-open
                          (expand-file-name
                           snapshot-name snapshot-directory)))
                   (funcall run-query db "main"))
               (when db
                 (ignore-errors (sqlite-close db)))
               (when snapshot-directory
                 (ignore-errors
                   (delete-directory snapshot-directory t)))))
         (error
          (error "zhihu: 读 %s 失败：只读访问：%s；临时副本：%s"
                 label
                 (error-message-string readonly-error)
                 (error-message-string copy-error))))))))

(defun zhihu--read-firefox-cookies (path url)
  "从 Firefox cookies.sqlite 的 PATH 读出适用于 URL 的 Cookie。
返回 ((NAME . VALUE) ...) alist。只读访问失败时改读临时 DB/WAL 副本。"
  (unless (file-readable-p path)
    (error "zhihu: Firefox Cookie 数据库不可读：%s" path))
  (zhihu--cookie-url-parts url)
  (zhihu--query-cookie-database
   path "Firefox cookies"
   (lambda (db schema)
     (zhihu--select-firefox-cookies
      db (concat schema ".moz_cookies") url))))

(defun zhihu--chromium-browser-spec (browser)
  "返回 BROWSER 的 Chromium 凭据存储配置。"
  (or (cdr (assq browser zhihu--chromium-browser-specs))
      (error "zhihu: 不支持的 Chromium 系浏览器：%s"
             browser)))

(defun zhihu--hmac-sha1-bytes (key bytes)
  "返回 KEY 对 BYTES 的 HMAC-SHA1 原始字节。"
  (require 'gnutls)
  (unless (and (fboundp 'gnutls-hash-mac)
               (assq 'SHA1 (gnutls-macs)))
    (error "zhihu: 当前 Emacs/GnuTLS 不支持 HMAC-SHA1"))
  ;; GnuTLS 会主动清空字符串形式的 key，传副本避免破坏调用方缓存。
  (gnutls-hash-mac 'SHA1 (copy-sequence key) bytes))

(defun zhihu--xor-byte-strings (left right)
  "逐字节异或等长的 LEFT 与 RIGHT。"
  (unless (= (length left) (length right))
    (error "zhihu: 内部 PBKDF2 字节长度不一致"))
  (let ((output (copy-sequence left)))
    (dotimes (index (length output))
      (aset output index
            (logxor (aref output index) (aref right index))))
    output))

(defun zhihu--pbkdf2-hmac-sha1 (password salt iterations length)
  "以 PASSWORD、SALT 和 ITERATIONS 派生 LENGTH 字节 PBKDF2 key。"
  (unless (and (integerp iterations) (> iterations 0)
               (integerp length) (> length 0))
    (error "zhihu: 无效的 PBKDF2 参数"))
  (let ((block 1)
        (output (unibyte-string)))
    (while (< (length output) length)
      (let* ((counter
              (unibyte-string
               (logand (ash block -24) #xff)
               (logand (ash block -16) #xff)
               (logand (ash block -8) #xff)
               (logand block #xff)))
             (unit
              (zhihu--hmac-sha1-bytes
               password (concat salt counter)))
             (accumulator (copy-sequence unit)))
        (dotimes (_ (1- iterations))
          (setq unit (zhihu--hmac-sha1-bytes password unit)
                accumulator
                (zhihu--xor-byte-strings accumulator unit)))
        (setq output (concat output accumulator)
              block (1+ block))))
    (substring output 0 length)))

(defun zhihu--chromium-secret-service-password (spec)
  "按照 SPEC 从 Secret Service 读取 Chromium Safe Storage secret。"
  (require 'secrets)
  (unless (and (boundp 'secrets-enabled)
               (symbol-value 'secrets-enabled))
    (error "zhihu: 当前会话没有可用的 Secret Service"))
  (let ((collections
         (delete-dups
          (cons "default" (secrets-list-collections))))
        secret)
    (condition-case err
        (dolist (application (plist-get spec :secret-applications))
          (dolist (collection collections)
            (unless secret
              (dolist
                  (item
                   (secrets-search-item-paths
                    collection
                    :xdg:schema
                    "chrome_libsecret_os_crypt_password_v2"
                    :application application))
                (unless secret
                  (setq secret
                        (secrets-get-secret collection item)))))))
      (error
       (error "zhihu: 从 Secret Service 读取浏览器密钥失败：%s"
              (error-message-string err))))
    (unless (and (stringp secret) (not (string-empty-p secret)))
      (error "zhihu: Secret Service 中没有浏览器 Safe Storage 密钥"))
    (encode-coding-string secret 'utf-8 t)))

(defconst zhihu--chromium-kwallet-endpoints
  '(("org.kde.kwalletd6" "/modules/kwalletd6")
    ("org.kde.kwalletd5" "/modules/kwalletd5")
    ("org.kde.kwalletd" "/modules/kwalletd"))
  "当前 KDE KWallet D-Bus 服务及其对象路径。")

(defun zhihu--chromium-kwallet-password-at (endpoint spec)
  "从 KWallet ENDPOINT 读取 SPEC 对应的 Safe Storage password。"
  (require 'dbus)
  (let* ((service (car endpoint))
         (path (cadr endpoint))
         (interface "org.kde.KWallet")
         (application "zhihu.el")
         handle)
    (unwind-protect
        (progn
          (unless
              (dbus-call-method
               :session service path interface "isEnabled")
            (error "KWallet 未启用"))
          (let ((wallet
                 (dbus-call-method
                  :session service path interface "networkWallet")))
            (setq handle
                  (dbus-call-method
                   :session service path interface "open"
                   wallet :int64 0 application)))
          (unless (and (integerp handle) (>= handle 0))
            (error "KWallet 无法打开"))
          (unless
              (dbus-call-method
               :session service path interface "hasFolder"
               :int32 handle
               (plist-get spec :kwallet-folder)
               application)
            (error "KWallet 中没有浏览器密钥目录"))
          (unless
              (dbus-call-method
               :session service path interface "hasEntry"
               :int32 handle
               (plist-get spec :kwallet-folder)
               (plist-get spec :kwallet-key)
               application)
            (error "KWallet 中没有浏览器 Safe Storage 密钥"))
          (let ((password
                 (dbus-call-method
                  :session service path interface "readPassword"
                  :int32 handle
                  (plist-get spec :kwallet-folder)
                  (plist-get spec :kwallet-key)
                  application)))
            (unless
                (and (stringp password)
                     (not (string-empty-p password)))
              (error "KWallet 中的浏览器密钥为空"))
            (encode-coding-string password 'utf-8 t)))
      (when (and (integerp handle) (>= handle 0))
        (ignore-errors
          (dbus-call-method
           :session service path interface "close"
           :int32 handle nil application))))))

(defun zhihu--chromium-kwallet-password (spec)
  "从当前 KDE KWallet 读取 SPEC 对应的 Safe Storage password。"
  (let (password last-error)
    (dolist (endpoint zhihu--chromium-kwallet-endpoints)
      (unless password
        (condition-case err
            (setq password
                  (zhihu--chromium-kwallet-password-at endpoint spec))
          (error (setq last-error err)))))
    (or password
        (error "zhihu: 从 KWallet 读取浏览器密钥失败：%s"
               (if last-error
                   (error-message-string last-error)
                 "没有可用服务")))))

(defun zhihu--chromium-linux-password (spec)
  "从 Linux 桌面凭据存储读取 SPEC 的 Safe Storage password。"
  (condition-case secret-service-error
      (zhihu--chromium-secret-service-password spec)
    (error
     (condition-case kwallet-error
         (zhihu--chromium-kwallet-password spec)
       (error
        (error
         "zhihu: 无法读取浏览器 Safe Storage 密钥：Secret Service：%s；KWallet：%s"
         (error-message-string secret-service-error)
         (error-message-string kwallet-error)))))))

(defun zhihu--chromium-v10-key ()
  "返回 GNU/Linux Chromium v10 key。"
  (unless (eq system-type 'gnu/linux)
    (error "zhihu: 当前系统不支持 Chromium Cookie 解密"))
  (copy-sequence zhihu--chromium-linux-v10-key))

(defun zhihu--chromium-v11-key (spec)
  "按照当前系统与 SPEC 返回 Chromium v11 key。"
  (unless (eq system-type 'gnu/linux)
    (error "zhihu: 当前系统不支持 Chromium v11 Cookie"))
  (let ((password (zhihu--chromium-linux-password spec)))
    (unwind-protect
        (zhihu--pbkdf2-hmac-sha1
         password (encode-coding-string "saltysalt" 'us-ascii t) 1 16)
      (clear-string password))))

(defun zhihu--pkcs7-unpad (bytes block-size)
  "校验并移除 BYTES 的 PKCS#7 padding，块大小为 BLOCK-SIZE。"
  (let* ((length (length bytes))
         (padding (and (> length 0) (aref bytes (1- length)))))
    (unless (and (integerp padding)
                 (> padding 0)
                 (<= padding block-size)
                 (<= padding length)
                 (cl-loop for index from (- length padding) below length
                          always (= (aref bytes index) padding)))
      (error "zhihu: Chromium Cookie 的 AES padding 无效"))
    (substring bytes 0 (- length padding))))

(defun zhihu--chromium-aes-cbc-decrypt (key ciphertext)
  "以 KEY 解密 Chromium AES-CBC CIPHERTEXT。"
  (require 'gnutls)
  (unless (and (fboundp 'gnutls-symmetric-decrypt)
               (assq 'AES-128-CBC (gnutls-ciphers)))
    (error "zhihu: 当前 Emacs/GnuTLS 不支持 AES-128-CBC"))
  (let ((result
         (gnutls-symmetric-decrypt
          'AES-128-CBC
          (copy-sequence key)
          zhihu--chromium-aes-cbc-iv
          ciphertext)))
    (unless (and (consp result) (stringp (car result)))
      (error "zhihu: Chromium Cookie AES 解密失败"))
    (zhihu--pkcs7-unpad (car result) 16)))

(defun zhihu--chromium-decrypt-cookie
    (encrypted-value host-key database-version keys)
  "用 KEYS 中相应前缀的密钥解密 ENCRYPTED-VALUE。
同时校验 HOST-KEY domain binding。"
  (unless (and (stringp encrypted-value)
               (>= (length encrypted-value) 3))
    (error "zhihu: Chromium Cookie 密文无效"))
  (let* ((prefix (substring encrypted-value 0 3))
         (key-entry (assoc-string prefix keys)))
    (unless key-entry
      (error "zhihu: 不支持 Chromium Cookie 加密格式 %s" prefix))
    (condition-case err
        (let ((plaintext
               (zhihu--chromium-aes-cbc-decrypt
                (cdr key-entry) (substring encrypted-value 3))))
          (when (>= database-version 24)
            (let ((domain-hash
                   (secure-hash
                    'sha256
                    (encode-coding-string host-key 'utf-8 t)
                    nil nil t)))
              (unless
                  (and
                   (>= (length plaintext) (length domain-hash))
                   (string-prefix-p domain-hash plaintext))
                (error
                 "zhihu: Chromium Cookie 的 domain binding 校验失败"))
              (setq plaintext
                    (substring plaintext (length domain-hash)))))
          (decode-coding-string plaintext 'utf-8 t))
      (error
       (error "zhihu: Chromium Cookie 解密失败：%s"
              (error-message-string err))))))

(defun zhihu--chromium-time-to-unix (value)
  "把 Chromium 微秒时间 VALUE 转成 Unix 秒。"
  (/ (- value zhihu--chromium-time-epoch-offset) 1000000.0))

(defun zhihu--select-chromium-cookies (db table url spec)
  "从 DB 的可信 TABLE 读取并解密适用于 URL 的 Chromium Cookie。"
  (pcase-let* ((`(,host ,request-path ,request-secure)
                 (zhihu--cookie-url-parts url))
               (domains (zhihu--cookie-domain-candidates host))
               (placeholders
                (mapconcat (lambda (_domain) "?") domains ","))
               (version-row
                (car
                 (sqlite-select
                  db
                  (concat
                   "SELECT value FROM " table ".meta "
                   "WHERE key = ?")
                  (list "version"))))
               (database-version
                (and version-row
                     (string-to-number (format "%s" (car version-row)))))
               (now (float-time))
               (raw-rows
                (sqlite-select
                 db
                 (concat
                  "SELECT host_key, name, value, encrypted_value, path, "
                  "expires_utc, is_secure, has_expires, creation_utc "
                  "FROM " table ".cookies "
                  "WHERE top_frame_site_key = ? AND host_key IN ("
                  placeholders ")")
                 (cons "" domains)))
               ;; Apply browser policy before touching encrypted values.
               ;; An expired or wrong-path record with a newer prefix must
               ;; not abort an otherwise valid request.
               (rows
                (cl-remove-if-not
                 (lambda (row)
                   (let* ((host-key (nth 0 row))
                          (cookie-path (or (nth 4 row) "/"))
                          (expires-utc (nth 5 row))
                          (cookie-secure
                           (not (zerop (or (nth 6 row) 0))))
                          (has-expires
                           (not (zerop (or (nth 7 row) 0))))
                          (expires
                           (and has-expires
                                (numberp expires-utc)
                                (zhihu--chromium-time-to-unix
                                 expires-utc))))
                     (and
                      (zhihu--cookie-domain-matches-p host-key host)
                      (zhihu--cookie-path-matches-p
                       cookie-path request-path)
                      (or (not cookie-secure) request-secure)
                      (or (not has-expires)
                          (and expires (> expires now))))))
                 raw-rows)))
    (unless (and (integerp database-version) (> database-version 0))
      (error "zhihu: Chromium Cookie 数据库缺少有效 schema version"))
    (let ((prefixes
           (delete-dups
            (delq nil
                  (mapcar
                   (lambda (row)
                     (let ((plain (nth 2 row))
                           (encrypted (nth 3 row)))
                       (and (string-empty-p (or plain ""))
                            (stringp encrypted)
                            (>= (length encrypted) 3)
                            (substring encrypted 0 3))))
                   rows))))
          keys records)
      (unwind-protect
          (progn
            (dolist (prefix prefixes)
              (push
               (cons
                prefix
                (pcase prefix
                  ("v10" (zhihu--chromium-v10-key))
                  ("v11" (zhihu--chromium-v11-key spec))
                  (_
                   (error
                    "zhihu: 不支持 Chromium Cookie 加密格式 %s"
                    prefix))))
               keys))
            (dolist (row rows)
              (let* ((host-key (nth 0 row))
                     (name (nth 1 row))
                     (plain-value (nth 2 row))
                     (encrypted-value (nth 3 row))
                     (expires (nth 5 row))
                     (has-expires (not (zerop (or (nth 7 row) 0))))
                     value)
                (setq value
                      (cond
                       ((not (string-empty-p (or plain-value "")))
                        plain-value)
                       ((not (string-empty-p (or encrypted-value "")))
                        (zhihu--chromium-decrypt-cookie
                         encrypted-value host-key database-version keys))
                       (t "")))
                (push
                 (zhihu--make-cookie-record
                  :name name
                  :value value
                  :domain host-key
                  :path (or (nth 4 row) "/")
                  :expires
                  (and has-expires (numberp expires)
                       (zhihu--chromium-time-to-unix expires))
                  :secure (not (zerop (or (nth 6 row) 0)))
                  :creation (nth 8 row))
                 records)))
            (zhihu--cookie-records-to-alist records))
        (dolist (entry keys)
          (when (stringp (cdr entry))
            (clear-string (cdr entry))))))))

(defun zhihu--read-chromium-cookies (path url browser)
  "从 BROWSER 的 Chromium Cookie 数据库 PATH 读取 URL 的 Cookie。"
  (let ((spec (zhihu--chromium-browser-spec browser)))
    (unless (file-readable-p path)
      (error "zhihu: %s Cookie 数据库不可读：%s" browser path))
    (zhihu--query-cookie-database
     path (format "%s Cookie" browser)
     (lambda (db schema)
       (zhihu--select-chromium-cookies db schema url spec)))))

(defun zhihu--read-browser-cookies (url)
  "从显式配置的浏览器 profile 读取适用于完整 URL 的 Cookie。"
  (let* ((browser zhihu-cookie-browser)
         (path (zhihu--cookie-store-file browser)))
    (pcase browser
      ('firefox
       (zhihu--read-firefox-cookies path url))
      ((or 'chromium 'chrome 'edge)
       (zhihu--read-chromium-cookies path url browser)))))

(defun zhihu--format-cookie-header (cookies xsrf-token)
  "把 COOKIES 与 XSRF-TOKEN 合并成 Cookie header。
COOKIES 是 ((NAME . VALUE) ...)；其中所有 `_xsrf' 都先被移除，再仅按
XSRF-TOKEN 加入一个。"
  (let* ((cookies-without-old-xsrf
          (cl-remove-if
           (lambda (cookie) (string-equal (car cookie) "_xsrf"))
           cookies))
         (merged
          (if xsrf-token
              (append cookies-without-old-xsrf
                      (list (cons "_xsrf" xsrf-token)))
            cookies-without-old-xsrf)))
    (when merged
      (mapconcat (lambda (cookie)
                   (format "%s=%s" (car cookie) (cdr cookie)))
                 merged "; "))))

(defun zhihu--xsrf-token-after-headers (headers previous-token)
  "根据响应 HEADERS 更新 PREVIOUS-TOKEN。
没有 `_xsrf' Set-Cookie 时保留旧值；非空值覆盖旧值；空值清除旧值。"
  (let ((token previous-token))
    (dolist (header headers)
      (when (and (eq (car header) 'set-cookie)
                 (string-match "\\`[ \t]*_xsrf=\\([^;]*\\)" (cdr header)))
        (let ((new-token (match-string 1 (cdr header))))
          (setq token
                (and (not (string-empty-p new-token)) new-token)))))
    token))

(defun zhihu--update-xsrf-state (xsrf-state headers)
  "根据响应 HEADERS 更新 XSRF-STATE。"
  (setf (zhihu--xsrf-state-xsrf-token xsrf-state)
        (zhihu--xsrf-token-after-headers
         headers
         (zhihu--xsrf-state-xsrf-token xsrf-state))))

;;;; Request signing

;; Zhihu web API 的 ZSE v4 签名。算法移植自 MIT 许可的 zhihu-sign-kt，
;; 并以固定输入输出向量交叉验证；完整声明见本文件头部。

(defconst zhihu--zse93 "101_3_3.0")

(defconst zhihu--zse-zk
  [1170614578 1024848638 1413669199 3951632832
	      3528873006 2921909214 4151847688 3997739139
	      1933479194 3323781115 3888513386 460404854
	      3747539722 2403641034 2615871395 2119585428
	      2265697227 2035090028 2773447226 4289380121
	      4217216195 2200601443 3051914490 1579901135
	      1321810770 456816404 2903323407 4065664991
	      330002838 3506006750 363569021 2347096187])

(defconst zhihu--zse-zb
  [20 223 245 7 248 2 194 209 87 6 227 253 240 128 222 91
      237 9 125 157 230 93 252 205 90 79 144 199 159 197 186 167
      39 37 156 198 38 42 43 168 217 153 15 103 80 189 71 191
      97 84 247 95 36 69 14 35 12 171 28 114 178 148 86 182
      32 83 158 109 22 255 94 238 151 85 77 124 254 18 4 26
      123 176 232 193 131 172 143 142 150 30 10 146 162 62 224 218
      196 229 1 192 213 27 110 56 231 180 138 107 242 187 54 120
      19 44 117 228 215 203 53 239 251 127 81 11 133 96 204 132
      41 115 73 55 249 147 102 48 122 145 106 118 74 190 29 16
      174 5 177 129 63 113 99 31 161 76 246 34 211 13 60 68
      207 160 65 111 82 165 67 169 225 57 112 244 155 51 236 200
      233 58 61 47 100 137 185 64 17 70 234 163 219 108 170 166
      59 149 52 105 24 212 78 173 45 0 116 226 119 136 206 135
      175 195 25 92 121 208 126 139 3 75 141 21 130 98 241 40
      154 66 184 49 181 46 243 88 101 183 8 23 72 188 104 179
      210 134 250 201 164 89 216 202 220 50 221 152 140 33 235 214])

(defconst zhihu--zse-alphabet
  "6fpLRqJO8M/c3jnYxFkUVC4ZIG12SiH=5v0mXDazWBTsuw7QetbKdoPyAl+hN9rgE")

(defconst zhihu--zse-key (vconcat "059053f7d15e01d7"))

(defun zhihu--zse-rotl32 (value bits)
  "把 VALUE 当作无符号 32 位整数循环左移 BITS 位。"
  (setq value (logand value #xffffffff))
  (logand #xffffffff
          (logior (ash value bits)
                  (ash value (- bits 32)))))

(defun zhihu--zse-read-u32-be (bytes offset)
  "从 BYTES 的 OFFSET 处读取一个无符号大端 32 位整数。"
  (logior (ash (aref bytes offset) 24)
          (ash (aref bytes (+ offset 1)) 16)
          (ash (aref bytes (+ offset 2)) 8)
          (aref bytes (+ offset 3))))

(defun zhihu--zse-write-u32-be (value bytes offset)
  "把 VALUE 作为大端 32 位整数写入 BYTES 的 OFFSET 处。"
  (dotimes (i 4)
    (aset bytes (+ offset i)
          (logand #xff (ash value (- (* 8 (- 3 i))))))))

(defun zhihu--zse-g-transform (value)
  "执行 ZSE v4 的 32 位非线性变换。"
  (let* ((b0 (logand #xff (ash value -24)))
         (b1 (logand #xff (ash value -16)))
         (b2 (logand #xff (ash value -8)))
         (b3 (logand #xff value))
         (substituted
          (logior (ash (aref zhihu--zse-zb b0) 24)
                  (ash (aref zhihu--zse-zb b1) 16)
                  (ash (aref zhihu--zse-zb b2) 8)
                  (aref zhihu--zse-zb b3))))
    (logand #xffffffff
            (logxor substituted
                    (zhihu--zse-rotl32 substituted 2)
                    (zhihu--zse-rotl32 substituted 10)
                    (zhihu--zse-rotl32 substituted 18)
                    (zhihu--zse-rotl32 substituted 24)))))

(defun zhihu--zse-r-block (input)
  "加密 16 字节 INPUT，返回一个新的 16 字节向量。"
  (let ((state (make-vector 36 0))
        (output (make-vector 16 0)))
    (dotimes (i 4)
      (aset state i (zhihu--zse-read-u32-be input (* i 4))))
    (dotimes (i 32)
      (let ((transformed
             (zhihu--zse-g-transform
              (logxor (aref state (+ i 1))
                      (aref state (+ i 2))
                      (aref state (+ i 3))
                      (aref zhihu--zse-zk i)))))
        (aset state (+ i 4)
              (logand #xffffffff (logxor (aref state i) transformed)))))
    (dotimes (i 4)
      (zhihu--zse-write-u32-be
       (aref state (- 35 i)) output (* i 4)))
    output))

(defun zhihu--zse-x-blocks (data initial-vector)
  "用 INITIAL-VECTOR 链式加密 DATA；DATA 长度必须是 16 的倍数。"
  (let ((output (make-vector (length data) 0))
        (iv initial-vector)
        (offset 0))
    (while (< offset (length data))
      (let ((mixed (make-vector 16 0)))
        (dotimes (i 16)
          (aset mixed i (logxor (aref data (+ offset i)) (aref iv i))))
        (setq iv (zhihu--zse-r-block mixed))
        (dotimes (i 16)
          (aset output (+ offset i) (aref iv i))))
      (setq offset (+ offset 16)))
    output))

(defun zhihu--zse-custom-encode (input)
  "用知乎 ZSE v4 的自定义字母表编码字节向量 INPUT。"
  (let* ((remainder (% (length input) 3))
         (padding (if (zerop remainder) 0 (- 3 remainder)))
         (bytes (make-vector (+ (length input) padding) 0))
         (index 0)
         (position (1- (+ (length input) padding)))
         chars)
    (dotimes (i (length input))
      (aset bytes i (aref input i)))
    (while (>= position 0)
      (let ((value 0))
        (dotimes (byte-index 3)
          (let* ((byte (aref bytes (- position byte-index)))
                 (mask (logand #xff (ash 58 (- (* 8 (% index 4)))))))
            (setq index (1+ index))
            (setq value
                  (logior value
                          (ash (logand #xff (logxor byte mask))
                               (* 8 byte-index))))))
        (dotimes (i 4)
          (push (aref zhihu--zse-alphabet
                      (logand 63 (ash value (- (* 6 i)))))
                chars)))
      (setq position (- position 3)))
    (concat (nreverse chars))))

(defun zhihu--zse-uri-unescaped-byte-p (byte)
  "BYTE 是否属于 JavaScript `encodeURIComponent' 不转义集。"
  (or (and (>= byte ?A) (<= byte ?Z))
      (and (>= byte ?a) (<= byte ?z))
      (and (>= byte ?0) (<= byte ?9))
      (memq byte '(?- ?_ ?. ?! ?~ ?* ?' ?\( ?\)))))

(defun zhihu--zse-encode-uri-component (input)
  "将 INPUT 按 JavaScript `encodeURIComponent' 编码成字节向量。"
  (let ((hex "0123456789ABCDEF")
        encoded)
    (dolist (byte (append (encode-coding-string input 'utf-8 t) nil))
      (if (zhihu--zse-uri-unescaped-byte-p byte)
          (push byte encoded)
        ;; 列表末尾会统一反转；此处按最终的 "%XX" 顺序 push。
        (push ?% encoded)
        (push (aref hex (ash byte -4)) encoded)
        (push (aref hex (logand byte #x0f)) encoded)))
    (vconcat (nreverse encoded))))

(defun zhihu--zse-v4-encrypt (input)
  "返回字符串 INPUT 的确定性 Zhihu ZSE v4 密文。"
  (let* ((input-bytes (zhihu--zse-encode-uri-component input))
         (unpadded-length (+ 2 (length input-bytes)))
         (padding (- 16 (% unpadded-length 16)))
         (plain (make-vector (+ unpadded-length padding) padding))
         (first (make-vector 16 0))
         cipher c0)
    ;; 使用固定种子，以得到经请求向量验证的确定性密文。
    (aset plain 0 12)
    (aset plain 1 0)
    (dotimes (i (length input-bytes))
      (aset plain (+ i 2) (aref input-bytes i)))
    (dotimes (i 16)
      (aset first i
            (logand #xff
                    (logxor (aref plain i) (aref zhihu--zse-key i) 42))))
    (setq c0 (zhihu--zse-r-block first)
          cipher (make-vector (length plain) 0))
    (dotimes (i 16)
      (aset cipher i (aref c0 i)))
    (when (> (length plain) 16)
      (let ((rest (zhihu--zse-x-blocks (cl-subseq plain 16) c0)))
        (dotimes (i (length rest))
          (aset cipher (+ i 16) (aref rest i)))))
    (zhihu--zse-custom-encode cipher)))

(defun zhihu--zse-path-and-query (url)
  "从 URL 取得 ZSE 签名使用的原样 path 和 query。"
  (let ((filename (url-filename (url-generic-parse-url url))))
    (cond
     ((string-empty-p filename) "/")
     ((string-prefix-p "?" filename) (concat "/" filename))
     (t filename))))

(defun zhihu--zse96-header (url dc0 &optional body)
  "为 URL、DC0 和可选 BODY 生成 `x-zse-96' 请求头值。"
  (let* ((source (mapconcat
                  #'identity
                  (delq nil (list zhihu--zse93
                                  (zhihu--zse-path-and-query url)
                                  dc0 body))
                  "+"))
         (digest (secure-hash
                  'md5 (encode-coding-string source 'utf-8 t))))
    (concat "2.0_" (zhihu--zse-v4-encrypt digest))))

(defun zhihu--zse-request-headers (url body dc0)
  "若 DC0 非空，返回 URL 相应的 ZSE 请求头。"
  (when (and (stringp dc0) (not (string-empty-p dc0)))
    `(("x-zse-93" . ,zhihu--zse93)
      ("x-zse-96" . ,(zhihu--zse96-header url dc0 body)))))

;;;; Requests

(defun zhihu--request-headers (&optional content-type cookie)
  "构造通用请求头 alist，可选加入 CONTENT-TYPE 与 COOKIE。"
  (let ((h `(("User-Agent" .
              "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36")
             ("Accept-Language" . "zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2")
             ("x-requested-with" . "fetch"))))
    (when content-type
      (push (cons "Content-Type" content-type) h))
    (when cookie
      (push (cons "Cookie" cookie) h))
    h))

(cl-defun zhihu--http
    (method url
            &key body content-type extra-headers raw-body sign-json xsrf-state
            with-zhihu-cookies)
  "同步发送 HTTP 请求并返回响应 plist。
METHOD 是 \"GET\" / \"POST\" / \"PATCH\" / \"PUT\" / \"DELETE\"。BODY 字符串可空。
EXTRA-HEADERS 追加到默认头；调用方不应重复已有的 header 名。
RAW-BODY 非 nil 时 BODY 直接当 unibyte 字节流发，不做 utf-8 编码。
SIGN-JSON 非 nil 时用本次 Cookie 读取中的 `d_c0' 生成 ZSE 请求头。
XSRF-STATE 非 nil 时用于发送并更新 `_xsrf'。
WITH-ZHIHU-COOKIES 非 nil 时从所选浏览器读取 URL 对应的 Cookie；默认不读取。"
  (when (and xsrf-state (not (zhihu--xsrf-state-p xsrf-state)))
    (error "zhihu: 无效的 XSRF state"))
  (let* ((cookie-host
          (and with-zhihu-cookies
               (url-host (url-generic-parse-url url))))
         (cookies
          (and cookie-host
               (zhihu--read-browser-cookies url)))
         (cookie
          (and cookie-host
               (zhihu--format-cookie-header
                cookies
                (and xsrf-state
                     (zhihu--xsrf-state-xsrf-token xsrf-state)))))
         (dc0 (cdr (assoc-string "d_c0" cookies)))
         (request-data (and body (if raw-body body
                                   (encode-coding-string body 'utf-8))))
         (request-headers
          (let ((base (zhihu--request-headers content-type cookie)))
            (dolist (h (and sign-json cookie-host
                            (zhihu--zse-request-headers url body dc0)))
              (push h base))
            (dolist (h extra-headers) (push h base))
            base))
         (method-symbol (intern (downcase method)))
         ;; plz 对非 2xx 会抛 `plz-error'；调用方仍需要看到原始 status/body，
         ;; 因此只取回这类错误中的 response。传输错误保持 plz 原始错误。
         (response
          (let ((plz-curl-default-args
                 (cons "--disable"
                       (cl-remove "--disable" plz-curl-default-args
                                  :test #'string=))))
            (condition-case err
                (plz method-symbol url
                  :headers request-headers
                  ;; JSON 与 OSS 都发送已经确定好的字节，避免 curl 改写换行。
                  :body request-data
                  :body-type 'binary
                  :as 'response
                  :then 'sync
                  :connect-timeout 30
                  :timeout 30)
              (plz-error
               (let ((data (cl-find-if #'plz-error-p (cdr err))))
                 (if-let* ((error-response
                           (and data (plz-error-response data))))
                     error-response
                   (signal (car err) (cdr err))))))))
         (headers (plz-response-headers response)))
    (when (and cookie-host xsrf-state)
      (zhihu--update-xsrf-state xsrf-state headers))
    (list :status (plz-response-status response)
          :headers headers
          :body (plz-response-body response))))

(cl-defun zhihu--http-json
    (method url &key body extra-headers (sign-json t) xsrf-state)
  "BODY plist 用 `json-serialize' 序列化后发送，响应 body 也按 JSON 解析。
XSRF-STATE 非 nil 时可能由响应更新。返回
(:status N :headers ALIST :json PARSED :body STRING)。"
  (let* ((data (and body
                    (json-serialize body
                                    :null-object :json-null
                                    :false-object :json-false)))
         ;; DATA 只序列化一次：签名和 `zhihu--http' 实际发送的是同一个字符串。
         (resp
          (zhihu--http
           method url
           :body data
           :content-type "application/json"
           :extra-headers extra-headers
           :sign-json sign-json
           :xsrf-state xsrf-state
           :with-zhihu-cookies t))
         (response-body (plist-get resp :body))
         (json (and response-body
                    (not (string-empty-p response-body))
                    (condition-case nil
                        (json-parse-string response-body
                                           :null-object :json-null
                                           :false-object :json-false
                                           :object-type 'plist)
                      (error nil)))))
    (append resp (list :json json))))

(defun zhihu--ensure-xsrf-token (xsrf-state)
  "确保 XSRF-STATE 含非空 token，并返回该 token。"
  (if-let* ((token (zhihu--xsrf-state-xsrf-token xsrf-state)))
      (if (string-empty-p token)
          (error "zhihu: XSRF state 中含空 token")
        token)
    (let ((resp
           (zhihu--http
            "GET" "https://www.zhihu.com/"
            :xsrf-state xsrf-state
            :with-zhihu-cookies t)))
      (unless (eq (plist-get resp :status) 200)
        (error "zhihu: 获取 XSRF token 失败 (HTTP %s)"
               (plist-get resp :status)))
      (or (zhihu--xsrf-state-xsrf-token xsrf-state)
          (error "zhihu: 知乎没有下发 XSRF token；请确认所选浏览器已登录知乎")))))

(cl-defun zhihu--zhuanlan-mutation-request
    (xsrf-state method url referer &key body)
  "以 XSRF-STATE 向 URL 发送专栏编辑器修改请求。
METHOD、REFERER 和可选 BODY 直接用于本次请求。"
  (zhihu--http-json
   method url
   :body body
   :extra-headers
   `(("x-xsrftoken" . ,(zhihu--ensure-xsrf-token xsrf-state))
     ("Referer" . ,referer)
     ("Origin" . "https://zhuanlan.zhihu.com"))
   :sign-json nil
   :xsrf-state xsrf-state))

;;;; Source metadata

;; URL and ID parsing

(defun zhihu--parse-id-or-url (s)
  "S 可以是裸数字 id（=question id）或知乎 URL。
返回包含 `:question-id' 以及可选 `:answer-id' 的 plist。"
  (cond
   ((string-match-p "^[0-9]+$" s)
    (list :question-id s))
   ((string-match
     "https?://[^/]*zhihu\\.com/question/\\([0-9]+\\)\\(?:/answer/\\([0-9]+\\)\\)?" s)
    (let ((result (list :question-id (match-string 1 s))))
      (if-let* ((answer-id (match-string 2 s)))
          (plist-put result :answer-id answer-id)
        result)))
   (t (error "zhihu: 不认识的 id/URL: %s" s))))

(declare-function org-collect-keywords "org" (keywords &optional unique directory))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-map "org-element" (data types fun &rest args))
(declare-function org-element-parse-buffer "org-element" (&rest args))
(declare-function org-element-property
                  "org-element" (property node &optional dflt force-undefer))
(declare-function org-element-type "org-element" (element))

(defvar zhihu--typst-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?/ ". 124bn" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "用于识别 Typst 字符串、注释和括号的 syntax table。")

(defun zhihu--typst-root (file)
  "返回能解析 FILE 全部绝对 import/include 的最近 Typst root。"
  (let* ((file (expand-file-name file))
         (dir (file-name-directory file))
         (abs-paths
          (with-temp-buffer
            (insert-file-contents file)
            (with-syntax-table zhihu--typst-syntax-table
              (goto-char (point-min))
              (let (ps)
                (while (re-search-forward
                        "^[ \t]*#\\(?:import\\|include\\)[ \t]+\"\\(/[^\"]+\\)\""
                        nil t)
                  (unless (nth 8
                               (save-excursion
                                 (syntax-ppss (match-beginning 0))))
                    (push (substring (match-string 1) 1) ps)))
                (delete-dups (nreverse ps)))))))
    (if (null abs-paths)
        (directory-file-name dir)
      ;; 所有路径必须由同一个候选目录解析。
      (let ((root
             (locate-dominating-file
              dir
              (lambda (candidate)
                (cl-every
                 (lambda (rel)
                   (file-exists-p (expand-file-name rel candidate)))
                 abs-paths)))))
        (unless root
          (error "zhihu: 找不到能解析全部绝对 import/include 的 Typst root"))
        (directory-file-name (expand-file-name root))))))

(defun zhihu--typst-query-metadata (file)
  "查询 FILE 的知乎 metadata、通用 `<banner>' 与原生 heading outline。
返回 `(:zhihu ZHIHU-OR-NIL :banner BANNER-OR-NIL :toc BOOLEAN)'。"
  (let* ((out
          (zhihu--shell-convert
           "typst"
           (list "eval" "--features" "html" "--target" "html"
                 "--root" (zhihu--typst-root file)
                 "--in" (expand-file-name file)
                 (concat
                  "let own = query(selector(metadata).and(<zhihu>)); "
                  "let banners = query(selector(metadata).and(<banner>)); "
                  "let tocs = query(outline).filter(it => { "
                  "let target = repr(it.target); "
                  "target == \"heading\" or "
                  "target.starts-with(\"heading.where(\") }); "
                  "(zhihu-count: own.len(), "
                  "zhihu-value: if own.len() == 1 "
                  "{ own.first().value } else { none }, "
                  "banner-count: banners.len(), "
                  "banner-value: if banners.len() == 1 "
                  "{ banners.first().value } else { none }, "
                  "toc-count: tocs.len())"))
           ""))
         (result
          (condition-case err
              (json-parse-string out :object-type 'plist
                                 :array-type 'array
                                 :null-object :json-null
                                 :false-object :json-false)
            (error
             (error "zhihu: typst eval 输出非合法 JSON: %s\n%s"
                    (error-message-string err) out)))))
    (unless (and
             (listp result)
             (cl-every
              (lambda (field) (plist-member result field))
              '(:zhihu-count :zhihu-value
                :banner-count :banner-value
                :toc-count)))
      (error "zhihu: typst eval 返回的 metadata 查询结构无效"))
    (let ((zhihu
           (pcase (plist-get result :zhihu-count)
             (0 nil)
             (1
              (let ((value (plist-get result :zhihu-value)))
                (unless (and (listp value) value)
                  (error
                   "zhihu: Typst <zhihu> metadata 必须是非空 dictionary"))
                value))
             (_ (error "zhihu: 文件中有多个 <zhihu> metadata 块"))))
          (banner
           (pcase (plist-get result :banner-count)
             (0 nil)
             (1
              (or
               (zhihu--normalize-metadata-string
                "banner" (plist-get result :banner-value))
               (error
                "zhihu: banner 不能为空；未设置时请删除该字段")))
             (_ (error "zhihu: 文件中有多个 <banner> metadata 块"))))
          (toc-count (plist-get result :toc-count)))
      (unless (and (integerp toc-count) (>= toc-count 0))
        (error "zhihu: typst eval 返回的 outline 数量无效"))
      (list :zhihu zhihu :banner banner :toc (> toc-count 0)))))

(defun zhihu--value-string (value)
  "把字符串或整数 VALUE 归一化成非空字符串。"
  (let ((s (cond ((stringp value) value)
                 ((integerp value) (number-to-string value)))))
    (and s (not (string-empty-p s)) s)))

(defun zhihu--normalize-metadata-id-string (field value)
  "把 metadata 的 FIELD/VALUE 归一化成数字 ID 字符串或 nil。"
  (cond
   ((memq value '(nil :null :json-null)) nil)
   ((integerp value)
    (if (>= value 0)
        (number-to-string value)
      (error "zhihu: %s 必须是非负整数或数字字符串" field)))
   ((stringp value)
    (let ((value (string-trim value)))
      (cond
       ((string-empty-p value) nil)
       ((string-match-p "\\`[0-9]+\\'" value) value)
       (t (error "zhihu: %s 必须是非负整数或数字字符串" field)))))
   (t (error "zhihu: %s 必须是非负整数或数字字符串" field))))

(defun zhihu--normalize-metadata-string (field value)
  "把 metadata 的 FIELD/VALUE 归一化成非空字符串或 nil。"
  (cond
   ((memq value '(nil :null :json-null)) nil)
   ((stringp value)
    (let ((value (string-trim value)))
      (unless (string-empty-p value) value)))
   (t (error "zhihu: %s 必须是字符串" field))))

(defun zhihu--normalize-metadata-boolean (field value)
  "把 metadata 的 FIELD/VALUE 归一化成布尔值；非法值报错。"
  (cond
   ((eq value t) t)
   ((memq value '(nil :false :json-false :null :json-null)) nil)
   ((stringp value)
    (pcase (downcase (string-trim value))
      ("true" t)
      ("false" nil)
      (_ (error "zhihu: %s 必须是 true 或 false" field))))
   (t (error "zhihu: %s 必须是 true 或 false" field))))

(defconst zhihu--creation-statement-api-types
  '("spoiler"
    "medical_advice"
    "fictional_creation"
    "contain_finance"
    "ai_creation")
  "稿件 `creation-statement' 可直接使用的知乎 API 类型。")

(defconst zhihu--content-source-api-channels
  '(("officialwebsite" . "officialWebsite")
    ("newsreport" . "newsReport")
    ("tvmedia" . "TVMedia")
    ("printmedia" . "printMedia"))
  "稿件 `content-source' 的小写输入与知乎 API channel 对照。")

(defconst zhihu--reprint-permission-api-values
  '("allowed" "disallowed" "need_payment")
  "稿件 `reprint-permission' 可直接使用的知乎 API 值。")

(defconst zhihu--comment-permission-api-values
  '("all" "censor" "followee" "nobody")
  "稿件 `comment-permission' 可直接使用的知乎 API 值。")

(defun zhihu--normalize-optional-enum (field value values)
  "按 VALUES 把 metadata 的 FIELD/VALUE 归一化成字符串或 nil。"
  (cond
   ((memq value '(nil :null :json-null)) nil)
   ((stringp value)
    (let ((value (downcase (string-trim value))))
      (cond
       ((string-empty-p value) nil)
       ((member value values) value)
       (t
        (error "zhihu: %s 必须为 %s"
               field
               (mapconcat #'identity values "、"))))))
   (t
    (error "zhihu: %s 必须为 %s"
           field
           (mapconcat #'identity values "、")))))

(defun zhihu--required-enum (field value values)
  "按 VALUES 归一化必填的 FIELD/VALUE；空值或非法值报错。"
  (or (zhihu--normalize-optional-enum field value values)
      (error "zhihu: %s 必须为 %s"
             field
             (mapconcat #'identity values "、"))))

(defun zhihu--normalize-metadata-creation-statement (value)
  "把 metadata 的创作声明 VALUE 归一化成字符串或 nil。
VALUE 直接使用知乎 API 类型；nil 或空字符串表示无创作声明。"
  (zhihu--normalize-optional-enum
   "creation-statement" value zhihu--creation-statement-api-types))

(defun zhihu--normalize-metadata-content-source (value)
  "把 metadata 的内容来源 VALUE 归一化为知乎 API channel 或 nil。"
  (when-let* ((value
              (zhihu--normalize-metadata-string "content-source" value)))
    (or
     (cdr
      (assoc-string
       (downcase value) zhihu--content-source-api-channels))
     (error
      "zhihu: content-source 必须为 %s"
      (mapconcat #'cdr zhihu--content-source-api-channels "、")))))

(defconst zhihu--article-topic-limit 3
  "知乎文章编辑器当前允许绑定的话题数上限。")

(defun zhihu--normalize-topic-name (value)
  "把 VALUE 归一化成可持久化的话题名称。"
  (unless (stringp value)
    (error "zhihu: 话题名称必须是字符串"))
  (let ((name (string-trim value)))
    (when (or (string-empty-p name)
              (string-match-p "[[:cntrl:]]" name))
      (error "zhihu: 话题名称不能为空或包含控制字符"))
    name))

(defun zhihu--normalize-metadata-topics
    (object &optional limit kind-name)
  "把 metadata 的 OBJECT 归一化成话题名称列表。
OBJECT 必须是至多 LIMIT 个互不重复的非空字符串组成的 sequence。
LIMIT 缺省为 `zhihu--article-topic-limit'；KIND-NAME 用于错误信息。"
  (setq limit (or limit zhihu--article-topic-limit)
        kind-name (or kind-name "文章"))
  (let ((topics
         (cond
          ((vectorp object) (append object nil))
          ((and (listp object) (proper-list-p object)) object)
          (t (error "zhihu: topics 必须是字符串 sequence"))))
        normalized)
    (when (> (length topics) limit)
      (error "zhihu: %s最多选择 %d 个话题" kind-name limit))
    (dolist (value topics)
      (let ((name (zhihu--normalize-topic-name value)))
        (when (member name normalized)
          (error "zhihu: topics 不能包含重复话题：%s" name))
        (push name normalized)))
    (nreverse normalized)))

(defconst zhihu--metadata-kind-scalar-fields
  '((answer :question-id :answer-id)
    (article :article-id :column-id))
  "各类稿件专属的标量 metadata 字段，按输出顺序排列。")

(defconst zhihu--metadata-empty-id-slot-fields
  '(:article-id)
  "允许以空值保留、等待首次发布写回的内容 ID 字段。")

(defconst zhihu--metadata-common-scalar-fields
  '(:creation-statement :content-source
    :reprint-permission :comment-permission)
  "各类稿件共用的可选标量 metadata 字段，按输出顺序排列。")

(defconst zhihu--metadata-scalar-fields
  (append (cdr (assq 'answer zhihu--metadata-kind-scalar-fields))
          (cdr (assq 'article zhihu--metadata-kind-scalar-fields))
          zhihu--metadata-common-scalar-fields)
  "全部标量 metadata 字段；用于格式适配器枚举字段。")

(defconst zhihu--metadata-fields
  (append zhihu--metadata-scalar-fields '(:topics))
  "全部知乎渠道 metadata 字段；用于编辑辅助识别字段名。")

(defun zhihu--zhihu-meta-from-plist (z)
  "把 Z 归一化成发布流程使用的知乎渠道 metadata plist。
数字 ID 会可靠转换为字符串。question-id 与 article-id 必须且只能
显式出现一个，分别表示回答与文章。article-id 允许保留空槽，
等待首次发布写回；其它未设置的字段应当缺席。"
  (when
      (or
       (plist-member z :toc)
       (plist-member z :enable-table-of-contents))
    (error "zhihu: toc 是通用目录请求，不能写入知乎渠道 metadata"))
  (dolist (field (append '(:topics)
                         zhihu--metadata-scalar-fields))
    (when (plist-member z field)
      (let ((value (plist-get z field)))
        (when (and
               (or (memq value '(nil :null :json-null))
                   (and (stringp value)
                        (string-empty-p (string-trim value)))
                   (and (eq field :topics)
                        (or (and (vectorp value)
                                 (zerop (length value)))
                            (and (listp value) (null value)))))
               (not (memq field zhihu--metadata-empty-id-slot-fields)))
          (error "zhihu: %s 不能为空；未设置时请删除该字段"
                 (substring (symbol-name field) 1))))))
  (let* ((question-id-present-p
          (and (plist-member z :question-id) t))
         (answer-id-present-p
          (and (plist-member z :answer-id) t))
         (article-id-present-p
          (and (plist-member z :article-id) t))
         (column-id-present-p
          (and (plist-member z :column-id) t))
         (identity-kinds
          (delq nil
                (list
                 (and question-id-present-p 'answer)
                 (and article-id-present-p 'article))))
         (_identity-check
          (unless (= (length identity-kinds) 1)
            (error
             "zhihu: metadata 必须且只能包含 question-id 或 article-id 中的一个")))
         (qid (zhihu--normalize-metadata-id-string
               "question-id" (plist-get z :question-id)))
         (aid (zhihu--normalize-metadata-id-string
               "answer-id" (plist-get z :answer-id)))
         (art (zhihu--normalize-metadata-id-string
               "article-id" (plist-get z :article-id)))
         (column
          (zhihu--normalize-metadata-string
           "column-id" (plist-get z :column-id)))
         (kind (car identity-kinds))
         (topics
          (zhihu--normalize-metadata-topics
           (plist-get z :topics)
           zhihu--article-topic-limit
           "文章"))
         (creation-statement
          (zhihu--normalize-metadata-creation-statement
           (plist-get z :creation-statement)))
         (content-source
          (zhihu--normalize-metadata-content-source
           (plist-get z :content-source)))
         (reprint-permission
          (zhihu--normalize-optional-enum
           "reprint-permission"
           (plist-get z :reprint-permission)
           zhihu--reprint-permission-api-values))
         (comment-permission
          (zhihu--normalize-optional-enum
           "comment-permission"
           (plist-get z :comment-permission)
           zhihu--comment-permission-api-values)))
    (when (and answer-id-present-p (not question-id-present-p))
      (error "zhihu: answer-id 必须与 question-id 一起出现"))
    (when (and column-id-present-p (not article-id-present-p))
      (error "zhihu: column-id 必须与 article-id 一起出现"))
    (when (and topics (not (eq kind 'article)))
      (error "zhihu: topics 只允许用于文章"))
    (append
     (list :kind kind)
     (when question-id-present-p
       (list :question-id qid))
     (when answer-id-present-p
       (list :answer-id aid))
     (when article-id-present-p
       (list :article-id art))
     (when column-id-present-p
       (list :column-id column))
     (list :topics topics
           :creation-statement creation-statement
           :content-source content-source
           :reprint-permission reprint-permission
           :comment-permission comment-permission))))

(defun zhihu--source-meta-from-parts (z title banner &optional toc)
  "合并通用文档字段 TITLE/BANNER、目录请求 TOC 与知乎渠道字段 Z。
这些值始终来自各格式自身的文档结构；这里只把它们与渠道字段拼成发布流程
使用的扁平上下文。"
  (append
   (list :title (zhihu--normalize-metadata-string "title" title)
         :banner (zhihu--normalize-metadata-string "banner" banner)
         :toc (zhihu--normalize-metadata-boolean "toc" toc))
   (zhihu--zhihu-meta-from-plist z)))

(defun zhihu--metadata-field-name (field)
  "把 metadata 关键字 FIELD 转成不带冒号的字段名。"
  (substring (symbol-name field) 1))

(defun zhihu--metadata-scalar-entries (meta)
  "从统一 META 返回应持久化的有序 (FIELD . VALUE) 标量列表。
nil 字段会被省略，但显式存在的空 article-id 会保留。"
  (let* ((kind (plist-get meta :kind))
         (kind-fields
          (cdr (assq kind zhihu--metadata-kind-scalar-fields)))
         (identity-field
         (pcase kind
            ('answer :question-id)
            ('article :article-id))))
    (unless kind-fields
      (error "zhihu: 未知 metadata kind: %S" kind))
    (unless (plist-member meta identity-field)
      (error "zhihu: %s metadata 缺少 %s 字段"
             (pcase kind
               ('answer "回答")
               ('article "文章"))
             (zhihu--metadata-field-name identity-field)))
    (when (and (eq kind 'answer)
               (not (plist-get meta :question-id)))
      (error "zhihu: 回答 metadata 的 question-id 不能为空"))
    (dolist (fields zhihu--metadata-kind-scalar-fields)
      (dolist (field (cdr fields))
        (when (and (plist-member meta field)
                   (not (memq field kind-fields)))
          (error "zhihu: %s metadata 不支持 %s"
                 kind (zhihu--metadata-field-name field)))))
    (cl-loop
     for field in
     (append
      kind-fields
     zhihu--metadata-common-scalar-fields)
     for value = (plist-get meta field)
     when (or value
              (and (memq field zhihu--metadata-empty-id-slot-fields)
                   (plist-member meta field)))
     collect (cons field value))))

(defun zhihu--format-topics-typst (topics)
  "把非空 TOPICS 名称列表格式化成多行 Typst array。"
  (concat "(\n"
          (mapconcat
           (lambda (name) (format "    %S," name))
           topics
           "\n")
          "\n  )"))

(defun zhihu--format-typst-zhihu-metadata (meta)
  "生成 META 对应的 Typst `<zhihu>' metadata 块。"
  (let* ((topics (plist-get meta :topics))
         (lines
          (mapcar
           (pcase-lambda (`(,field . ,value))
             (format "  %s: %s,"
                     (zhihu--metadata-field-name field)
                     (cond
                      ((null value) "none")
                      ((eq value t) "true")
                      (t (format "%S" value)))))
           (zhihu--metadata-scalar-entries meta))))
    (when topics
      (setq lines
            (append
             lines
             (list
              (format "  topics: %s,"
                      (zhihu--format-topics-typst topics))))))
    (if lines
        (concat "#metadata((\n"
                (mapconcat #'identity lines "\n")
                "\n)) <zhihu>\n")
      "")))

(defun zhihu--typst-native-metadata-region ()
  "返回唯一的 `#metadata(...) <zhihu>' 调用区域 (BEG . END)。
允许 metadata 单行或多行排版，并忽略字符串和 Typst 注释中的伪调用。"
  (save-excursion
    (with-syntax-table zhihu--typst-syntax-table
      (let ((case-fold-search nil)
            (parse-sexp-ignore-comments t)
            regions)
        (goto-char (point-min))
        (while (re-search-forward "#metadata\\_>" nil t)
          (let ((beg (match-beginning 0))
                (resume (match-end 0)))
            (unless (nth 8 (syntax-ppss beg))
              (goto-char resume)
              (forward-comment (point-max))
              (when (eq (char-after) ?\()
                (condition-case nil
                    (let ((call-end (scan-sexps (point) 1)))
                      (goto-char call-end)
                      (forward-comment (point-max))
                      (when (looking-at "<zhihu>")
                        (let ((end (match-end 0)))
                          (goto-char end)
                          (skip-chars-forward " \t")
                          (when (eq (char-after) ?\n)
                            (forward-char 1))
                          (setq end (point))
                          (save-excursion
                            (goto-char beg)
                            (let ((bol (line-beginning-position)))
                              (when (string-match-p
                                     "\\`[ \t]*\\'"
                                     (buffer-substring-no-properties bol beg))
                                (setq beg bol))))
                          (push (cons beg end) regions))))
                  (scan-error nil))))
            (goto-char (max resume (point)))))
        (pcase regions
          ('nil nil)
          (`(,region) region)
          (_ (error "zhihu: 文件中有多个 <zhihu> metadata 块")))))))

(defun zhihu--typst-write-native-metadata (file meta)
  "把 META 作为模板无关的 `<zhihu>' metadata 安全写入 FILE。"
  (let ((block (zhihu--format-typst-zhihu-metadata meta)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((region (zhihu--typst-native-metadata-region)))
        (cond
         (region
          (delete-region (car region) (cdr region))
          (goto-char (car region))
          (insert block))
         ;; 不猜测其它手写表达式的边界，避免误删正文。
         ((save-excursion
            (with-syntax-table zhihu--typst-syntax-table
              (goto-char (point-min))
              (cl-loop while (re-search-forward "<zhihu>" nil t)
                       thereis
                       (not (nth 8
                                 (save-excursion
                                   (syntax-ppss (match-beginning 0))))))))
         (error "zhihu: <zhihu> 不是标准 #metadata((...)) 块，拒绝自动重写"))
         (t
          (unless (string-empty-p block)
            (goto-char (point-min))
            (insert block "\n")))))
      (write-region (point-min) (point-max) file nil 'silent))))




























;; Org metadata
;;
;; 标题和题图使用标准风格的 `#+TITLE:'、`#+BANNER:'；知乎渠道状态使用包自己的
;; 关键字：
;;   #+ZHIHU_QUESTION_ID: 123
;;   #+ZHIHU_ANSWER_ID: 456
;; 或：
;;   #+ZHIHU_ARTICLE_ID: 789
;;   #+ZHIHU_COLUMN_ID: hackers
;; QUESTION_ID、ARTICLE_ID 与 THOUGHT_ID 必须且只能出现一个。ARTICLE_ID/THOUGHT_ID
;; 可以保留空槽，等待首次发布写回；其它知乎关键字出现时必须有非空值。
;; 话题是单行 JSON array。

(defun zhihu--org-metadata-keyword (field)
  "把 metadata 关键字 FIELD 转成对应的 Org 关键字名。"
  (concat
   "ZHIHU_"
   (upcase
    (replace-regexp-in-string
     "-" "_" (zhihu--metadata-field-name field) t t))))

(defconst zhihu--org-keyword-names
  (mapcar #'zhihu--org-metadata-keyword
          (append zhihu--metadata-scalar-fields
                  '(:topics)))
  "本包拥有的 Org 文档级关键字。")

(defconst zhihu--org-empty-id-keyword-names
  (mapcar #'zhihu--org-metadata-keyword
          zhihu--metadata-empty-id-slot-fields)
  "允许保留空值、等待首次发布写回的 Org 关键字。")

(defun zhihu--org-collect-keywords (&optional names)
  "收集当前 buffer 的 NAMES 文档级关键字。
NAMES 缺省为本包拥有的知乎关键字。
忽略源码块里的同名文本；重复值立即报错，只有内容 ID 槽允许为空。
返回 (KEY . VALUE) alist。"
  (require 'org)
  (unless (derived-mode-p 'org-mode)
    (delay-mode-hooks (org-mode)))
  (mapcar
   (lambda (entry)
     (let ((key (car entry))
           (values (cdr entry)))
       (unless (= (length values) 1)
         (error "zhihu: Org 关键字 %s 不能重复" key))
       (let ((value (string-trim (car values))))
         (when (and (string-empty-p value)
                    (not (member key zhihu--org-empty-id-keyword-names)))
           (error "zhihu: Org 关键字 %s 不能为空" key))
         (cons key value))))
   (org-collect-keywords (or names zhihu--org-keyword-names))))

(defun zhihu--org-topics (raw)
  "把 Org 关键字里的 RAW JSON array 解析为话题名称 sequence。"
  (let ((topics
         (condition-case nil
             (json-parse-string raw
                                :array-type 'array
                                :null-object :json-null
                                :false-object :json-false)
           (error nil))))
    (unless (vectorp topics)
      (error "zhihu: ZHIHU_TOPICS 必须是合法 JSON array"))
    topics))

(defun zhihu--org-toc-enabled-p (raw)
  "把 Org 原生 `#+TOC' 的 RAW 值规范化为是否启用文章目录。"
  (when raw
    (let ((case-fold-search t))
      (unless
          (string-match-p
           "\\`headlines\\(?:[ \t]+[1-3]\\)?\\'"
           raw)
        (error
         "zhihu: Org 目录只支持 #+TOC: headlines 1..3")))
    t))

(defun zhihu--org-read-meta (file)
  "从 Org FILE 读取通用文档 metadata 与知乎 metadata。"
  (with-temp-buffer
    (insert-file-contents file)
    (require 'org)
    (delay-mode-hooks (org-mode))
    (let* ((keywords
            (zhihu--org-collect-keywords
             (append
              '("BANNER" "TOC")
              zhihu--org-keyword-names)))
           (title
            (cdr
             (assoc-string
              "TITLE"
              (org-collect-keywords '("TITLE") '("TITLE"))
              t)))
           (banner
            (cdr (assoc-string "BANNER" keywords t)))
           (toc
            (zhihu--org-toc-enabled-p
             (cdr (assoc-string "TOC" keywords t))))
           z)
      (dolist (field zhihu--metadata-scalar-fields)
        (when-let*
            ((value
              (cdr
               (assoc-string
                (zhihu--org-metadata-keyword field) keywords t))))
          (setq z (plist-put z field value))))
      (when-let*
          ((raw
            (cdr
             (assoc-string
              (zhihu--org-metadata-keyword :topics) keywords t))))
        (setq z (plist-put z :topics (zhihu--org-topics raw))))
      (zhihu--source-meta-from-parts z title banner toc))))

(defun zhihu--format-org-zhihu-metadata (meta)
  "生成 META 对应的 Org `#+ZHIHU_*' metadata 行。"
  (let* ((topics (plist-get meta :topics))
         (lines
         (mapcar
           (pcase-lambda (`(,field . ,value))
             (if (null value)
                 (format "#+%s:"
                         (zhihu--org-metadata-keyword field))
               (format "#+%s: %s"
                       (zhihu--org-metadata-keyword field)
                       (if (eq value t) "true" value))))
           (zhihu--metadata-scalar-entries meta))))
    (when topics
      (setq lines
            (append
             lines
             (list
              (format
               "#+%s: %s"
               (zhihu--org-metadata-keyword :topics)
               (json-serialize (vconcat topics)
                               :null-object :json-null
                               :false-object :json-false))))))
    (if lines
        (concat (mapconcat #'identity lines "\n") "\n")
      "")))

(defun zhihu--org-write-zhihu-meta (file meta)
  "把 META 中的知乎渠道字段写入 Org FILE。"
  (let ((block (zhihu--format-org-zhihu-metadata meta)))
    (with-temp-buffer
      (insert-file-contents file)
      (require 'org)
      (delay-mode-hooks (org-mode))
      (let (regions)
        (org-element-map (org-element-parse-buffer) 'keyword
			 (lambda (node)
			   (when (member (org-element-property :key node)
					 zhihu--org-keyword-names)
			     (let ((begin (org-element-property :begin node)))
			       (push (cons begin
					   (save-excursion
					     (goto-char begin)
					     (line-beginning-position 2)))
				     regions)))))
        (dolist (region (sort regions (lambda (a b) (> (car a) (car b)))))
          (delete-region (car region) (cdr region))))
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (looking-at "^#\\+[[:alnum:]_]+:.*\n")
          (goto-char (match-end 0))))
      (insert block)
      (write-region (point-min) (point-max) file nil 'silent))))

;; Metadata dispatch

;; Metadata dispatch

(defun zhihu--file-format (file)
  "返回 FILE 对应的 `typst' 或 `org'；其它返回 nil。"
  (pcase (downcase (or (file-name-extension file) ""))
    ("typ" 'typst)
    ("org" 'org)
    (_ nil)))

(defun zhihu--read-source-meta (file)
  "从 FILE 读取通用文档 metadata 与知乎渠道 metadata。
question-id 与 article-id 必须且只能出现一个，分别表示回答与文章。"
  (cl-ecase (zhihu--file-format file)
    (typst
     (let ((source-meta (zhihu--typst-query-metadata file)))
       (zhihu--source-meta-from-parts
        (plist-get source-meta :zhihu)
        nil
        (plist-get source-meta :banner)
        (plist-get source-meta :toc))))
    (org (zhihu--org-read-meta file))))

(defun zhihu--write-zhihu-meta (file meta)
  "把 META 中的知乎渠道状态写回 FILE。"
  (cl-ecase (zhihu--file-format file)
    (typst (zhihu--typst-write-native-metadata file meta))
    (org (zhihu--org-write-zhihu-meta file meta))))

;;;; HTML conversion

;; External process execution

(defun zhihu--shell-convert (program args input)
  "对 INPUT 跑 PROGRAM ARGS，返回 stdout。非零 exit 抛错。"
  ;; `call-process-region' 的 STDERR-FILE 只能是文件名/t/nil，不能是 buffer。
  ;; 单独捕获 stderr 也避免 Typst 的实验功能 warning 污染 stdout/JSON。
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

(defun zhihu--typst-footnote-inline-nodes (definition)
  "从 Typst 脚注 DEFINITION 返回去掉 backlink 的单段 inline 节点。"
  (let* ((children (zhihu--dom-trim-blank-children definition))
         (paragraph-p
          (and (= (length children) 1)
               (consp (car children))
               (eq (dom-tag (car children)) 'p)))
         (inline
          (zhihu--dom-trim-blank-children
           (if paragraph-p (car children) definition)))
         (backlink (car inline)))
    (unless
        (and backlink
             (consp backlink)
             (eq (dom-tag backlink) 'sup)
             (equal (dom-attr backlink 'role) "doc-backlink"))
      (error "zhihu: Typst 脚注定义缺少标准 backlink"))
    (zhihu--single-fragment-link-target backlink "Typst 脚注 backlink ")
    (cdr inline)))

(defun zhihu--typst-footnote-reference-data (definition)
  "从 Typst 脚注 DEFINITION 提取知乎引用的 (:text ... :url ...)。"
  (let ((nodes (zhihu--typst-footnote-inline-nodes definition))
        (link-count 0)
        (url ""))
    (cl-labels
        ((validate
          (node)
          (cond
           ((stringp node) nil)
           ((not (consp node))
            (error "zhihu: Typst 脚注包含无法识别的节点"))
           ((eq (dom-tag node) 'a)
            (cl-incf link-count)
            (when (> link-count 1)
              (error "zhihu: 一条 Typst 脚注最多只能包含一个链接"))
            (setq url (dom-attr node 'href))
            (mapc #'validate (dom-children node)))
           ((memq (dom-tag node)
                  '(em strong b i s strike del code span small sub sup mark))
            (mapc #'validate (dom-children node)))
           (t
            (error "zhihu: Typst 脚注不支持 %s 节点"
                   (dom-tag node))))))
      (mapc #'validate nodes))
    (zhihu--reference-data
     (mapconcat
      (lambda (node)
        (if (stringp node)
            node
          (dom-inner-text node)))
      nodes "")
     url "Typst 脚注")))

(defun zhihu--typst-noteref-target (node)
  "从 Typst 的 doc-noteref NODE 返回 definition fragment ID。"
  (unless (eq (dom-tag node) 'sup)
    (error "zhihu: Typst doc-noteref 必须是 sup 节点"))
  (zhihu--single-fragment-link-target node "Typst doc-noteref "))

(defun zhihu--typst-footnote-definitions (endnotes)
  "索引 ENDNOTES 中的 Typst 脚注定义并返回 hash table。"
  (let ((definitions (make-hash-table :test 'equal)))
    (dolist (definition (dom-by-tag endnotes 'li))
      (let ((id (dom-attr definition 'id)))
        (unless (and (stringp id) (not (string-empty-p id)))
          (error "zhihu: Typst 脚注定义缺少 id"))
        (when (gethash id definitions)
          (error "zhihu: Typst 脚注定义 id 重复：%s" id))
        (puthash id definition definitions)))
    definitions))

(defun zhihu--reference-marker (data numero)
  "从已校验的引用 DATA 和正整数 NUMERO 构造内部 marker。"
  `(span
    ((data-zhihu-reference . "true")
     (data-text . ,(plist-get data :text))
     (data-url . ,(plist-get data :url))
     (data-numero . ,(number-to-string numero)))
    ,(format "[%d]" numero)))

(defun zhihu--typst-rewrite-footnotes (html)
  "把 Typst HTML 中的原生脚注改写为 Pandoc 可保留的引用 marker。"
  (let* ((dom (zhihu--parse-html html))
         (sentinels
          (dom-search
           dom
           (lambda (node)
             (and (consp node)
                  (assq 'data-zhihu-reference (cadr node))))))
         (endnotes
          (zhihu--dom-nodes-with-attribute dom 'role "doc-endnotes"))
         (noterefs
          (zhihu--dom-nodes-with-attribute dom 'role "doc-noteref")))
    (when sentinels
      (error "zhihu: Typst HTML 不能预先包含内部引用 marker"))
    (if (and (null endnotes) (null noterefs))
        html
      (unless (= (length endnotes) 1)
        (error "zhihu: Typst HTML 必须只包含一个 doc-endnotes section"))
      (let ((endnotes (car endnotes)))
        (unless (and (eq (dom-tag endnotes) 'section) noterefs)
          (error "zhihu: Typst doc-endnotes 与 doc-noteref 结构不完整"))
        (let ((definitions
               (zhihu--typst-footnote-definitions endnotes))
              (reference-data (make-hash-table :test 'equal))
              (numbers (make-hash-table :test 'equal))
              (next-number 0))
          (cl-labels
              ((rewrite
                (node)
                (cond
                 ((stringp node) node)
                 ((not (consp node)) node)
                 ((eq node endnotes) nil)
                 ((equal (dom-attr node 'role) "doc-noteref")
                  (let* ((target (zhihu--typst-noteref-target node))
                         (definition (gethash target definitions)))
                    (unless definition
                      (error "zhihu: Typst 脚注引用缺少定义：%s" target))
                    (let ((data
                           (or
                            (gethash target reference-data)
                            (puthash
                             target
                             (zhihu--typst-footnote-reference-data definition)
                             reference-data))))
                      (let ((numero
                             (or
                              (gethash target numbers)
                              (puthash
                               target (cl-incf next-number) numbers))))
                        (zhihu--reference-marker data numero)))))
                 (t
                  (cons
                   (dom-tag node)
                   (cons
                    (copy-tree (cadr node))
                    (delq nil
                          (mapcar #'rewrite (dom-children node)))))))))
            (zhihu--outer-html (rewrite dom))))))))

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



(defun zhihu--h-cite-node-p (node)
  "NODE 是否是 Microformats2 `h-cite' 根节点。"
  (and (consp node) (zhihu--node-has-class-p node "h-cite")))

(defun zhihu--h-cite-property-nodes (node property)
  "返回 NODE 内具有 Microformats2 PROPERTY class 的元素。"
  (dom-search
   node
   (lambda (child)
     (and
      (consp child)
      (zhihu--node-has-class-p child property)))))

(defun zhihu--single-h-cite-child (node)
  "返回 NODE 唯一的 `h-cite' 子节点，否则返回 nil。"
  (pcase (zhihu--dom-significant-children node)
    (`(,child)
     (and (zhihu--h-cite-node-p child) child))))

(defun zhihu--validate-h-cite-placement (body)
  "确保 BODY 中的 `h-cite' 只作为顶层独立内容出现。"
  (let (allowed)
    (dolist (child (zhihu--dom-significant-children body))
      (cond
       ((zhihu--h-cite-node-p child)
        (push child allowed))
       ((and (consp child) (eq (dom-tag child) 'p))
        (when-let* ((cite (zhihu--single-h-cite-child child)))
          (push cite allowed)))))
    (dolist (node (dom-search body #'zhihu--h-cite-node-p))
      (unless (memq node allowed)
        (error "zhihu: h-cite 卡片必须是文档顶层的独立内容")))))

(defun zhihu--h-cite-data (node)
  "严格读取 Microformats2 `h-cite' NODE 的 URL 与标题。"
  (let ((url-nodes (zhihu--h-cite-property-nodes node "u-url"))
        (name-nodes (zhihu--h-cite-property-nodes node "p-name")))
    (unless (= (length url-nodes) 1)
      (error "zhihu: h-cite 卡片必须只包含一个 u-url"))
    (unless (= (length name-nodes) 1)
      (error "zhihu: h-cite 卡片必须只包含一个 p-name"))
    (let* ((url-node (car url-nodes))
           (name-node (car name-nodes))
           (url
            (and (eq (dom-tag url-node) 'a)
                 (dom-attr url-node 'href)))
           (title
            (string-trim
             (replace-regexp-in-string
              "[[:space:]]+" " " (dom-inner-text name-node)))))
      (unless (zhihu--web-url-p url)
        (error
         "zhihu: h-cite 的 u-url 必须是含 host 的 HTTP(S) 链接"))
      (when (string-empty-p title)
        (error "zhihu: h-cite 的 p-name 不能为空"))
      (list :url url :title title))))

(defun zhihu--native-link-card-node (node)
  "把 Microformats2 `h-cite' NODE 转成知乎原生 link-card。"
  (let* ((data (zhihu--h-cite-data node))
         (url (plist-get data :url))
         (title (plist-get data :title)))
    `(a ((href . ,url)
         (data-draft-node . "block")
         (data-draft-type . "link-card")
         (data-draft-title . ,title)
         (data-draft-cover . ""))
        ,title)))

(defun zhihu--math-span-p (node)
  "NODE 是否是 pandoc 输出的 math span。"
  (and (consp node)
       (eq (dom-tag node) 'span)
       (zhihu--node-has-class-p node "math")))

(defun zhihu--math-span-tex (node)
  "从 pandoc math span NODE 取出 TeX，去掉 \\(…\\) / \\[…\\]。"
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
   ((zhihu--h-cite-node-p node)
    (zhihu--native-link-card-node node))
   ((and
     (eq (dom-tag node) 'p)
     (zhihu--single-h-cite-child node))
    (zhihu--native-link-card-node
     (zhihu--single-h-cite-child node)))
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
    (zhihu--validate-h-cite-placement body)
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

(defun zhihu--normalize-html (html)
  "用 Pandoc 规范化 Typst HTML，再转换为知乎方言 HTML。"
  (zhihu--zhihuify-html
   (zhihu--shell-convert
    "pandoc"
    '("-f" "html" "-t" "html5" "--mathjax" "--wrap=none"
      "--no-highlight")
    (zhihu--typst-rewrite-footnotes html))))



(defun zhihu--org->html (org-text)
  "ORG-TEXT → 知乎方言 HTML。"
  (let ((org-export-use-babel nil)
        (org-confirm-babel-evaluate t)
        (org-export-with-section-numbers nil)
        ;; 禁用自动目录；显式 #+TOC 仍由 ox-html 导出。
        (org-export-with-toc nil)
        (org-html-htmlize-output-type nil)
        (org-html-head-include-default-style nil)
        (org-html-head-include-scripts nil)
        (org-export-filter-latex-fragment-functions
         '(zhihu--org-html-latex-fragment-filter))
        (org-export-filter-latex-environment-functions
         '(zhihu--org-html-latex-environment-filter)))
    (with-temp-buffer
      (insert org-text)
      (delay-mode-hooks (org-mode))
      (zhihu--render-mermaid-blocks
       (zhihu--zhihuify-html
        (zhihu--org-normalize-structure
         (zhihu--org-rewrite-footnotes
          (org-export-as
           'html nil nil t '(:section-numbers nil :with-toc nil)))))))))


;; Source conversion entry points

(defun zhihu--typst-compile-html (file)
  "把 Typst FILE 编译成保留 `<head>' 的完整 HTML。
通过标准输入编译一个只包含 show rule 与 FILE include 的 wrapper，在生成 HTML
前移除以 heading 为 target 的 `outline'。其它 outline 保持原样。"
  (let* ((file (expand-file-name file))
         (root (zhihu--typst-root file))
         (relative
          (replace-regexp-in-string
           "\\\\" "/"
           (file-relative-name file (file-name-as-directory root))
           t t)))
    (when (or (file-name-absolute-p relative)
              (string-prefix-p "../" relative))
      (error "zhihu: Typst 源稿不在项目 root 内：%s" file))
    (zhihu--shell-convert
     "typst"
     (list "compile" "--features=html"
           "--root" root "-f" "html" "-" "-")
     (concat
      "#show outline: it => {\n"
      "  let target = repr(it.target)\n"
      "  if target == \"heading\" or "
      "target.starts-with(\"heading.where(\") "
      "{ none } else { it }\n"
      "}\n"
      "#include "
      (json-serialize (concat "/" relative))
      "\n"))))

(defun zhihu--html-document-title (html)
  "从完整 HTML 的 `<title>' 返回纯文本标题；缺失或为空时返回 nil。"
  (when-let* ((dom (zhihu--parse-html html))
              (node (car (dom-by-tag dom 'title)))
              (title (string-trim (dom-inner-text node))))
    (unless (string-empty-p title) title)))

(defun zhihu--source-to-html (file)
  "把 Typst 或 Org FILE 转为知乎 HTML。"
  (pcase (zhihu--file-format file)
    ('typst
     (let ((full (zhihu--typst-compile-html file)))
       (zhihu--normalize-html full)))
    ('org
     (zhihu--org->html
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string))))
    (_ (error "zhihu: 不支持的文件类型 %s" file))))

(defun zhihu--compile-source-document
    (file metadata-title &optional title-function)
  "把 FILE 编译成统一的知乎 HTML 文档表示。
返回 `(:format FORMAT :title TITLE :html HTML)'。Org 标题来自已经解析的
METADATA-TITLE；Typst 的标题和正文来自同一次 HTML 编译。
非 nil 的 TITLE-FUNCTION 会在正文转换前接收标题，其返回值成为 TITLE。"
  (let ((format
         (or (zhihu--file-format file)
             (user-error "zhihu: 不支持的文件类型 %s" file))))
    (if (eq format 'typst)
        (let* ((full-html
                (progn
                  (message "zhihu: 编译 %s..." format)
                  (zhihu--typst-compile-html file)))
               (resolved-title
                (zhihu--html-document-title full-html))
               (title
                (if title-function
                    (funcall title-function resolved-title)
                  resolved-title)))
          (list :format format
                :title title
                :html (zhihu--normalize-html full-html)))
      (let ((title
             (if title-function
                 (funcall title-function metadata-title)
               metadata-title)))
        (message "zhihu: 编译 %s..." format)
        (list :format format
              :title title
              :html (zhihu--source-to-html file))))))

;;;; User mentions

(defconst zhihu--user-search-endpoint
  "https://www.zhihu.com/people/autocomplete"
  "知乎网页编辑器使用的公开用户补全端点。")

(defun zhihu--user-record (entry)
  "把用户补全响应中的 ENTRY 归一化为可写入 mention 的用户 plist。"
  (when (and (vectorp entry)
             (>= (length entry) 6)
             (equal (aref entry 0) "people"))
    (let ((name (aref entry 1))
          (id (aref entry 2))
          (hash (aref entry 4))
          (description (aref entry 5)))
      (when (and (stringp name)
                 (not (string-empty-p (string-trim name)))
                 (stringp id)
                 (string-match-p "\\`[^/?#[:space:]]+\\'" id)
                 (stringp hash)
                 (string-match-p
                  "\\`[[:xdigit:]]\\{32\\}\\'" hash)
                 (stringp description))
        (list
         :name (string-trim name)
         :id id
         :hash (downcase hash)
         :description
         (replace-regexp-in-string
          "[[:space:]]+" " " (string-trim description)))))))

(defun zhihu--search-users (query)
  "搜索 QUERY 对应的知乎用户，返回规范用户 plist 列表。"
  (setq query (string-trim (or query "")))
  (when (string-empty-p query)
    (user-error "zhihu: 用户搜索词不能为空"))
  (let* ((url
          (concat
           zhihu--user-search-endpoint
           "?token=" (url-hexify-string query)
           "&max_matches=10&use_similar=0"))
         (resp (zhihu--http "GET" url))
         (status (plist-get resp :status))
         (body (plist-get resp :body)))
    (unless (zhihu--successful-status-p status)
      (error "zhihu: 搜索知乎用户失败 (HTTP %s)：%s"
             status (zhihu--response-error-message nil body)))
    (let ((json
           (condition-case err
               (json-parse-string
                body
                :array-type 'array
                :null-object :json-null
                :false-object :json-false)
             (error
              (error "zhihu: 用户补全响应不是合法 JSON：%s"
                     (error-message-string err))))))
      (unless (and (vectorp json)
                   (> (length json) 0)
                   (vectorp (aref json 0)))
        (error "zhihu: 用户补全响应结构无效"))
      (let ((seen (make-hash-table :test 'equal))
            users)
        (dolist (entry (cdr (append (aref json 0) nil)))
          (when-let* ((user (zhihu--user-record entry)))
            (unless (gethash (plist-get user :id) seen)
              (puthash (plist-get user :id) t seen)
              (push user users))))
        (nreverse users)))))

(defun zhihu--user-completion-candidates (users)
  "把 USERS 转为补全使用的唯一 `(LABEL . USER)' 列表。"
  (let ((used (make-hash-table :test 'equal)))
    (mapcar
     (lambda (user)
       (let* ((name (plist-get user :name))
              (id (plist-get user :id))
              (description (plist-get user :description))
              (label
               (if (string-empty-p description)
                   (format "%s (%s)" name id)
                 (format "%s — %s (%s)"
                         name description id))))
         (while (gethash label used)
           (setq label
                 (format "%s [%s]"
                         label
                         (substring (plist-get user :hash) 0 8))))
         (puthash label t used)
         (cons label user)))
     users)))

(defun zhihu--user-mention-source (format user)
  "把 USER 格式化为 FORMAT 对应的持久化 mention 源标记。"
  (let* ((name (plist-get user :name))
         (id (plist-get user :id))
         (hash (plist-get user :hash))
         (profile-url
          (concat "https://www.zhihu.com/people/" id))
         (marker (concat "member_mention_" hash))
         (text (concat "@" name)))
    (cl-ecase format
      (org
       (format "@@html:<a href=\"%s\" title=\"%s\">%s</a>@@"
               profile-url marker
               (string-replace
                "@" "&#64;" (xml-escape-string text))))
      (typst
       (format
        "#html.elem(\"a\", attrs: (href: %S, title: %S), text(%S))"
        profile-url marker text)))))

;;;###autoload
(defun zhihu-insert-user-mention (query)
  "搜索 QUERY 对应的知乎用户，并在光标处插入原生 mention。"
  (interactive (list (read-string "搜索知乎用户: ")))
  (let ((source-format
         (and buffer-file-name
              (zhihu--file-format buffer-file-name))))
    (unless source-format
      (user-error "zhihu: 当前 buffer 不是受支持的源稿文件"))
    (let* ((users (zhihu--search-users query))
           (candidates
            (zhihu--user-completion-candidates users)))
      (unless candidates
        (user-error "zhihu: 没有找到知乎用户：%s" query))
      (let* ((selected
              (completing-read
               "知乎用户: " candidates nil t))
             (user (cdr (assoc selected candidates))))
        (insert
         (zhihu--user-mention-source
          source-format user))))))

;;;; Editing support

(defconst zhihu--column-completion-cache-unloaded
  (make-symbol "zhihu-column-completion-cache-unloaded")
  "表示当前 buffer 尚未读取可投稿专栏候选的哨兵。")

(defvar-local zhihu--column-completion-cache
  zhihu--column-completion-cache-unloaded
  "当前 buffer 缓存的可投稿专栏列表。")

(defvar-local zhihu--topic-completion-cache nil
  "当前 buffer 中按查询词缓存的知乎话题补全结果。")

(defun zhihu--reset-completion-caches ()
  "清空当前 buffer 的全部知乎编辑补全缓存。"
  (setq
   zhihu--column-completion-cache
   zhihu--column-completion-cache-unloaded
   zhihu--topic-completion-cache nil))

(defun zhihu--invalidate-column-completion-caches ()
  "让所有现存源稿 buffer 下次补全时重新读取可投稿专栏。"
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (local-variable-p
             'zhihu--column-completion-cache buffer)
        (setq
         zhihu--column-completion-cache
         zhihu--column-completion-cache-unloaded)))))

(defun zhihu--cached-writable-columns ()
  "返回可投稿专栏，并在当前源稿 buffer 中缓存结果。"
  (if (eq zhihu--column-completion-cache
          zhihu--column-completion-cache-unloaded)
      (setq zhihu--column-completion-cache
            (zhihu--writable-columns))
    zhihu--column-completion-cache))

(defun zhihu--column-completion-candidates (columns)
  "把 COLUMNS 转为名称优先且全局唯一的补全条目。"
  (let ((title-counts (make-hash-table :test 'equal))
        (raw-titles (make-hash-table :test 'equal))
        (used-labels (make-hash-table :test 'equal)))
    (dolist (column columns)
      (let ((title (plist-get column :title)))
        (puthash title (1+ (gethash title title-counts 0))
                 title-counts)
        (puthash title t raw-titles)))
    (mapcar
     (lambda (column)
       (let* ((title (plist-get column :title))
              (id (plist-get column :id))
              (label
               (if (> (gethash title title-counts 0) 1)
                   (format "%s [%s]" title id)
                 title)))
         (when (> (gethash title title-counts 0) 1)
           (while (or (gethash label raw-titles)
                      (gethash label used-labels))
             (setq label (format "%s [%s]" label id))))
         (puthash label t used-labels)
         (cons label column)))
     columns)))









(defun zhihu--scalar-completion-context
    (replace-start value-limit origin &optional comment-p)
  "返回简单标量值的补全边界。
REPLACE-START 至 VALUE-LIMIT 是最终整体改写范围，ORIGIN 是原光标。
COMMENT-P 表示 VALUE-LIMIT 后紧跟注释。"
  (save-excursion
    (let ((value-start
           (progn
             (goto-char replace-start)
             (skip-chars-forward " \t" value-limit)
             (point)))
          (value-end
           (progn
             (goto-char value-limit)
             (skip-chars-backward " \t" replace-start)
             (point))))
      (if (>= value-start value-end)
          (when (and (<= replace-start origin)
                     (<= origin value-limit))
            (list
             :completion-start origin
             :completion-end origin
             :replace-start replace-start
             :replace-end value-limit
             :suffix
             (if comment-p " " "")))
        (let* ((delimiter (char-after value-start))
               (quoted-p (memq delimiter '(?\" ?\')))
               (closed-p
                (and quoted-p
                     (> value-end (1+ value-start))
                     (eq (char-before value-end) delimiter)))
               (completion-start
                (if quoted-p (1+ value-start) value-start))
               (completion-end
                (if closed-p (1- value-end) value-end)))
          (when (and (<= completion-start origin)
                     (<= origin completion-end))
            (list
             :completion-start completion-start
             :completion-end completion-end
             :replace-start replace-start
             :replace-end value-limit
             :suffix
             (let ((spacing
                    (buffer-substring-no-properties
                     value-end value-limit)))
               (if (and comment-p (string-empty-p spacing))
                   " "
                 spacing)))))))))






(defconst zhihu--org-editing-block-types
  '(center-block comment-block drawer dynamic-block example-block
                 export-block property-drawer quote-block special-block
                 src-block verse-block)
  "不能包含文档级知乎编辑 metadata 的 Org 元素类型。")

(defun zhihu--org-editing-blocked-node-p (node)
  "NODE 位于非文档级 Org 容器中时返回非 nil。"
  (let (blocked)
    (while node
      (when (memq (org-element-type node)
                  zhihu--org-editing-block-types)
        (setq blocked t))
      (setq node (org-element-property :parent node)))
    blocked))

(defun zhihu--org-column-id-context ()
  "返回光标所在 Org `ZHIHU_COLUMN_ID' 值槽的补全上下文。"
  (when (and buffer-file-name
             (eq (zhihu--file-format buffer-file-name) 'org)
             (derived-mode-p 'org-mode))
    (require 'org-element)
    (let* ((origin (point))
           (line-start (line-beginning-position))
           (line-end (line-end-position))
           (node (org-element-context)))
      (when (and (eq (org-element-type node) 'keyword)
                 (equal (org-element-property :key node)
                        "ZHIHU_COLUMN_ID")
                 (not (zhihu--org-editing-blocked-node-p node)))
        (save-excursion
          (goto-char line-start)
          (let ((case-fold-search t))
            (when (looking-at
                   "^#\\+ZHIHU_COLUMN_ID[ \t]*:")
              (when-let* ((bounds
                          (zhihu--scalar-completion-context
                           (match-end 0) line-end origin)))
                (append (list :format 'org) bounds)))))))))


(defun zhihu--typst-metadata-dictionary-info (region)
  "返回 canonical Typst metadata REGION 的 dictionary 信息。
结果包含 dictionary 内容的 `:start'、`:end' 和直接字段的相对语法
`:depth'。"
  (save-excursion
    (with-syntax-table zhihu--typst-syntax-table
      (let ((case-fold-search nil)
            (parse-sexp-ignore-comments t))
        (goto-char (car region))
        (when (re-search-forward "#metadata\\_>" (cdr region) t)
          (forward-comment (point-max))
          (when (eq (char-after) ?\()
            (forward-char 1)
            (forward-comment (point-max))
            (when (eq (char-after) ?\()
              (let* ((dictionary-open (point))
                     (dictionary-end
                      (condition-case nil
                          (scan-sexps dictionary-open 1)
                        (scan-error nil))))
                (when (and dictionary-end
                           (<= dictionary-end
                               (cdr region)))
                  (list
                   :start (1+ dictionary-open)
                   :end (1- dictionary-end)
                   :depth
                   (1+
                    (car
                     (syntax-ppss
                      dictionary-open)))))))))))))

(defun zhihu--typst-column-id-context ()
  "返回光标所在 canonical Typst `column-id' 值槽的上下文。
只接受 `<zhihu>' dictionary 的直接字段，并拒绝字符串、注释或嵌套值中的
伪字段。值槽本身按 writer 生成的单行标量处理。"
  (when (and buffer-file-name
             (eq (zhihu--file-format buffer-file-name) 'typst))
    (let ((case-fold-search nil)
          (origin (point))
          (line-start (line-beginning-position))
          (line-end (line-end-position))
          (region
           (condition-case nil
               (zhihu--typst-native-metadata-region)
             (error nil))))
      (when-let* ((dictionary
                  (and
                   region
                   (zhihu--typst-metadata-dictionary-info
                    region))))
        (save-excursion
          (goto-char line-start)
          (when (re-search-forward
                 "\\_<column-id\\_>[ \t]*:" line-end t)
            (let* ((replace-start (match-end 0))
                   (key-start (match-beginning 0))
                   (state
                    (with-syntax-table
                        zhihu--typst-syntax-table
                      (syntax-ppss key-start))))
              (when (and
                     (<= (plist-get dictionary :start)
                         key-start)
                     (< key-start
                        (plist-get dictionary :end))
                     (= (car state)
                        (plist-get dictionary :depth))
                     (not (nth 3 state))
                     (not (nth 4 state)))
                (let (comma)
                  (save-excursion
                    (goto-char replace-start)
                    (while
                        (and
                         (not comma)
                         (re-search-forward "," line-end t))
                      (let ((comma-state
                             (with-syntax-table
                                 zhihu--typst-syntax-table
                               (syntax-ppss
                                (match-beginning 0)))))
                        (when
                            (and
                             (= (car comma-state)
                                (plist-get
                                 dictionary :depth))
                             (not (nth 3 comma-state))
                             (not (nth 4 comma-state)))
                          (setq comma
                                (match-beginning 0))))))
                  (when-let*
                      ((bounds
                        (zhihu--scalar-completion-context
                         replace-start
                         (if comma
                             comma
                           line-end)
                         origin)))
                    (append
                     (list :format 'typst)
                     bounds)))))))))))

(defun zhihu--column-id-context ()
  "返回当前源稿光标所在 `column-id' 值槽的上下文。"
  (pcase (and buffer-file-name
              (zhihu--file-format buffer-file-name))
    ('org (zhihu--org-column-id-context))
    ('typst (zhihu--typst-column-id-context))))

(defun zhihu--topic-kind-and-limit (identities)
  "按 IDENTITIES 中唯一的内容 ID 字段返回类型和话题上限。"
  (when (= (length identities) 1)
    (pcase (car identities)
      (:article-id (cons 'article zhihu--article-topic-limit)))))

(defun zhihu--double-quoted-string-info (start limit)
  "解析 START 处、不超过 LIMIT 的双引号字符串。
返回内容边界、紧随右引号的位置和解码后的字符串。"
  (when (and (< start limit) (eq (char-after start) ?\"))
    (let ((position (1+ start))
          closing)
      (while (and (< position limit) (not closing))
        (pcase (char-after position)
          (?\\
           (setq position (+ position 2)))
          (?\"
           (setq closing position))
          ((or ?\n ?\r)
           (setq position limit))
          (_ (cl-incf position))))
      (when (and closing (< closing limit))
        (let* ((literal
                (buffer-substring-no-properties start (1+ closing)))
               (value
                (condition-case nil
                    (json-parse-string literal)
                  (error nil))))
          (when (stringp value)
            (list :completion-start (1+ start)
                  :completion-end closing
                  :after (1+ closing)
                  :name value)))))))

(defun zhihu--quoted-sequence-items
    (start end open close &optional trailing-comma-p)
  "解析 START 至 END 中由 OPEN/CLOSE 包围的字符串 sequence。
只接受逗号分隔的双引号字符串。TRAILING-COMMA-P 非 nil
时允许尾逗号。"
  (save-excursion
    (goto-char start)
    (skip-chars-forward " \t\r\n" end)
    (when (eq (char-after) open)
      (forward-char 1)
      (let (items just-saw-comma valid done)
        (setq valid t)
        (while (and valid (not done))
          (skip-chars-forward " \t\r\n" end)
          (cond
           ((>= (point) end)
            (setq valid nil))
           ((eq (char-after) close)
            (if (and just-saw-comma items (not trailing-comma-p))
                (setq valid nil)
              (forward-char 1)
              (skip-chars-forward " \t\r\n" end)
              (setq valid (= (point) end)
                    done t)))
           ((eq (char-after) ?\")
            (let ((info
                   (zhihu--double-quoted-string-info (point) end)))
              (if (not info)
                  (setq valid nil)
                (push info items)
                (setq just-saw-comma nil)
                (goto-char (plist-get info :after))
                (skip-chars-forward " \t\r\n" end)
                (cond
                 ((eq (char-after) ?,)
                  (forward-char 1)
                  (setq just-saw-comma t))
                 ((eq (char-after) close))
                 (t (setq valid nil))))))
           (t (setq valid nil))))
        (and valid (nreverse items))))))

(defun zhihu--topic-context-from-items (format items origin kind-limit)
  "从 FORMAT/ITEMS/ORIGIN/KIND-LIMIT 生成统一话题补全上下文。"
  (when kind-limit
    (let ((index 0)
          current)
      (dolist (item items)
        (when (and (null current)
                   (<= (plist-get item :completion-start) origin)
                   (<= origin (plist-get item :completion-end)))
          (setq current (cons index item)))
        (cl-incf index))
      (when (and current
                 (< (car current) (cdr kind-limit)))
        (let ((current-index (car current))
              (item (cdr current))
              (other-index 0)
              others)
          (dolist (candidate items)
            (unless (= other-index current-index)
              (push (plist-get candidate :name) others))
            (cl-incf other-index))
          (list
           :format format
           :kind (car kind-limit)
           :completion-start (plist-get item :completion-start)
           :completion-end (plist-get item :completion-end)
           :replace-start (plist-get item :completion-start)
           :replace-end (plist-get item :completion-end)
           :other-topics (nreverse others)))))))









(defun zhihu--org-topic-kind-and-limit ()
  "按当前 Org buffer 的文档级 identity 返回话题类型和上限。"
  (require 'org-element)
  (let (identities)
    (org-element-map
     (org-element-parse-buffer) 'keyword
     (lambda (node)
       (unless (zhihu--org-editing-blocked-node-p node)
         (pcase (org-element-property :key node)
           ("ZHIHU_QUESTION_ID" (push :question-id identities))
           ("ZHIHU_ARTICLE_ID" (push :article-id identities))))))
    (zhihu--topic-kind-and-limit (nreverse identities))))

(defun zhihu--org-topic-context ()
  "返回当前 Org `ZHIHU_TOPICS' JSON item 的补全上下文。"
  (when (and buffer-file-name
             (eq (zhihu--file-format buffer-file-name) 'org)
             (derived-mode-p 'org-mode))
    (require 'org-element)
    (let* ((origin (point))
           (node (org-element-context)))
      (when (and (eq (org-element-type node) 'keyword)
                 (equal (org-element-property :key node) "ZHIHU_TOPICS")
                 (not (zhihu--org-editing-blocked-node-p node)))
        (save-excursion
          (goto-char (line-beginning-position))
          (let ((case-fold-search t))
            (when (looking-at "^#\\+ZHIHU_TOPICS[ \t]*:")
              (let ((items
                     (zhihu--quoted-sequence-items
                      (match-end 0) (line-end-position) ?\[ ?\])))
                (zhihu--topic-context-from-items
                 'org items origin
                 (zhihu--org-topic-kind-and-limit))))))))))


(defun zhihu--typst-direct-metadata-fields (dictionary)
  "返回 Typst DICTIONARY 中直接出现的已知 metadata 字段。
每项为 `(FIELD KEY-START VALUE-START)'。"
  (let ((case-fold-search nil)
        (regexp
         (concat
          "\\_<\\("
          (mapconcat
           (lambda (field)
             (regexp-quote (zhihu--metadata-field-name field)))
           zhihu--metadata-fields
           "\\|")
          "\\)"
          "\\_>[ \t]*:"))
        found)
    (save-excursion
      (with-syntax-table zhihu--typst-syntax-table
        (goto-char (plist-get dictionary :start))
        (while (re-search-forward regexp (plist-get dictionary :end) t)
          (let* ((key-start (match-beginning 0))
                 (key-end (match-end 0))
                 (field-name (match-string-no-properties 1))
                 (state (syntax-ppss key-start)))
            (when (and (= (car state) (plist-get dictionary :depth))
                       (not (nth 3 state))
                       (not (nth 4 state)))
              (push
               (list
                (intern (concat ":" field-name))
                key-start key-end)
               found))
            (goto-char key-end)))))
    (nreverse found)))

(defun zhihu--metadata-fields-for-kind (kind)
  "返回 KIND 允许出现的有序知乎渠道 metadata 字段。"
  (pcase kind
    ('answer
     (append
      (cdr (assq 'answer zhihu--metadata-kind-scalar-fields))
      zhihu--metadata-common-scalar-fields))
    ('article
     (append
      (cdr (assq 'article zhihu--metadata-kind-scalar-fields))
      zhihu--metadata-common-scalar-fields
      '(:topics)))))

(defun zhihu--metadata-kind-for-identity (field)
  "返回 identity metadata FIELD 对应的稿件类型。"
  (pcase field
    (:question-id 'answer)
    (:article-id 'article)))

(defun zhihu--metadata-key-candidate-fields (present-fields)
  "按 PRESENT-FIELDS 返回当前可新增的 metadata 字段。
PRESENT-FIELDS 必须是 dictionary 中已识别出的直接字段。若字段重复、包含
多个 identity，或已出现字段与 identity 冲突，则返回 nil。identity 尚未
确定时只返回与已有辅助字段兼容的 identity 字段。"
  (when (= (length present-fields)
           (length (delete-dups (copy-sequence present-fields))))
    (let ((identities
           (cl-remove-if-not
            (lambda (field)
              (memq field '(:question-id :article-id)))
            present-fields)))
      (pcase (length identities)
        (0
         (cl-loop
          for identity in '(:question-id :article-id)
          for kind = (zhihu--metadata-kind-for-identity identity)
          for allowed = (zhihu--metadata-fields-for-kind kind)
          when (cl-every
                (lambda (field) (memq field allowed))
                present-fields)
          collect identity))
        (1
         (let* ((kind
                 (zhihu--metadata-kind-for-identity (car identities)))
                (allowed (zhihu--metadata-fields-for-kind kind)))
           (when (cl-every
                  (lambda (field) (memq field allowed))
                  present-fields)
             (cl-remove-if
              (lambda (field) (memq field present-fields))
              allowed))))))))



(defun zhihu--metadata-key-capf (info)
  "根据字段名上下文 INFO 返回知乎 metadata key 的 CAPF。"
  (when info
    (list
     (plist-get info :completion-start)
     (plist-get info :completion-end)
     (mapcar
      (lambda (field)
        (concat (zhihu--metadata-field-name field) ":"))
      (plist-get info :candidates))
     :exclusive t)))



(defun zhihu--typst-trivia-only-p (start end)
  "START 到 END 仅包含 Typst 空白或注释时返回非 nil。"
  (save-restriction
    (narrow-to-region start end)
    (save-excursion
      (with-syntax-table zhihu--typst-syntax-table
        (let ((parse-sexp-ignore-comments t))
          (goto-char (point-min))
          (forward-comment (point-max))
          (= (point) (point-max)))))))

(defun zhihu--typst-direct-entry-end (start dictionary)
  "返回 START 之后 Typst DICTIONARY 当前直接 entry 的结束位置。
结果是下一个直接逗号的位置；没有直接逗号时为 dictionary 内容末尾。"
  (save-excursion
    (with-syntax-table zhihu--typst-syntax-table
      (let ((parse-sexp-ignore-comments t)
            (limit (plist-get dictionary :end)))
        (goto-char start)
        (catch 'entry-end
          (while (re-search-forward "," limit t)
            (let* ((comma (match-beginning 0))
                   (resume (match-end 0))
                   (state
                    (save-excursion
                      (syntax-ppss comma))))
              ;; `syntax-ppss' may move point while consulting its cache.
              (goto-char resume)
              (when (and (= (car state)
                            (plist-get dictionary :depth))
                         (not (nth 3 state))
                         (not (nth 4 state)))
                (throw 'entry-end comma))))
          limit)))))

(defun zhihu--typst-metadata-key-context ()
  "返回光标所在 canonical Typst metadata 直接字段名槽的上下文。
字符串、注释、值槽和嵌套 dictionary 中的文本均不算字段名槽。"
  (when (and buffer-file-name
             (eq (zhihu--file-format buffer-file-name) 'typst))
    (let* ((origin (point))
           (region
            (condition-case nil
                (zhihu--typst-native-metadata-region)
              (error nil)))
           (dictionary
            (and region
                 (zhihu--typst-metadata-dictionary-info region))))
      (when (and dictionary
                 (<= (plist-get dictionary :start) origin)
                 (<= origin (plist-get dictionary :end)))
        (save-excursion
          (with-syntax-table zhihu--typst-syntax-table
            (let* ((parse-sexp-ignore-comments t)
                   (state (syntax-ppss origin))
                   (entry-start (plist-get dictionary :start))
                   colon-seen)
              (when (and (= (car state) (plist-get dictionary :depth))
                         (not (nth 3 state))
                         (not (nth 4 state)))
                (goto-char entry-start)
                (while (re-search-forward "[,:]" origin t)
                  (let* ((punctuation (match-beginning 0))
                         (resume (match-end 0))
                         (punctuation-state
                          (save-excursion
                            (syntax-ppss punctuation))))
                    (when (and
                           (= (car punctuation-state)
                              (plist-get dictionary :depth))
                           (not (nth 3 punctuation-state))
                           (not (nth 4 punctuation-state)))
                      (if (eq (char-after punctuation) ?,)
                          (setq entry-start resume
                                colon-seen nil)
                        (setq colon-seen t)))
                    ;; `syntax-ppss' may move point while consulting its cache.
                    (goto-char resume)))
                (unless colon-seen
                  (let ((completion-start
                         (save-excursion
                           (goto-char origin)
                           (skip-chars-backward
                            "A-Za-z0-9-" entry-start)
                           (point)))
                        (completion-end
                         (save-excursion
                           (goto-char origin)
                           (skip-chars-forward
                            "A-Za-z0-9-"
                            (plist-get dictionary :end))
                           (point))))
                    (when (and
                           (zhihu--typst-trivia-only-p
                            entry-start completion-start)
                           (zhihu--typst-trivia-only-p
                            completion-end
                            (zhihu--typst-direct-entry-end
                             completion-end dictionary)))
                      (let* ((fields
                              (mapcar
                               #'car
                               (zhihu--typst-direct-metadata-fields
                                dictionary)))
                             (candidates
                              (zhihu--metadata-key-candidate-fields fields)))
                        (when candidates
                          (list
                           :completion-start completion-start
                           :completion-end completion-end
                           :candidates candidates))))))))))))))

(defun zhihu--typst-metadata-key-capf ()
  "返回 canonical Typst metadata 直接字段名槽的 CAPF。"
  (zhihu--metadata-key-capf
   (zhihu--typst-metadata-key-context)))

(defun zhihu--typst-topic-context ()
  "返回当前 Typst `<zhihu>' topics tuple item 的补全上下文。"
  (when (and buffer-file-name
             (eq (zhihu--file-format buffer-file-name) 'typst))
    (let ((origin (point))
          (region
           (condition-case nil
               (zhihu--typst-native-metadata-region)
             (error nil))))
      (when-let* ((dictionary
                  (and region
                       (zhihu--typst-metadata-dictionary-info region))))
        (let* ((fields (zhihu--typst-direct-metadata-fields dictionary))
               (identities
                (mapcar
                 #'car
                 (cl-remove-if-not
                  (lambda (entry)
                    (memq (car entry)
                          '(:question-id :article-id)))
                  fields)))
               (kind-limit (zhihu--topic-kind-and-limit identities))
               (topic-fields
                (cl-remove-if-not
                 (lambda (entry) (eq (car entry) :topics)) fields)))
          (when (and kind-limit (= (length topic-fields) 1))
            (save-excursion
              (with-syntax-table zhihu--typst-syntax-table
                (goto-char (nth 2 (car topic-fields)))
                (forward-comment (plist-get dictionary :end))
                (when (eq (char-after) ?\()
                  (let* ((tuple-start (point))
                         (tuple-end
                          (condition-case nil
                              (scan-sexps tuple-start 1)
                            (scan-error nil))))
                    (when (and tuple-end
                               (<= tuple-end
                                   (1+ (plist-get dictionary :end))))
                      (zhihu--topic-context-from-items
                       'typst
                       (zhihu--quoted-sequence-items
                        tuple-start tuple-end ?\( ?\) t)
                       origin kind-limit))))))))))))

(defun zhihu--topic-item-context ()
  "返回当前源稿 `topics' item 的补全上下文。"
  (pcase (and buffer-file-name
              (zhihu--file-format buffer-file-name))
    ('org (zhihu--org-topic-context))
    ('typst (zhihu--typst-topic-context))))

(defun zhihu--completion-session-remember (session entries)
  "把 ENTRIES 累积进 SESSION，并原样返回。"
  (dolist (entry entries)
    (puthash
     (substring-no-properties (car entry))
     (cons (substring-no-properties (car entry))
           (cdr entry))
     (zhihu--completion-session-seen session)))
  entries)

(defun zhihu--completion-session-entry (session candidate)
  "返回 SESSION 中 CANDIDATE 对应的完整候选条目。"
  (gethash
   (substring-no-properties candidate)
   (zhihu--completion-session-seen session)))

(defun zhihu--completion-session-release (session)
  "释放 SESSION 持有的源稿 markers。"
  (dolist
      (marker
       (list
        (zhihu--completion-session-start-marker session)
        (zhihu--completion-session-end-marker session)))
    (set-marker marker nil)))

(defun zhihu--completion-session-finish
    (session candidate status)
  "STATUS 完成时把 SESSION 的显示 CANDIDATE 提交为源稿。"
  (when (memq status '(finished exact))
    (unwind-protect
        (when-let* ((source
                     (zhihu--completion-session-source-buffer
                      session))
                    (live-source
                     (and (buffer-live-p source) source))
                    (start-marker
                     (zhihu--completion-session-start-marker
                      session))
                    (end-marker
                     (zhihu--completion-session-end-marker
                      session))
                    (entry
                     (zhihu--completion-session-entry
                      session candidate))
                    (start (marker-position start-marker))
                    (end (marker-position end-marker)))
          (when (and
                 (eq live-source (marker-buffer start-marker))
                 (eq live-source (marker-buffer end-marker))
                 (<= start end))
            (with-current-buffer live-source
              (delete-region start end)
              (goto-char start)
              (insert
               (funcall
                (zhihu--completion-session-source-function
                 session)
                (zhihu--completion-session-format session)
                (cdr entry)
                (or
                 (zhihu--completion-session-suffix session)
                 ""))))))
      (zhihu--completion-session-release session))))

(defun zhihu--column-id-completion-source
    (format column suffix)
  "把 COLUMN ID 格式化为 FORMAT 源稿，并保留 SUFFIX。"
  (let ((id (plist-get column :id)))
    (concat
     (pcase format
       ('org (concat " " id))
       ('typst (concat " " (format "%S" id)))
       (_ (error "zhihu: 不支持的补全源稿格式 %S"
                 format)))
     suffix)))

(defun zhihu--column-completion-entries (source-buffer)
  "在 SOURCE-BUFFER 中返回专栏名称补全条目。"
  (when (buffer-live-p source-buffer)
    (with-current-buffer source-buffer
      (zhihu--column-completion-candidates
       (zhihu--cached-writable-columns)))))

(defun zhihu--column-id-capf (info)
  "根据值槽 INFO 返回 `column-id' CAPF 数据。"
  (let* ((source-buffer (current-buffer))
         (session
          (zhihu--make-completion-session
           :source-buffer source-buffer
           :start-marker
           (copy-marker (plist-get info :replace-start))
           :end-marker
           (copy-marker (plist-get info :replace-end) t)
           :format (plist-get info :format)
           :suffix (plist-get info :suffix)
           :seen (make-hash-table :test 'equal)
           :source-function
           #'zhihu--column-id-completion-source))
         (table
          (completion-table-dynamic
           (lambda (_prefix)
             (mapcar
              #'car
              (zhihu--completion-session-remember
               session
               (zhihu--column-completion-entries
                source-buffer)))))))
    (list
     (plist-get info :completion-start)
     (plist-get info :completion-end)
     table
     :exclusive t
     :annotation-function
     (lambda (candidate)
       (when-let* ((entry
                   (zhihu--completion-session-entry
                    session candidate)))
         (format "  ID %s"
                 (plist-get (cdr entry) :id))))
     :exit-function
     (lambda (candidate status)
       (zhihu--completion-session-finish
        session candidate status)))))























(defun zhihu--cached-topic-completions (query)
  "返回 QUERY 的话题补全结果，并在当前 buffer 中缓存。"
  (setq query (string-trim (substring-no-properties query)))
  (unless (string-empty-p query)
    (let ((entry (assoc-string query zhihu--topic-completion-cache)))
      (if entry
          (cdr entry)
        (let ((topics (zhihu--search-article-topics query)))
          ;; 空结果也缓存，避免重复请求同一查询。
          (push (cons query topics) zhihu--topic-completion-cache)
          topics)))))

(defun zhihu--remote-completion-table
    (entries-function category)
  "创建按远端相关性返回 ENTRIES-FUNCTION 结果的补全表。"
  (lambda (string predicate action)
    (cond
     ((eq action 'metadata)
      `(metadata
        (category . ,category)
        (display-sort-function . identity)
        (cycle-sort-function . identity)))
     ((and (consp action)
           (eq (car action) 'boundaries))
      '(boundaries 0 . 0))
     (t
      (let ((labels
             (mapcar #'car
                     (funcall entries-function string))))
        (when predicate
          (setq labels
                (cl-remove-if-not
                 (lambda (candidate)
                   (funcall predicate candidate))
                 labels)))
        (cond
         ((eq action t) labels)
         ((eq action 'lambda)
          (and (member
                (substring-no-properties string)
                labels)
               t))
         ((null labels) nil)
         ((member (substring-no-properties string)
                  labels)
          t)
         ((null (cdr labels)) (car labels))
         (t string)))))))

(defun zhihu--topic-completion-source (format record _suffix)
  "把 RECORD 的话题名转为 FORMAT 字符串内容；_SUFFIX 被忽略。"
  (let* ((name (plist-get record :name))
         (quoted
         (pcase format
            ('org (json-encode-string name))
            ('typst (format "%S" name))
            (_ (error "zhihu: 不支持的话题补全源稿格式 %S"
                      format)))))
    (substring quoted 1 -1)))

(defun zhihu--topic-capf (info)
  "根据 topics item INFO 返回知乎话题 CAPF 数据。"
  (let* ((source-buffer (current-buffer))
         (other-topics (plist-get info :other-topics))
         (session
          (zhihu--make-completion-session
           :source-buffer source-buffer
           :start-marker (copy-marker (plist-get info :replace-start))
           :end-marker (copy-marker (plist-get info :replace-end) t)
           :format (plist-get info :format)
           :seen (make-hash-table :test 'equal)
           :source-function #'zhihu--topic-completion-source))
         (table
          (zhihu--remote-completion-table
           (lambda (current)
             (or
              (when-let* ((entry
                          (zhihu--completion-session-entry session current)))
                (list entry))
              (when (buffer-live-p source-buffer)
                (with-current-buffer source-buffer
                  (zhihu--completion-session-remember
                   session
                   (zhihu--article-topic-completion-entries
                    (cl-remove-if
                     (lambda (record)
                       (member (plist-get record :name) other-topics))
                     (zhihu--cached-topic-completions current))))))))
           'zhihu-topic)))
    (list
     (plist-get info :completion-start)
     (plist-get info :completion-end)
     table
     :exclusive t
     :exit-function
     (lambda (candidate status)
       (zhihu--completion-session-finish
        session candidate status)))))

;;;###autoload
(defun zhihu-completion-at-point ()
  "补全知乎源稿的 metadata 字段、`column-id' 和 `topics'。"
  (save-restriction
    (widen)
    (or
     (zhihu--typst-metadata-key-capf)
     (when-let* ((info (zhihu--column-id-context)))
       (zhihu--column-id-capf info))
     (when-let* ((info (zhihu--topic-item-context)))
       (zhihu--topic-capf info)))))

(defun zhihu--source-editing-format ()
  "返回当前 buffer 可由 `zhihu-mode' 编辑的源稿格式。"
  (when buffer-file-name
    (pcase (zhihu--file-format buffer-file-name)
      ('org
       (and (derived-mode-p 'org-mode) 'org))
      ('typst
       (and (derived-mode-p 'typst-ts-mode 'typst-mode)
            'typst)))))

(defvar zhihu-mode-map (make-sparse-keymap)
  "`zhihu-mode' 的空按键映射。")

(defun zhihu--disable-editing-support ()
  "从当前 buffer 移除知乎编辑辅助并清空缓存。"
  (remove-hook
   'completion-at-point-functions
   #'zhihu-completion-at-point t)
  (remove-hook
   'after-revert-hook
   #'zhihu--reset-completion-caches t)
  (zhihu--reset-completion-caches)
  (kill-local-variable
   'zhihu--column-completion-cache)
  (kill-local-variable
   'zhihu--topic-completion-cache))

;;;###autoload
(define-minor-mode zhihu-mode
  "在当前 Org 或 Typst 知乎源稿中启用编辑辅助。
本 mode 提供 metadata 字段名、`column-id' 和 `topics' 补全。发布仍由
`zhihu-publish' 显式执行。"
  :init-value nil
  :lighter " 知"
  :keymap zhihu-mode-map
  :group 'zhihu
  (if zhihu-mode
      (unless (zhihu--source-editing-format)
        (setq zhihu-mode nil)
        (zhihu--disable-editing-support)
        (user-error
         "zhihu: 当前 buffer 不是对应主模式的 Org 或 Typst 源稿"))
    (zhihu--disable-editing-support))
  (when zhihu-mode
    (zhihu--reset-completion-caches)
    (add-hook
     'completion-at-point-functions
     #'zhihu-completion-at-point nil t)
    (add-hook
     'after-revert-hook
     #'zhihu--reset-completion-caches nil t)))

;;;; Images

;; Zhihu and OSS upload
;;
;; 1. POST https://api.zhihu.com/images           {image_hash, source}
;;    → upload_token + upload_file{object_key, state}
;;    state: 1 = 服务端已存在该 hash，不需要 PUT；2 = 待上传
;; 2. PUT  https://zhihu-pics-upload.zhimg.com/<object_key>
;;    Authorization: OSS access_id:HMAC-SHA1(string-to-sign)
;;    把图片二进制原样 PUT 上去（阿里云 OSS bucket）
;; 3. GET  https://api.zhihu.com/images/<image_id>
;;    poll 到 status=success 拿最终 picx URL
;;
;; 图片协议参考 zhihu.nvim lua/zhihu/api/{post/image,put,image}.lua。
;; OSS string-to-sign 格式见 https://help.aliyun.com/zh/oss/developer-reference/include-signatures-in-the-authorization-header
;; 这里走的是"用临时 STS token"的变体（多一行 x-oss-security-token）。

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

(defun zhihu--render-svg-png (svg-bytes)
  "用 Typst 把 SVG-BYTES 栅格化为透明背景的 PNG 字节。"
  (unless (and (stringp svg-bytes) (> (length svg-bytes) 0))
    (error "zhihu: SVG 图片不能为空"))
  (unless (executable-find "typst")
    (user-error
     "zhihu: SVG 转 PNG 需要 PATH 中存在 typst"))
  (let* ((directory (make-temp-file "zhihu-svg-" t))
         (svg (expand-file-name "input.svg" directory))
         (source (expand-file-name "render.typ" directory))
         (output (expand-file-name "output.png" directory)))
    (unwind-protect
        (progn
          (zhihu--write-file-bytes svg svg-bytes)
          (with-temp-file source
            (insert
             "#set page(width: auto, height: auto, margin: 0pt, fill: none)\n"
             "#image(\"input.svg\")\n"))
          (zhihu--shell-convert
           "typst"
           (list
            "compile"
            "--root" directory
            "--format" "png"
            "--ppi" "144"
            source
            output)
           "")
          (unless (and (file-regular-p output)
                       (> (file-attribute-size
                           (file-attributes output))
                          0))
            (error "zhihu: typst 没有生成 PNG"))
          (let ((bytes (zhihu--read-file-bytes output)))
            (unless (zhihu--png-bytes-p bytes)
              (error "zhihu: typst 的 SVG 转换输出不是有效的 PNG"))
            bytes))
      (ignore-errors (delete-directory directory t)))))

(defun zhihu--normalize-image-data (image)
  "归一化 (MIME . BYTES) IMAGE；SVG 会转换为 PNG。"
  (let* ((mime (zhihu--require-image-mime (car image)))
         (base-mime
          (downcase
           (string-trim
            (car (split-string mime ";" t))))))
    (if (string-equal base-mime "image/svg+xml")
        (cons "image/png" (zhihu--render-svg-png (cdr image)))
      image)))

(defun zhihu--hmac-sha1-base64 (key data)
  "HMAC-SHA1(KEY, DATA)，返回 base64 字符串（无换行）。
KEY/DATA 都是 string；内部按 unibyte 处理。"
  (base64-encode-string
   (zhihu--hmac-sha1-bytes
    (encode-coding-string key 'utf-8)
    (encode-coding-string data 'utf-8))
   t))

(defun zhihu--image-prefetch (md5-hex source)
  "POST /images，告诉服务端要传一张 hash 是 MD5-HEX 的图。
SOURCE 是 \"answer\" 或 \"article\"。返回 parsed JSON plist：
  :upload_file (:image_id :object_key :state)  state=1 已存在 / state=2 待传
  :upload_token (:access_id :access_key :access_token :access_timestamp)"
  (let ((resp
         (zhihu--http-json
          "POST" "https://api.zhihu.com/images"
          :body `(:image_hash ,md5-hex :source ,source))))
    (unless (eq (plist-get resp :status) 200)
      (error "zhihu: image prefetch 失败 (%s) %s"
             (plist-get resp :status) (plist-get resp :body)))
    (or (plist-get resp :json)
        (error "zhihu: image prefetch 响应不是 JSON: %s"
               (plist-get resp :body)))))

(defun zhihu--oss-string-to-sign
    (content-type date access-token object-key oss-user-agent)
  "拼 OSS PUT 签名前的 string-to-sign。换行用 \\n。
OSS-USER-AGENT 必须与实际发送的 x-oss-user-agent 请求头一致。"
  (concat "PUT" "\n"
          ""    "\n"        ; Content-MD5（空）
          content-type "\n"
          date "\n"
          "x-oss-date:" date "\n"
          "x-oss-security-token:" access-token "\n"
          "x-oss-user-agent:" oss-user-agent "\n"
          "/zhihu-pics/" object-key))

(defun zhihu--image-oss-put (object-key bytes mime token)
  "把二进制 PUT 到 OSS。TOKEN 是 prefetch 返回的 :upload_token plist。"
  (let* ((oss-user-agent
          "aliyun-sdk-js/6.8.0 Firefox 137.0 on OS X 10.15")
         (date (let ((system-time-locale "C"))
                 (format-time-string "%a, %d %b %Y %H:%M:%S GMT" nil t)))
         (access-id (plist-get token :access_id))
         (access-key (plist-get token :access_key))
         (access-token (plist-get token :access_token))
         (string-to-sign
          (zhihu--oss-string-to-sign
           mime date access-token object-key oss-user-agent))
         (signature (zhihu--hmac-sha1-base64 access-key string-to-sign))
         (url
          (concat "https://zhihu-pics-upload.zhimg.com/" object-key))
         (extra `(("x-oss-user-agent" . ,oss-user-agent)
                  ("x-oss-date" . ,date)
                  ("x-oss-security-token" . ,access-token)
                  ("Authorization" . ,(format "OSS %s:%s" access-id signature))))
         (resp
         (zhihu--http
           "PUT" url
           :body bytes
           :content-type mime
           :extra-headers extra
           :raw-body t)))
    (unless (memq (plist-get resp :status) '(200 203))
      (error "zhihu: OSS PUT %s 失败 (%s) %s"
             url (plist-get resp :status) (plist-get resp :body)))
    resp))

(defun zhihu--image-poll (image-id)
  "轮询 /images/<id>，返回最终 src URL。"
  (let ((url (format "https://api.zhihu.com/images/%s" image-id)))
    (cl-loop for attempt from 1 to 10 do
             (let* ((resp
                     (zhihu--http-json
                      "GET" url :sign-json nil))
                    (status-code (plist-get resp :status))
                    (body (plist-get resp :body))
                    (json (plist-get resp :json)))
               (unless (zhihu--successful-status-p status-code)
                 (error "zhihu: 图片 %s poll 失败 (HTTP %s)：%s"
                        image-id status-code body))
               (unless json
                 (error "zhihu: 图片 %s poll 响应不是 JSON：%s"
                        image-id body))
               (let ((status (plist-get json :status)))
                 (cond
                  ((equal status "success")
                   (let ((src (plist-get json :src)))
                     (unless (and (stringp src) (not (string-empty-p src)))
                       (error "zhihu: 图片 %s poll 成功响应缺少 src" image-id))
                     (cl-return src)))
                  ((member status '("init" "pending"))
                   (if (= attempt 10)
                       (error "zhihu: 图片 %s poll 超时" image-id)
                     (sleep-for 0.5)))
                  ((member status '("failed" "error"))
                   (error "zhihu: 图片 %s 处理失败（status=%s）"
                          image-id status))
                  ((and (stringp status) (not (string-empty-p status)))
                   (error "zhihu: 图片 %s 返回未知状态：%s"
                          image-id status))
                  (t
                   (error "zhihu: 图片 %s poll 响应缺少 status"
                          image-id))))))))

(defun zhihu--upload-bytes (bytes mime source)
  "上传二进制图片，返回最终 picx URL。
服务端按图片 MD5 去重；已有图片会走 state=1，不再 PUT 原始字节。"
  (unless (member source '("answer" "article"))
    (error "zhihu: 图片 source 必须是 answer 或 article"))
  (let* ((md5-hex (secure-hash 'md5 bytes))
         (pf (zhihu--image-prefetch md5-hex source))
         (file (plist-get pf :upload_file))
         (upload-state (plist-get file :state))
         (image-id (plist-get file :image_id)))
    (pcase upload-state
      (1)
      (2
       (zhihu--image-oss-put
        (plist-get file :object_key)
        bytes mime
        (plist-get pf :upload_token)))
      (_
       (error "zhihu: prefetch 返回未知 state=%S" upload-state)))
    (zhihu--image-poll image-id)))

;;;###autoload
(defun zhihu-upload-image-file (path)
  "按回答图片上传 PATH，返回 picx URL（也存入 kill-ring）。"
  (interactive "f图片路径: ")
  (let ((mime (mailcap-file-name-to-mime-type path)))
    (unless (and mime (string-prefix-p "image/" (downcase mime)))
      (user-error "zhihu: 无法识别为图片文件：%s" path))
    (let* ((image
            (zhihu--normalize-image-data
             (cons mime (zhihu--read-file-bytes path))))
           (url
            (zhihu--upload-bytes
             (cdr image) (car image) "answer")))
      (kill-new url)
      (message "zhihu: 已上传 → %s（已 kill-ring）" url)
      url)))

;; HTML image rewriting
;;
;; 输入：HTML 字符串 + base-dir（源文件所在目录）
;; 输出：重写后的 HTML
;; 行为：
;;   - data:image/...;base64,... → 解码为 bytes
;;   - 本地路径/file: URL → 去 query/fragment、percent-decode 后读文件
;;   - http(s):// 外链 → 跳过（保留原 src）
;;   - 其它 scheme → 明确拒绝
;;   - 本地图片每次通过知乎图片接口取得远端 URL

(defun zhihu--decode-data-url (s)
  "data:URL 字符串解析为 (MIME . BYTES)。失败返回 nil。"
  (when (string-match "^data:\\([^;,]+\\)\\(?:;[^,]*\\)?,\\(.*\\)" s)
    (let ((mime (match-string 1 s))
          (payload (match-string 2 s))
          (params (match-string 0 s)))
      (cond
       ((string-match-p ";base64," params)
        (cons mime (base64-decode-string payload)))
       (t  ; URL-encoded
        (cons mime (url-unhex-string payload)))))))

(defun zhihu--normalize-img-src (src base-dir)
  "把非 data 图片 SRC 归一化为绝对本地路径。
相对路径以 BASE-DIR 为根。解析前移除 query/fragment，路径中的 percent
encoding 按 UTF-8 解码。HTTP(S) 及协议相对 URL 返回 `external'；
`file:' URL 仅接受空 host 或 localhost，其它 scheme 报错。"
  (unless (and (stringp base-dir) (not (string-empty-p base-dir)))
    (error "zhihu: 解析本地图片需要非空 base-dir"))
  (let* ((reference
          (substring src 0 (or (string-match "[?#]" src) (length src))))
         (parsed (url-generic-parse-url reference))
         (scheme (url-type parsed))
         (host (url-host parsed)))
    (cond
     ((or (member scheme '("http" "https"))
          (and (null scheme) host))
      'external)
     ((and scheme (not (string-equal scheme "file")))
      (error "zhihu: 不支持图片 URL scheme：%s" scheme))
     ((and host
           (not (string-empty-p host))
           (not (string-equal (downcase host) "localhost")))
      (error "zhihu: 不支持远程 file URL host：%s" host))
     (t
      (let ((encoded-path
             (if scheme (url-filename parsed) reference)))
        (when (string-empty-p encoded-path)
          (error "zhihu: 图片 src 缺少本地路径"))
        (expand-file-name
         (decode-coding-string
          (url-unhex-string encoded-path)
         'utf-8)
         base-dir))))))

(defun zhihu--require-image-mime (mime)
  "返回图片 MIME；MIME 不是 image/* 时立即报错。"
  (unless (and (stringp mime)
               (string-prefix-p "image/" (downcase mime)))
    (error "zhihu: 图片 MIME 类型无效：%S" mime))
  mime)

(defun zhihu--img-bytes-and-mime (src base-dir)
  "把 SRC 解释成 (MIME . BYTES)；HTTP(S) 外链返回 `external'。"
  (let
      ((image
        (if (string-prefix-p "data:" src)
            (or (zhihu--decode-data-url src)
                (error "zhihu: 无法解析图片 data URL"))
          (let ((path
                 (zhihu--normalize-img-src
                  src base-dir)))
            (if (eq path 'external)
                'external
              (unless (file-readable-p path)
                (error "zhihu: 图片文件不可读：%s" path))
              (cons
               (zhihu--require-image-mime
                (mailcap-file-name-to-mime-type path))
               (zhihu--read-file-bytes path)))))))
    (if (eq image 'external)
        'external
      (zhihu--normalize-image-data image))))

(defun zhihu--rewrite-img-srcs (html base-dir source)
  "扫描 HTML 所有 <img>，按需上传。
返回重写后的 HTML。SOURCE 透传给 `zhihu--upload-bytes'，必须是
\"answer\" 或 \"article\"。"
  (let* ((dom (zhihu--parse-html (concat "<html><body>" html "</body></html>")))
         (hosted 0)
         (skipped 0))
    (dolist (img (dom-by-tag dom 'img))
      (let ((src (dom-attr img 'src)))
        (unless (and (stringp src) (not (string-empty-p src)))
          (error "zhihu: <img> 缺少非空 src"))
        (let ((mb (zhihu--img-bytes-and-mime src base-dir)))
          (if (eq mb 'external)
              (cl-incf skipped)
            (let ((url
                   (zhihu--upload-bytes
                    (cdr mb) (car mb) source)))
              (cl-incf hosted)
              (setcdr (assq 'src (dom-attributes img)) url))))))
    (let ((body (car (dom-by-tag dom 'body))))
      (message "zhihu: 图片 %d 托管 / %d 外链跳过"
               hosted skipped)
      (zhihu--inner-html body))))

;;;; Publishing

;; Zhihu API
;;
;; 端点列表（参见 zhihu.nvim 的 lua/zhihu/api/）：
;;   POST  https://www.zhihu.com/api/v4/columns/request    (申请新专栏)
;;   POST  https://zhuanlan.zhihu.com/api/articles/drafts   (新文章草稿)
;;   PATCH https://zhuanlan.zhihu.com/api/articles/AID/draft(更新文章草稿)
;;   POST  https://www.zhihu.com/api/v4/content/publish     (发布)
;;   POST  https://www.zhihu.com/api/v4/columns/CID/items   (收录进专栏)

(defconst zhihu--column-request-endpoint
  "https://www.zhihu.com/api/v4/columns/request"
  "申请创建普通知乎专栏的端点。")

(defconst zhihu--column-request-referer
  "https://www.zhihu.com/column/request"
  "申请创建普通知乎专栏时使用的网页来源。")

(defconst zhihu--current-user-endpoint
  "https://www.zhihu.com/api/v4/me"
  "读取当前登录知乎账号的端点。")

(defconst zhihu--writable-columns-page-size 50
  "每次读取当前账号可投稿专栏的数量。")

(defconst zhihu--writable-columns-include
  "data[*].column.articles_count,voteup_count"
  "可投稿专栏列表请求的字段扩展。")

(defconst zhihu--column-disallowed-emoji-regexp
  (format "[%c-%c]" #x1f300 #x1f64f)
  "知乎专栏申请表当前拒绝的 emoji 范围。")

(defconst zhihu--column-js-whitespace-regexp
  (format
   "\\`[%c-%c%c%c%c%c-%c%c-%c%c%c%c%c]+\\'"
   #x0009 #x000d #x0020 #x00a0 #x1680
   #x2000 #x200a #x2028 #x2029 #x202f
   #x205f #x3000 #xfeff)
  "匹配 ECMAScript `\\s' 当前包含的一个或多个字符。")

(defun zhihu--response-error-message (json &optional body)
  "从知乎响应 JSON 或原始 BODY 中提取错误信息。"
  (let ((nested (and (listp json) (plist-get json :error))))
    (or (and nested (plist-get nested :message))
        (and (listp json) (plist-get json :message))
        (and (stringp body)
             (let ((body (string-trim body)))
               (unless (string-empty-p body)
                 body)))
        "未知错误")))

(defun zhihu--successful-status-p (status)
  "STATUS 是否是 HTTP 2xx 成功状态码。"
  (and (integerp status) (<= 200 status) (< status 300)))

(defun zhihu--response-request-id (resp)
  "从 RESP 中取得知乎请求 ID，便于追查空响应。"
  (alist-get 'zhi-request-id (plist-get resp :headers)))

(defun zhihu--signal-column-read-http-error (action resp)
  "根据 RESP 报告只读专栏 ACTION 的 HTTP 错误。"
  (let ((status (plist-get resp :status))
        (json (plist-get resp :json))
        (body (plist-get resp :body)))
    (if (memq status '(401 403))
        (user-error
         (concat
          "zhihu: %s失败 (HTTP %s)：所选浏览器中的知乎登录状态不可用；"
          "请登录并完成验证后重试")
         action status)
      (error "zhihu: %s失败 (HTTP %s)：%s"
             action status (zhihu--response-error-message json body)))))

(defun zhihu--current-user-url-token ()
  "返回当前登录知乎账号经过校验的 url_token。"
  (let* ((resp
          (zhihu--http-json "GET" zhihu--current-user-endpoint))
         (status (plist-get resp :status))
         (json (plist-get resp :json)))
    (unless (zhihu--successful-status-p status)
      (zhihu--signal-column-read-http-error "读取当前账号" resp))
    (unless (and (listp json) (plist-member json :url_token))
      (error "zhihu: 当前账号响应缺少 url_token"))
    (let ((url-token (plist-get json :url_token)))
      (unless (and (stringp url-token)
                   (not (string-empty-p (string-trim url-token)))
                   (equal url-token (string-trim url-token))
                   (not (string-match-p "[[:cntrl:]]" url-token)))
        (error "zhihu: 当前账号响应中的 url_token 无效"))
      url-token)))

(defun zhihu--writable-column-count (value field offset index)
  "校验专栏计数 VALUE，并在错误中标明 FIELD、OFFSET 和 INDEX。"
  (unless (and (integerp value) (>= value 0))
    (error "zhihu: 可投稿专栏响应 offset %d 第 %d 项的 %s 无效"
           offset index field))
  value)

(defun zhihu--writable-column-record (item offset index)
  "把分页响应中的 ITEM 严格转换成一条可投稿专栏记录。
OFFSET 和 INDEX 只用于定位畸形响应。"
  (unless (and (listp item) (plist-member item :column))
    (error "zhihu: 可投稿专栏响应 offset %d 第 %d 项缺少 column"
           offset index))
  (let* ((column (plist-get item :column))
         (id
          (and
           (listp column)
           (zhihu--value-string (plist-get column :id))))
         (title (and (listp column) (plist-get column :title))))
    (unless (and id
                 (equal id (string-trim id))
                 (not (string-match-p "[[:cntrl:]]" id)))
      (error "zhihu: 可投稿专栏响应 offset %d 第 %d 项的 column.id 无效"
             offset index))
    (unless (and (stringp title)
                 (not (string-empty-p (string-trim title)))
                 (equal title (string-trim title))
                 (not (string-match-p "[[:cntrl:]]" title)))
      (error
       "zhihu: 可投稿专栏响应 offset %d 第 %d 项的 column.title 无效"
       offset index))
    (list
     :id id
     :title title
     :articles-count
     (zhihu--writable-column-count
      (plist-get column :articles_count) "column.articles_count"
      offset index)
     :voteup-count
     (zhihu--writable-column-count
      (plist-get column :voteup_count) "column.voteup_count"
      offset index)
     :contributions-count
     (zhihu--writable-column-count
      (plist-get item :contributions_count) "contributions_count"
      offset index))))

(defun zhihu--writable-columns-page (url-token offset)
  "读取 URL-TOKEN 对应账号从 OFFSET 开始的一页可投稿专栏。"
  (let* ((url
          (format
           (concat
            "https://www.zhihu.com/api/v4/members/%s/column-contributions"
            "?offset=%d&limit=%d&include=%s")
           (url-hexify-string url-token)
           offset
           zhihu--writable-columns-page-size
           (url-hexify-string zhihu--writable-columns-include)))
         (resp (zhihu--http-json "GET" url))
         (status (plist-get resp :status))
         (json (plist-get resp :json)))
    (unless (zhihu--successful-status-p status)
      (zhihu--signal-column-read-http-error "读取可投稿专栏" resp))
    (unless (and (listp json)
                 (plist-member json :paging)
                 (plist-member json :data))
      (error "zhihu: 可投稿专栏响应 offset %d 缺少 paging 或 data"
             offset))
    (let* ((paging (plist-get json :paging))
           (data (plist-get json :data))
           (is-end (and (listp paging) (plist-get paging :is_end)))
           (totals (and (listp paging) (plist-get paging :totals))))
      (unless (and (listp paging)
                   (plist-member paging :is_end)
                   (memq is-end '(t :json-false)))
        (error "zhihu: 可投稿专栏响应 offset %d 的 paging.is_end 无效"
               offset))
      (unless (and (plist-member paging :totals)
                   (integerp totals)
                   (>= totals 0))
        (error "zhihu: 可投稿专栏响应 offset %d 的 paging.totals 无效"
               offset))
      (unless (vectorp data)
        (error "zhihu: 可投稿专栏响应 offset %d 的 data 不是数组"
               offset))
      (let (records)
        (dotimes (index (length data))
          (push
           (zhihu--writable-column-record
            (aref data index) offset index)
           records))
        (list :records (nreverse records)
              :count (length data)
              :is-end (eq is-end t)
              :totals totals)))))

(defun zhihu--writable-columns ()
  "返回当前登录账号全部可投稿专栏的已校验记录。
记录至少包含 `:id'、`:title'、`:articles-count'、`:voteup-count'
和 `:contributions-count'。分页中途失败时不返回部分结果。"
  (let ((url-token (zhihu--current-user-url-token))
        (offset 0)
        (seen (make-hash-table :test #'equal))
        records
        done)
    (while (not done)
      (let* ((page (zhihu--writable-columns-page url-token offset))
             (page-records (plist-get page :records))
             (count (plist-get page :count))
             (is-end (plist-get page :is-end))
             (totals (plist-get page :totals)))
        (dolist (record page-records)
          (let ((id (plist-get record :id)))
            (unless (gethash id seen)
              (puthash id t seen)
              (push record records))))
        (if is-end
            (setq done t)
          (when (zerop count)
            (error
             (concat
              "zhihu: 可投稿专栏分页在 offset %d 返回空页，"
              "但 paging.is_end 为 false")
             offset))
          (let ((next-offset (+ offset count)))
            (when (>= next-offset totals)
              (error
               (concat
                "zhihu: 可投稿专栏分页在 offset %d 未结束，"
                "但下一 offset %d 已达到 totals %d")
               offset next-offset totals))
            (setq offset next-offset)))))
    (nreverse records)))

(defun zhihu--normalize-new-column-title (title)
  "按知乎申请表规则归一化新专栏标题 TITLE。"
  (unless (stringp title)
    (user-error "zhihu: 专栏名称必须是字符串"))
  (when (string-match-p "[[:cntrl:]]" title)
    (user-error "zhihu: 专栏名称不能包含换行或控制字符"))
  (when
      (or
       (string-empty-p title)
       (string-match-p zhihu--column-js-whitespace-regexp title))
    (user-error "zhihu: 专栏名称不能为空"))
  (setq title (string-trim title))
  (when (string-empty-p title)
    (user-error "zhihu: 专栏名称不能为空"))
  (when (> (length title) 20)
    (user-error "zhihu: 专栏名称最多 20 个字符（当前 %d）"
                (length title)))
  (when (string-match-p zhihu--column-disallowed-emoji-regexp title)
    (user-error "zhihu: 专栏名称不能包含知乎申请表不接受的 emoji"))
  title)

(defun zhihu--normalize-new-column-intro (intro)
  "按知乎申请表规则归一化可选的新专栏简介 INTRO。"
  (unless (or (null intro) (stringp intro))
    (user-error "zhihu: 专栏简介必须是字符串"))
  (setq intro (or intro ""))
  (when (and (not (string-empty-p intro))
             (string-match-p
              zhihu--column-js-whitespace-regexp intro))
    (user-error "zhihu: 专栏简介不能只包含空白字符"))
  (when (> (length intro) 1000)
    (user-error "zhihu: 专栏简介最多 1000 个字符（当前 %d）"
                (length intro)))
  (when (string-match-p zhihu--column-disallowed-emoji-regexp intro)
    (user-error "zhihu: 专栏简介不能包含知乎申请表不接受的 emoji"))
  intro)

(defun zhihu--normalize-created-column-id (value)
  "归一化专栏创建响应中的 ID VALUE；畸形值返回 nil。"
  (when-let* ((id (zhihu--value-string value)))
    (setq id (string-trim id))
    (and (not (string-empty-p id))
         (not (string-match-p "[[:cntrl:]]" id))
         id)))

(defun zhihu--signal-column-create-result-unknown (detail)
  "以 DETAIL 报告无法确认专栏申请结果。"
  (signal
   'zhihu-create-result-unknown
   (list
    (format
     (concat
      "zhihu: 无法确认新建专栏的结果（%s）；"
      "申请可能已经提交，请先到知乎检查，勿直接重试")
     detail))))

(defun zhihu--column-manual-censor-result (payload)
  "解析专栏申请响应 PAYLOAD，返回 `pending' 或新专栏 ID。
同时兼容网页接口出现过的 `manualCensor' 与 `manual_censor' 字段；
字段必须明确存在，且只能是 JSON boolean。"
  (let* ((camel-member
          (and (listp payload) (plist-member payload :manualCensor)))
         (snake-member
          (and (listp payload) (plist-member payload :manual_censor)))
         (members (delq nil (list camel-member snake-member)))
         modes)
    (unless members
      (zhihu--signal-column-create-result-unknown
       "成功响应缺少 manualCensor"))
    (dolist (member members)
      (let ((value (cadr member)))
        (push
         (cond
           ((eq value t) 'pending)
           ((eq value :json-false) 'created)
          (t
           (zhihu--signal-column-create-result-unknown
            "manualCensor 不是布尔值")))
         modes)))
    (unless (cl-every (lambda (mode) (eq mode (car modes))) (cdr modes))
      (zhihu--signal-column-create-result-unknown
       "manualCensor 字段彼此冲突"))
    (if (eq (car modes) 'pending)
        'pending
      (or (zhihu--normalize-created-column-id (plist-get payload :id))
          (zhihu--signal-column-create-result-unknown
           "免审成功响应缺少专栏 ID")))))

(defun zhihu--create-column (title &optional intro)
  "申请创建名为 TITLE、简介为 INTRO 的普通知乎专栏。
立即创建成功时返回专栏 ID；进入人工审核时返回 `pending'。
这是非幂等操作；结果不明时不会自动重试。"
  (setq title (zhihu--normalize-new-column-title title)
        intro (zhihu--normalize-new-column-intro intro))
  (let* ((xsrf-state (zhihu--make-xsrf-state))
         ;; Bootstrap 在 POST 之前完成；它失败时可以确定申请尚未发送。
         (xsrf-token (zhihu--ensure-xsrf-token xsrf-state))
         (resp
          (condition-case err
              (zhihu--http-json
               "POST" zhihu--column-request-endpoint
               :body `(:title ,title
                       :intro ,intro
                       :intro_type "plain")
               :extra-headers
               `(("x-xsrftoken" . ,xsrf-token)
                 ("Origin" . "https://www.zhihu.com")
                 ("Referer" . ,zhihu--column-request-referer))
               :xsrf-state xsrf-state)
            (plz-error
             (zhihu--signal-column-create-result-unknown
              (error-message-string err)))
            (quit
             (zhihu--signal-column-create-result-unknown
              "请求期间操作被中断"))))
         (status (plist-get resp :status))
         (json (plist-get resp :json)))
    (cond
     ((zhihu--successful-status-p status)
      (zhihu--column-manual-censor-result
       (and (listp json) (plist-get json :payload))))
     ((memq status '(401 403))
      (user-error
       "zhihu: 新建专栏失败 (HTTP %s)：请确认浏览器已登录并完成知乎验证；%s"
       status
       (zhihu--response-error-message json (plist-get resp :body))))
     ((and (integerp status)
           (<= 400 status) (< status 500)
           (/= status 408))
      (user-error "zhihu: 新建专栏失败 (HTTP %s)：%s"
                  status
                  (zhihu--response-error-message
                   json (plist-get resp :body))))
     (t
      (zhihu--signal-column-create-result-unknown
       (if (integerp status)
           (format "HTTP %s：%s"
                   status
                   (zhihu--response-error-message
                    json (plist-get resp :body)))
         "响应缺少 HTTP 状态码"))))))

(defun zhihu--create-article-draft (xsrf-state title html)
  "创建标题为 TITLE、正文为 HTML 的文章草稿，并返回 article ID。"
  (let* ((url "https://zhuanlan.zhihu.com/api/articles/drafts")
         (body `(:title ,title
			:content ,html
			:delta_time ,(alist-get 'delta_time zhihu-publish-defaults)
			:can_reward
			,(alist-get 'can_reward zhihu-publish-defaults)))
         (resp
          (zhihu--zhuanlan-mutation-request
           xsrf-state "POST" url
           "https://zhuanlan.zhihu.com/write"
           :body body))
         (json (plist-get resp :json)))
    (unless (zhihu--successful-status-p (plist-get resp :status))
      (error "zhihu: 创建文章草稿失败 (%s)：%s"
             (plist-get resp :status)
             (zhihu--response-error-message json (plist-get resp :body))))
    (zhihu--value-string (plist-get json :id))))

(defun zhihu--article-topic-record (object source)
  "把 SOURCE 返回的 OBJECT 归一化成话题记录。"
  (unless (and (listp object) object)
    (error "zhihu: %s 中的话题不是 JSON object" source))
  (let ((id (zhihu--value-string (plist-get object :id)))
        (name (zhihu--normalize-topic-name (plist-get object :name))))
    (list :id id :name name :object object)))

(defun zhihu--article-topic-records (array source)
  "把 SOURCE 返回的 JSON ARRAY 转成话题记录列表。"
  (unless (vectorp array)
    (error "zhihu: %s 的 topics 不是 JSON array" source))
  (mapcar
   (lambda (object)
     (zhihu--article-topic-record object source))
   (append array nil)))

(defun zhihu--search-article-topics (query)
  "搜索与 QUERY 匹配的知乎文章话题，返回话题记录列表。"
  (setq query (string-trim (or query "")))
  (when (string-empty-p query)
    (user-error "zhihu: 话题搜索词不能为空"))
  (let* ((url
          (concat
           "https://zhuanlan.zhihu.com/api/autocomplete/topics?token="
           (url-hexify-string query)
           "&max_matches=5&use_similar=0&topic_filter=1"))
         ;; 该公开补全端点不需要浏览器 Cookie。
         (resp (zhihu--http "GET" url))
         (status (plist-get resp :status))
         (body (plist-get resp :body)))
    (unless (zhihu--successful-status-p status)
      (error "zhihu: 搜索文章话题失败 (HTTP %s)：%s"
             status (zhihu--response-error-message nil body)))
    (let ((array
           (json-parse-string body
                              :object-type 'plist
                              :array-type 'array
                              :null-object :json-null
                              :false-object :json-false)))
      (zhihu--article-topic-records
       array "文章话题补全响应"))))

(defun zhihu--article-topic-candidate-label (record)
  "返回话题 RECORD 在补全界面中的显示文本。"
  (let* ((object (plist-get record :object))
         (description
          (cl-find-if
           (lambda (value)
             (and (stringp value)
                  (not (string-empty-p (string-trim value)))))
           (list (plist-get object :introduction)
                 (plist-get object :excerpt))))
         (description
          (and description
               (replace-regexp-in-string
                "[[:space:]]+" " " (string-trim description)))))
    (if description
        (format "%s — %s (%s)"
                (plist-get record :name)
                description
                (plist-get record :id))
      (format "%s (%s)"
              (plist-get record :name)
              (plist-get record :id)))))

(defun zhihu--article-topic-completion-entries (records)
  "把话题 RECORDS 转为补全使用的 `(LABEL . RECORD)' 列表。"
  (mapcar
   (lambda (record)
     (cons (zhihu--article-topic-candidate-label record)
           record))
   records))

(defun zhihu--get-article-topics (xsrf-state article-id)
  "读取 ARTICLE-ID 的草稿话题，返回话题记录列表。"
  (setq article-id (zhihu--value-string article-id))
  (unless (and article-id
               (string-match-p "\\`[0-9]+\\'" article-id))
    (error "zhihu: 无效的 article-id：%s" article-id))
  (let* ((url
          (format "https://zhuanlan.zhihu.com/api/articles/%s/draft"
                  (url-hexify-string article-id)))
         (referer
          (format "https://zhuanlan.zhihu.com/p/%s/edit" article-id))
         (resp
          (zhihu--http-json
           "GET" url
           :extra-headers `(("Referer" . ,referer))
           :sign-json nil
           :xsrf-state xsrf-state))
         (status (plist-get resp :status))
         (json (plist-get resp :json)))
    (unless (eq status 200)
      (error "zhihu: 读取文章话题失败 (%s)：%s"
             status
             (zhihu--response-error-message json (plist-get resp :body))))
    (zhihu--article-topic-records
     (plist-get json :topics)
     "文章草稿响应")))

(defun zhihu--mutate-article-topic
    (xsrf-state article-id method suffix body action)
  "对 ARTICLE-ID 的话题执行 METHOD/SUFFIX/BODY，并以 ACTION 报错。"
  (let* ((url
          (format "https://zhuanlan.zhihu.com/api/articles/%s/topics%s"
                  (url-hexify-string article-id)
                  suffix))
         (resp
          (zhihu--zhuanlan-mutation-request
           xsrf-state method url
           (format "https://zhuanlan.zhihu.com/p/%s/edit" article-id)
           :body body))
         (status (plist-get resp :status))
         (json (plist-get resp :json)))
    (unless (zhihu--successful-status-p status)
      (error "zhihu: %s失败 (%s)：%s"
             action status
             (zhihu--response-error-message json (plist-get resp :body))))
    resp))

(defun zhihu--resolve-article-topic (name)
  "把话题 NAME 精确解析成补全端点返回的话题记录。"
  (or
   (cl-find-if
    (lambda (record)
      (equal (plist-get record :name) name))
    (zhihu--search-article-topics name))
   (error
    "zhihu: 找不到名称完全匹配的话题 %S；请重新选择文章话题"
    name)))

(defun zhihu--same-string-set-p (left right)
  "LEFT 与 RIGHT 是否包含相同的一组字符串。"
  (equal (sort (copy-sequence left) #'string<)
         (sort (copy-sequence right) #'string<)))

(defun zhihu--sync-article-topics (xsrf-state article-id topics)
  "把 ARTICLE-ID 的远端话题严格同步为本地 TOPICS 名称列表。"
  (setq topics (zhihu--normalize-metadata-topics topics))
  (let* ((remote (zhihu--get-article-topics xsrf-state article-id))
         (remote-ids
          (mapcar (lambda (record) (plist-get record :id)) remote))
         ;; 已经绑定且名称完全相等的话题不必再次请求补全。
         (target
          (mapcar
           (lambda (name)
             (or
              (cl-find-if
               (lambda (record)
                 (equal (plist-get record :name) name))
               remote)
              (zhihu--resolve-article-topic name)))
           topics))
         (target-ids
          (mapcar (lambda (record) (plist-get record :id)) target))
         (to-delete
          (cl-remove-if
           (lambda (record)
             (member (plist-get record :id) target-ids))
           remote))
         (to-add
          (cl-remove-if
           (lambda (record)
             (member (plist-get record :id) remote-ids))
           target)))
    (when (or to-delete to-add)
      ;; 先解绑以便在远端已有三个话题时为新话题腾出名额。
      (dolist (record to-delete)
        (let ((id (plist-get record :id))
              (name (plist-get record :name)))
          (message "zhihu: 解绑文章话题 %s..." name)
          (zhihu--mutate-article-topic
           xsrf-state article-id "DELETE"
           (concat "/" (url-hexify-string id))
           nil
           (format "解绑文章话题 %s" name))))
      (dolist (record to-add)
        (let ((name (plist-get record :name)))
          (message "zhihu: 绑定文章话题 %s..." name)
          (zhihu--mutate-article-topic
           xsrf-state article-id "POST" ""
           (plist-get record :object)
           (format "绑定文章话题 %s" name))))
      (let* ((final
              (zhihu--get-article-topics xsrf-state article-id))
             (final-ids
              (mapcar (lambda (record) (plist-get record :id)) final))
             (final-names
              (mapcar (lambda (record) (plist-get record :name)) final)))
        (unless (and (zhihu--same-string-set-p target-ids final-ids)
                     (zhihu--same-string-set-p topics final-names))
          (error "zhihu: 文章话题同步后校验失败：本地 %S，远端 %S"
                 topics final-names))))
    topics))

(defun zhihu--article-in-column-p (xsrf-state article-id column-id)
  "查询 ARTICLE-ID，并判断它是否已收录于 COLUMN-ID。"
  (let* ((article-id (zhihu--value-string article-id))
         (column-id (zhihu--value-string column-id))
         (url (format "https://www.zhihu.com/api/v4/articles/%s"
                      (url-hexify-string article-id)))
         (resp
          (zhihu--http-json "GET" url :xsrf-state xsrf-state))
         (status (plist-get resp :status))
         (json (plist-get resp :json)))
    (unless (and (eq status 200) (listp json))
      (error "zhihu: 查询文章专栏失败 (%s)：%s"
             status
             (zhihu--response-error-message json (plist-get resp :body))))
    (let ((column (plist-get json :column)))
      (and (listp column)
           (equal (zhihu--value-string (plist-get column :id))
                  column-id)))))

(defun zhihu--add-article-to-column (xsrf-state column-id article-id)
  "把 ARTICLE-ID 对应的已发布文章收录进 COLUMN-ID。"
  (let* ((url (format "https://www.zhihu.com/api/v4/columns/%s/items"
                      (url-hexify-string column-id)))
         (body `(:type "article" :id ,article-id))
         (resp
          (zhihu--zhuanlan-mutation-request
           xsrf-state "POST" url
           (format "https://zhuanlan.zhihu.com/p/%s/edit" article-id)
           :body body))
         (status (plist-get resp :status))
         (json (plist-get resp :json)))
    (unless (zhihu--successful-status-p status)
      (error "zhihu: 收录进专栏 %s 失败 (%s)：%s"
             column-id status
             (zhihu--response-error-message json (plist-get resp :body))))
    resp))

;; 发布端点的 body 与当前知乎 web bundle 的 answer builder 逐字段
;; 对齐。include 是前端要求后端一起返回的字段列表。

(defconst zhihu--publish-include-string
  "is_contain_ai_content,is_visible,paid_info,paid_info_content,has_column,admin_closed_comment,reward_info,annotation_action,annotation_detail,collapse_reason,is_normal,is_sticky,collapsed_by,suggest_edit,comment_count,thanks_count,favlists_count,can_comment,content,editable_content,voteup_count,reshipment_settings,comment_permission,created_time,updated_time,review_info,relevant_info,question,excerpt,attachment,content_source,is_labeled,endorsements,reaction_instruction,reaction,ip_info,relationship.is_authorized,voting,is_thanked,is_author,is_nothelp,is_favorited;author.vip_info,kvip_info,badge[*].topics;settings.table_of_content.enabled")

(defun zhihu--resolve-comment-permission
    (value &optional allowed-values)
  "解析评论权限 VALUE，并在缺失时使用全局默认值。
ALLOWED-VALUES 缺省为回答/文章使用的枚举。"
  (setq allowed-values
        (or allowed-values zhihu--comment-permission-api-values))
  (or
   (zhihu--normalize-optional-enum
    "comment-permission" value allowed-values)
   (zhihu--required-enum
    "zhihu-publish-default-comment-permission"
    zhihu-publish-default-comment-permission
    allowed-values)))

(cl-defun zhihu--resolve-publish-settings
    (&key creation-statement content-source toc
          reprint-permission comment-permission)
  "解析稿件 metadata 和全局默认值，返回发布 payload 使用的设置。"
  (let* ((creation-statement
          (zhihu--normalize-metadata-creation-statement
           creation-statement))
         (content-source
          (zhihu--normalize-metadata-content-source content-source))
         (toc
          (zhihu--normalize-metadata-boolean
           "toc" toc))
         (reprint-permission
          (or
           (zhihu--normalize-optional-enum
            "reprint-permission"
            reprint-permission
            zhihu--reprint-permission-api-values)
           (zhihu--required-enum
            "zhihu-publish-default-reprint-permission"
            zhihu-publish-default-reprint-permission
            zhihu--reprint-permission-api-values)))
         (comment-permission
          (zhihu--resolve-comment-permission comment-permission)))
    `((disclaimer_status . ,(if creation-statement "open" "close"))
      (disclaimer_type . ,(or creation-statement "none"))
      (content_source . ,content-source)
      (thank_inviter_status . "close")
      (thank_inviter . "")
      (table_of_contents
       . ,(if toc t :json-false))
      (reshipment_settings . ,reprint-permission)
      (comment_permission . ,comment-permission)
      (draft_type . ,(alist-get 'draft_type zhihu-publish-defaults))
      (can_reward . ,(alist-get 'can_reward zhihu-publish-defaults)))))

(cl-defun zhihu--publish-data
    (settings draft &key question-id html)
  "从 SETTINGS 和 DRAFT 构造发布 payload 中公共的 data。
QUESTION-ID 非 nil 时写入 extra_info；HTML 非 nil 时加入回答正文。"
  (let* ((web-editor-options
          `(:reward_setting
            (:can_reward ,(alist-get 'can_reward settings) :tagline "")
            :reshipment_settings
            ,(alist-get 'reshipment_settings settings)
            :thank_inviter ,(alist-get 'thank_inviter settings)
            :comment_permission ,(alist-get 'comment_permission settings)
            :commercial_zhitask_bind_info :json-null
            :is_report :json-false
            :push_activity :json-false
            :thank_inviter_status
            ,(alist-get 'thank_inviter_status settings)
            :table_of_contents_enabled
            ,(alist-get 'table_of_contents settings)
            :disclaimer_status ,(alist-get 'disclaimer_status settings)
            :disclaimer_type ,(alist-get 'disclaimer_type settings)
            :commercial_report_info (:is_report :json-false)
            ,@(when-let* ((channel (alist-get 'content_source settings)))
                (list :content_source `(:channel ,channel)))))
         (extra-info
          `(:publisher "pc"
            :include ,zhihu--publish-include-string
            :pc_business_params
            ,(json-serialize
              web-editor-options
              :null-object :json-null
              :false-object :json-false)
            ,@(when question-id (list :question_id question-id)))))
    `(:hybridInfo
      ,(if-let* ((channel (alist-get 'content_source settings)))
           `(:contentSource (:channel ,channel))
         (make-hash-table :test 'equal))
      :toFollower ,(make-hash-table :test 'equal)
      :extra_info ,extra-info
      :draft ,draft
      :reprint
      (:reshipment_settings ,(alist-get 'reshipment_settings settings))
      :publishSwitch
      (:draft_type ,(alist-get 'draft_type settings))
      :creationStatement
      (:disclaimer_type ,(alist-get 'disclaimer_type settings)
       :disclaimer_status ,(alist-get 'disclaimer_status settings))
      :contentsTables
      (:table_of_contents_enabled ,(alist-get 'table_of_contents settings))
      :commercialReportInfo (:isReport 0)
      :thanksInvitation
      (:thank_inviter_status ,(alist-get 'thank_inviter_status settings)
       :thank_inviter ,(alist-get 'thank_inviter settings))
      :commentsPermission
      (:comment_permission ,(alist-get 'comment_permission settings))
      :appreciate
      (:can_reward ,(alist-get 'can_reward settings) :tagline "")
      ,@(when html (list :hybrid `(:html ,html))))))

(cl-defun zhihu--publish-request (body &key xsrf-state)
  "发送 BODY，并返回服务端确认的内容 ID。"
  (let* ((url "https://www.zhihu.com/api/v4/content/publish")
         (resp
          (zhihu--http-json
           "POST" url
           :body body
           :sign-json nil
           :xsrf-state xsrf-state))
         (status (plist-get resp :status))
         (response-body (plist-get resp :body))
         (json (plist-get resp :json))
         (code (and (listp json) (plist-get json :code))))
    (when (and (eq status 403)
               (stringp response-body)
               (string-match-p
                "\\\"need_login\\\"[[:space:]]*:[[:space:]]*true"
                response-body))
      (user-error
       "zhihu: 所选浏览器中的知乎登录状态不可用；请登录后重试"))
    (unless (and (zhihu--successful-status-p status)
                 json
                 (equal code 0))
      (let ((request-id (zhihu--response-request-id resp)))
        (error "zhihu: 发布失败（HTTP %s，code %s）：%s%s"
               status
               (or code "无")
               (zhihu--response-error-message json response-body)
               (if request-id
                   (format "（zhi-request-id: %s）" request-id)
                 ""))))
    (let* ((result-value (plist-get (plist-get json :data) :result))
           (result
            (cond
             ((stringp result-value)
              (condition-case nil
                  (json-parse-string result-value
                                     :null-object :json-null
                                     :false-object :json-false
                                     :object-type 'plist)
                (error nil)))
             ((listp result-value) result-value)))
           (published-id
            (or
             (zhihu--value-string
              (plist-get (plist-get result :publish) :id))
             (zhihu--value-string (plist-get result :id)))))
      (unless published-id
        (error
         "zhihu: 发布响应缺少 data.result.publish.id 或 data.result.id"))
      published-id)))

(cl-defun zhihu--publish-answer
    (answer-id question-id html
               &key creation-statement content-source
               reprint-permission comment-permission)
  "发布回答并返回服务端确认的 answer-id。"
  (let* ((settings
          (zhihu--resolve-publish-settings
           :creation-statement creation-statement
           :content-source content-source
           :reprint-permission reprint-permission
           :comment-permission comment-permission))
         (draft
          `(:disabled 1
            :isPublished ,(if answer-id t :json-false)
            ,@(when answer-id (list :contentId answer-id))))
         (body
          `(:action "answer"
            :data ,(zhihu--publish-data
                    settings draft :question-id question-id :html html))))
    (zhihu--publish-request body)))

(cl-defun zhihu--publish-article
    (article-id is-published
                &key xsrf-state creation-statement content-source
                toc
                reprint-permission comment-permission)
  "发布文章并返回服务端确认的 article-id。"
  (unless article-id
    (error "zhihu: 发布文章需要 article-id"))
  (let* ((settings
          (zhihu--resolve-publish-settings
           :creation-statement creation-statement
           :content-source content-source
           :toc toc
           :reprint-permission reprint-permission
           :comment-permission comment-permission))
         (draft
          `(:disabled 1
            :isPublished ,(if is-published t :json-false)
            :id ,article-id))
         (body
          `(:action "article"
            :data ,(zhihu--publish-data settings draft))))
    (zhihu--publish-request body :xsrf-state xsrf-state)))

;; Source creation

(defun zhihu--question-title (question-id)
  "从知乎读取 QUESTION-ID 对应的非空问题标题。"
  (let* ((url (format "https://www.zhihu.com/api/v4/questions/%s"
                      (url-hexify-string question-id)))
         (resp (zhihu--http-json "GET" url))
         (status (plist-get resp :status))
         (json (plist-get resp :json)))
    (unless (zhihu--successful-status-p status)
      (error "zhihu: 获取问题 %s 标题失败 (HTTP %s)：%s"
             question-id status
             (zhihu--response-error-message json (plist-get resp :body))))
    (string-trim (plist-get json :title))))

(defun zhihu--checkpoint-meta (file meta)
  "把 META 中的知乎渠道状态写入 FILE，并刷新其 visiting buffer。"
  (zhihu--write-zhihu-meta file meta)
  (when-let* ((buf (find-buffer-visiting (expand-file-name file))))
    (with-current-buffer buf
      (unless (buffer-modified-p)
        (revert-buffer t t t)))))

(defun zhihu--new-source-spec (file)
  "校验新源稿 FILE，返回 (绝对路径 格式)。
格式只由扩展名决定。"
  (unless (and (stringp file) (not (string-empty-p (string-trim file))))
    (user-error "zhihu: 源稿文件名不能为空"))
  (let* ((file (expand-file-name file))
         (format (zhihu--file-format file))
         (stem
          (string-trim
           (file-name-sans-extension
            (file-name-nondirectory file)))))
    ;; 相对路径可能从远程 `default-directory' 展开成 TRAMP 路径，因此必须
    ;; 检查归一化后的绝对路径。
    (when (file-remote-p file)
      (user-error "zhihu: 新建源稿不支持远程路径"))
    (unless (and format (not (file-directory-p file)))
      (user-error
       (concat
        "zhihu: 源稿目标必须是使用 .typ 或 .org 扩展名的文件路径：%s")
       file))
    (when (or (string-empty-p stem)
              (member stem '("." ".."))
              (string-match-p "[[:cntrl:]]" stem))
      (user-error "zhihu: 源稿文件名无效：%s"
                  (file-name-nondirectory file)))
    (list file format)))

(defun zhihu--format-new-source-metadata (format meta)
  "生成 FORMAT/META 对应的新源稿 metadata 区域。"
  (let ((title (or (plist-get meta :title) ""))
        (banner (plist-get meta :banner))
        (toc (plist-get meta :toc)))
    (cl-ecase format
      (typst
       (let* ((zhihu-block (zhihu--format-typst-zhihu-metadata meta))
              (block
               (concat
                (when banner
                  (format "#metadata(%S) <banner>\n" banner))
                zhihu-block)))
         (concat
          block
          (unless (string-empty-p block) "\n")
          (format "#set document(title: %S)\n\n" title)
          (when toc
            "#outline()\n\n"))))
      (org
       (concat
        (format "#+TITLE: %s\n" title)
        (when banner
          (format "#+BANNER: %s\n" banner))
        (when toc
          "#+TOC: headlines 2\n")
        (zhihu--format-org-zhihu-metadata meta)
        "\n")))))

(defun zhihu--create-source-file (file meta)
  "按 META 创建 FILE；目标路径已被占用时拒绝覆盖。"
  (pcase-let ((`(,file ,format) (zhihu--new-source-spec file)))
    (when (or (file-exists-p file) (file-symlink-p file))
      (user-error "zhihu: 源稿目标路径已被占用，拒绝覆盖：%s" file))
    (let* ((source-title
            (pcase (plist-get meta :kind)
              ('answer
               (zhihu--question-title (plist-get meta :question-id)))
              ('article (or (plist-get meta :title) ""))
              (kind (error "zhihu: 未知 metadata kind: %S" kind))))
           (source-meta
            (plist-put (copy-sequence meta) :title source-title))
           (source
            (zhihu--format-new-source-metadata format source-meta)))
      (make-directory (file-name-directory file) t)
      (with-temp-buffer
        (insert source)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region (point-min) (point-max) file nil 'silent nil 'excl)))
      (find-file file)
      (message "zhihu: 已创建 %s" file)
      file)))

;;;###autoload
(defun zhihu-new-answer (question file)
  "根据问题 ID/URL QUESTION 创建知乎回答源稿 FILE。
对应问题的标题会成为初始文档标题；创建后显式启用 `zhihu-mode'。"
  (interactive
   (list (read-string "知乎问题 ID 或 URL: ")
         (read-file-name "源稿文件（.typ/.org）: "
                         default-directory nil nil)))
  (let ((meta
         (zhihu--zhihu-meta-from-plist
          (zhihu--parse-id-or-url question))))
    (setq file (zhihu--create-source-file file meta))
    (zhihu-mode 1)
    file))

;;;###autoload
(defun zhihu-new-column (title &optional intro)
  "申请创建名称为 TITLE、可选简介为 INTRO 的普通知乎专栏。
网页接口不要求封面，本命令也不提示或上传封面。立即创建成功时返回专栏
ID；知乎将申请转入人工审核时返回 `pending'。"
  (interactive
   (list
    (read-string "知乎专栏名称: ")
    (read-string "知乎专栏简介（可空）: ")))
  (let ((result (zhihu--create-column title intro)))
    (if (eq result 'pending)
        (message "zhihu: 专栏申请已提交，正在等待知乎人工审核")
      (zhihu--invalidate-column-completion-caches)
      (message "zhihu: 已创建专栏 %s" result))
    result))

;; Publishing workflows

(defun zhihu--append-article-cc-statement (html)
  "按 `zhihu-article-cc-statement' 在 HTML 末尾追加 CC 声明引用。"
  (let* ((spec
          (pcase zhihu-article-cc-statement
           ('nil nil)
           ('cc0 'cc0)
           ('by '("by" "署名"))
           ('by-sa '("by-sa" "署名—相同方式共享"))
           ('by-nd '("by-nd" "署名—禁止演绎"))
           ('by-nc '("by-nc" "署名—非商业性使用"))
           ('by-nc-sa '("by-nc-sa" "署名—非商业性使用—相同方式共享"))
           ('by-nc-nd '("by-nc-nd" "署名—非商业性使用—禁止演绎"))
           (_
            (error "zhihu: 无效的 zhihu-article-cc-statement：%S"
                   zhihu-article-cc-statement))))
         (statement
          (pcase spec
            ('nil nil)
            ('cc0
             (concat
              "<blockquote><p>除另有声明外，本文中的原创内容已通过 "
              "<a href=\"https://creativecommons.org/publicdomain/zero/"
              "1.0/deed.zh-hans\" rel=\"license\">CC0 1.0 通用</a>"
              "，在法律允许的范围内贡献至公共领域。</p></blockquote>"))
            (`(,slug ,name)
             (format
              (concat
               "<blockquote><p>除另有声明外，本文中的原创内容采用 "
               "<a href=\"https://creativecommons.org/licenses/%s/4.0/"
               "deed.zh-hans\" rel=\"license\">CC %s 4.0"
               "（%s 4.0 协议国际版）</a> 许可。</p></blockquote>")
              slug (upcase slug) name)))))
    (if (null statement)
        html
      (concat
       html
       (unless (or (string-empty-p html) (string-suffix-p "\n" html))
         "\n")
       statement))))

(defun zhihu--package-publish-content (kind html base-dir)
  "按 KIND 打包已经统一转换完成的知乎 HTML。
HTML 在进入本函数前已经是共同的知乎方言。回答和文章上传正文图片并重写
地址；文章另加可选的 CC 声明。BASE-DIR 用于解析相对图片路径。
返回值至少包含 `:html'。"
  (pcase kind
    ('answer
     (list
      :html
      (zhihu--rewrite-img-srcs html base-dir "answer")))
    ('article
     (list
      :html
      (zhihu--append-article-cc-statement
       (zhihu--rewrite-img-srcs html base-dir "article"))))
    (_
     (error "zhihu: 不支持的发布内容类型：%S" kind))))

(defun zhihu--publish-answer-file (file meta)
  (let* ((qid (plist-get meta :question-id))
         (aid (plist-get meta :answer-id))
         (base-dir (file-name-directory (expand-file-name file)))
         (document
          (zhihu--compile-source-document
           file (plist-get meta :title)))
         (content
          (zhihu--package-publish-content
           'answer (plist-get document :html) base-dir))
         (html (plist-get content :html))
         (newly-created (null aid)))
    (setq meta (copy-sequence meta))
    (message "zhihu: 发布中...")
    (setq aid
          (zhihu--publish-answer
           aid qid html
           :creation-statement (plist-get meta :creation-statement)
           :content-source (plist-get meta :content-source)
           :reprint-permission (plist-get meta :reprint-permission)
           :comment-permission (plist-get meta :comment-permission)))
    (when newly-created
      (setq meta (plist-put meta :answer-id aid))
      (zhihu--checkpoint-meta file meta))
    (message "zhihu: 已发布 question/%s/answer/%s" qid aid)
    aid))

(defun zhihu--publish-article-file (xsrf-state file meta)
  (let* ((aid (plist-get meta :article-id))
         (column-id (plist-get meta :column-id))
         (banner (plist-get meta :banner))
         (topics (plist-get meta :topics))
         (newly-created (null aid))
         (base-dir (file-name-directory (expand-file-name file)))
         (document
          (zhihu--compile-source-document
           file (plist-get meta :title)
           (lambda (title)
             (or title
                 (user-error
                  "zhihu: 源稿缺少文档标题，无法作为文章发布")))))
         ;; 必须在上传正文图片、题图或创建远端草稿前完成校验。
         (section-link-targets
          (zhihu--article-section-link-targets
           (plist-get document :html)
           (plist-get meta :toc)))
         (title (plist-get document :title))
         (content
          (zhihu--package-publish-content
           'article (plist-get document :html) base-dir))
         (html (plist-get content :html))
         (title-image
          (cond
           ((not banner) "")
           ((string-prefix-p "data:" banner)
            (error "zhihu: banner 只支持本地图片路径"))
           (t
            (let ((image (zhihu--img-bytes-and-mime banner base-dir)))
              (when (eq image 'external)
                (error "zhihu: banner 只支持本地图片路径：%s" banner))
              (zhihu--upload-bytes
               (cdr image) (car image) "article"))))))
    (setq meta (copy-sequence meta))
    (when newly-created
      (message "zhihu: 创建文章草稿...")
      (setq aid (zhihu--create-article-draft xsrf-state title html))
      ;; 草稿 ID 立即写盘；后续失败时不会重复创建文章。
      (setq meta (plist-put meta :article-id aid))
      (zhihu--checkpoint-meta file meta))
    ;; 新文章直到创建草稿后才有 ID；取得 ID 后改写，再由下面的 PATCH
    ;; 保存最终正文并立即发布，不增加用户可见的草稿步骤。
    (setq html
          (zhihu--rewrite-article-section-links
           html aid section-link-targets))
    (message "zhihu: patch 文章草稿...")
    (let ((resp
           (zhihu--zhuanlan-mutation-request
            xsrf-state "PATCH"
            (format "https://zhuanlan.zhihu.com/api/articles/%s/draft" aid)
            (format "https://zhuanlan.zhihu.com/p/%s/edit" aid)
            :body
            `(:title ,title
              :content ,html
              :titleImage ,title-image
              :isTitleImageFullScreen :json-false
              :delta_time
              ,(alist-get 'delta_time zhihu-publish-defaults)
              :table_of_contents
              ,(if (plist-get meta :toc)
                   t
                 :json-false)
              :can_reward
              ,(alist-get 'can_reward zhihu-publish-defaults)))))
      (unless (zhihu--successful-status-p (plist-get resp :status))
        (error "zhihu: 存文章草稿失败 (%s) %s"
               (plist-get resp :status)
               (zhihu--response-error-message
                (plist-get resp :json)
                (plist-get resp :body)))))
    (message "zhihu: 同步文章话题...")
    ;; 缺失 topics 表示本地空集合，也必须执行同步以解绑远端全部话题。
    (zhihu--sync-article-topics xsrf-state aid topics)
    (message "zhihu: 发布中...")
    (zhihu--publish-article
     aid (not newly-created)
     :xsrf-state xsrf-state
     :creation-statement (plist-get meta :creation-statement)
     :content-source (plist-get meta :content-source)
     :toc
     (plist-get meta :toc)
     :reprint-permission (plist-get meta :reprint-permission)
     :comment-permission (plist-get meta :comment-permission))
    (when column-id
      (condition-case err
          (if (zhihu--article-in-column-p xsrf-state aid column-id)
              (message "zhihu: p/%s 已收录于专栏 %s" aid column-id)
            (message "zhihu: 收录进专栏 %s..." column-id)
            (zhihu--add-article-to-column xsrf-state column-id aid))
        (error
         (error "zhihu: 文章已发布为 p/%s，但处理专栏 %s 失败：%s；\
column-id 已保留，再次发布即可重试"
                aid column-id (error-message-string err)))))
    (message "zhihu: 已发布 p/%s" aid)
    aid))

;; Interactive entry points

;;;###autoload
(defun zhihu-publish ()
  "保存并发布当前回答或文章源稿。"
  (interactive)
  (message "zhihu: 开始发布...")
  (redisplay)
  (unless buffer-file-name
    (user-error "zhihu: 当前 buffer 没有对应的源稿文件"))
  (when (buffer-modified-p)
    (save-buffer))
  (let ((file buffer-file-name))
    (unless (zhihu--file-format file)
      (user-error "zhihu: 不支持的文件类型 %s" file))
    (let ((meta (zhihu--read-source-meta file)))
      (pcase (plist-get meta :kind)
        ('answer (zhihu--publish-answer-file file meta))
        ('article
         (zhihu--publish-article-file
          (zhihu--make-xsrf-state)
          file meta))
        (_ (user-error "zhihu: metadata 没有指定回答或文章"))))))

(provide 'zhihu)
;;; zhihu.el ends here
