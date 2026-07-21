;;; zhihu.el --- Write and publish on Zhihu  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Dzming Li

;; Author: Dzming Li <i@dzming.li>
;; Maintainer: Dzming Li <i@dzming.li>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (yaml "1.2.4"))
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

;; 在Emacs中撰写并发布知乎文章和回答。
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'url-cookie)
(require 'url-util)
(require 'json)
(require 'dom)
(require 'sqlite)

(defgroup zhihu nil
  "Edit and publish Zhihu answers and articles from Emacs."
  :group 'applications
  :prefix "zhihu-")

;;;; Customization

(defcustom zhihu-publish-defaults
  '((draft_type . "normal")
    (delta_time . 30)
    (disclaimer_status . "close")
    (disclaimer_type . "none")
    (thank_inviter_status . "close")
    (thank_inviter . "")
    (reshipment_settings . "allowed")
    (table_of_contents . :json-false)
    (can_reward . :json-false)
    (comment_permission . "all"))
  "发布答案/文章时的默认 settings。这些字段对应知乎创作中心
https://www.zhihu.com/creator/editor-setting 的设置项。"
  :type '(alist :key-type symbol :value-type sexp)
  :group 'zhihu)

;;;; Cookies

(defvar zhihu--session-xsrf-token nil
  "本次 Emacs 会话从知乎响应中取得的 `_xsrf' token。
Firefox 不会立刻把 session cookie 写入 cookies.sqlite，因此修改请求前
需要由 Emacs 自己访问一次知乎来取得它。")

;; Firefox profiles and cookie database

(defun zhihu--firefox-base-directory ()
  "返回当前系统的 Firefox 配置根目录；仅支持原生 Linux 和 macOS。"
  (pcase system-type
    ('gnu/linux (expand-file-name "~/.mozilla/firefox"))
    ('darwin (expand-file-name "~/Library/Application Support/Firefox"))
    (_ nil)))

(defun zhihu--firefox-cookies-candidates ()
  "自动发现 Firefox profile 中可读的 cookies.sqlite。
扫描 Linux/macOS 标准目录，并读取 profiles.ini 中的自定义 Path。"
  (when-let ((base (zhihu--firefox-base-directory)))
    (let* ((profiles-dir (if (eq system-type 'darwin)
                             (expand-file-name "Profiles" base)
                           base))
           (candidates
            (file-expand-wildcards
             (expand-file-name "*/cookies.sqlite" profiles-dir) t))
           (ini (expand-file-name "profiles.ini" base)))
      (when (file-readable-p ini)
        (with-temp-buffer
          (insert-file-contents ini)
          (goto-char (point-min))
          (let ((case-fold-search t))
            (while (re-search-forward "^Path=[ \t]*\\(.+?\\)[ \t]*$" nil t)
              (let* ((configured (match-string-no-properties 1))
                     (profile (if (file-name-absolute-p configured)
                                  configured
                                (expand-file-name configured base)))
                     (database (expand-file-name "cookies.sqlite" profile)))
                (when (file-readable-p database)
                  (push database candidates)))))))
      (delete-dups candidates))))

(defun zhihu--firefox-cookies-path ()
  "返回自动发现的最新 Firefox cookies.sqlite，找不到返回 nil。"
  (car (sort (zhihu--firefox-cookies-candidates)
             #'file-newer-than-file-p)))

(defun zhihu--select-firefox-cookies (db table host)
  "从 DB 的可信 TABLE 中选出适用于 HOST 的未过期默认容器 cookie。"
  (let ((now (floor (float-time))))
    (mapcar
     (lambda (row) (cons (car row) (cadr row)))
     (sqlite-select
      db
      (concat "SELECT name, value FROM " table " "
              "WHERE (host = ? OR host = ?) "
              "AND originAttributes = ? "
              ;; Firefox profiles in the wild use both seconds and
              ;; milliseconds for `expiry'.
              "AND expiry > CASE "
              "WHEN expiry > 100000000000 THEN ? ELSE ? END")
      (list ".zhihu.com" host "" (* now 1000) now)))))

(defun zhihu--read-firefox-cookies-readonly (path host)
  "用只读 URI ATTACH PATH，并在 SQLite/WAL 的一致读事务中查询 HOST。"
  (let (db)
    (unwind-protect
        (progn
          ;; `sqlite-open' 本身没有 readonly 参数，所以只让它创建内存主库；
          ;; Firefox 原库通过 SQLite URI 明确以 mode=ro attach。
          (setq db (sqlite-open))
          (sqlite-execute
           db "ATTACH DATABASE ? AS firefox"
           (list (concat "file:" path "?mode=ro&cache=private")))
          (zhihu--select-firefox-cookies
           db "firefox.moz_cookies" host))
      (when db (ignore-errors (sqlite-close db))))))

(defun zhihu--read-firefox-cookies-copy (path host)
  "Firefox 独占锁阻止只读 ATTACH 时，从 PATH 的临时副本查询 HOST。
顺序复制主库与 WAL 只能作为 best-effort fallback；SQLite 会校验副本。
SHM 是可重建的进程协调状态，不能复制。"
  (let ((snapshot-dir nil)
        (db nil))
    (unwind-protect
        (progn
          (setq snapshot-dir (make-temp-file "zhihu-firefox-cookies-" t))
          (dolist (suffix '("" "-wal"))
            (let ((source (concat path suffix)))
              (when (file-readable-p source)
                (copy-file source
                           (expand-file-name
                            (concat "cookies.sqlite" suffix) snapshot-dir)
                           t))))
          (setq db (sqlite-open
                    (expand-file-name "cookies.sqlite" snapshot-dir)))
          (zhihu--select-firefox-cookies db "moz_cookies" host))
      (when db (ignore-errors (sqlite-close db)))
      (when snapshot-dir
        (ignore-errors (delete-directory snapshot-dir t))))))

(defun zhihu--read-firefox-cookies (&optional host)
  "从 Firefox cookies.sqlite 读出 HOST（默认 www.zhihu.com）的 cookies。
返回 ((NAME . VALUE) ...) alist。优先以 SQLite 只读 URI 获得 WAL 一致视图；
若运行中的 Firefox 使用独占锁，再退回临时 DB/WAL 副本。读不到返回 nil。"
  (let* ((host (downcase
                (string-remove-prefix "." (or host "www.zhihu.com"))))
         (path (zhihu--firefox-cookies-path)))
    (when (and path (file-readable-p path))
      (condition-case nil
          (zhihu--read-firefox-cookies-readonly path host)
        (error
         (condition-case err
             (zhihu--read-firefox-cookies-copy path host)
           (error
            (message "zhihu: 读 Firefox cookies 失败 (%s)"
                     (error-message-string err))
            nil)))))))

(defun zhihu--cookie-header (&optional host)
  "从 Firefox 现读并构造 HTTP Cookie 头。
返回 \"k1=v1; k2=v2; ...\" 形式字符串；读取失败返回 nil（请求会照旧发出，
但服务端只能识别为未登录用户）。HOST 是本次请求的知乎域名。"
  (let* ((cookies (zhihu--read-firefox-cookies host))
         (header
          (when cookies
            (mapconcat (lambda (kv) (format "%s=%s" (car kv) (cdr kv)))
                       cookies "; "))))
    ;; `_xsrf' 通常是 session cookie；运行中的 Firefox 只把它留在内存，
    ;; cookies.sqlite 里可能没有。若本会话已经从 Set-Cookie 得到新值，
    ;; 用它覆盖 header 中可能存在的旧值。
    (if (not zhihu--session-xsrf-token)
        header
      (let* ((parts (and header (split-string header ";[ \t]*" t)))
             (parts
              (cl-remove-if
               (lambda (part)
                 (string-match-p "\\`[ \t]*_xsrf=" part))
               parts)))
        (mapconcat #'identity
                   (append parts
                           (list (concat "_xsrf=" zhihu--session-xsrf-token)))
                   "; ")))))

(defun zhihu--cookie-value (name cookie-header)
  "从 COOKIE-HEADER 中读取 NAME 的值；找不到返回 nil。"
  (when cookie-header
    (let ((case-fold-search nil)
          value)
      (dolist (part (split-string cookie-header ";[ \t]*" t))
        (when (and (null value)
                   (string-match
                    (concat "\\`[ \t]*" (regexp-quote name) "=\\(.*\\)\\'")
                    part))
          (setq value (match-string 1 part))))
      value)))

(defun zhihu--remember-xsrf-token (headers)
  "从响应 HEADERS 的 Set-Cookie 中记住 `_xsrf'。"
  (dolist (header headers)
    (when (and (string-equal (car header) "set-cookie")
               (string-match "\\`[ \t]*_xsrf=\\([^;]*\\)" (cdr header)))
      (setq zhihu--session-xsrf-token (match-string 1 (cdr header))))))

;;;; Request signing

(defvar zhihu--sign-http-json t
  "非 nil 时为知乎 JSON 请求附加 ZSE 签名。
不需要签名的发布请求会在调用点明确关闭签名。")

;; Zhihu web API 的 ZSE v4 签名。算法移植自 MIT 许可的 zhihu-sign-kt，
;; 并以当前 zhihu-cli 的兼容值和固定向量交叉验证；完整声明见本文件头部。

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
    ;; 兼容当前 zhihu-cli 的固定种子和已验证请求向量。
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
  (let* ((scheme-end (string-match "://" url))
         (authority-and-path
          (if scheme-end (substring url (+ scheme-end 3)) url))
         (slash (string-match "/" authority-and-path)))
    ;; 与 zhihu-sign-kt 的 `"/" + substringAfter("//").substringAfter('/')'
    ;; 保持一致，不对 query 再做解析或重编码。
    (concat "/" (if slash
                    (substring authority-and-path (1+ slash))
                  authority-and-path))))

(defun zhihu--zse96-header (url dc0 &optional body zse93)
  "为 URL、DC0 和可选 BODY 生成 `x-zse-96' 请求头值。"
  (let* ((source (mapconcat
                  #'identity
                  (delq nil (list (or zse93 zhihu--zse93)
                                  (zhihu--zse-path-and-query url)
                                  dc0 body))
                  "+"))
         (digest (secure-hash
                  'md5 (encode-coding-string source 'utf-8 t))))
    (concat "2.0_" (zhihu--zse-v4-encrypt digest))))

(defun zhihu--zse-request-headers (url body)
  "若 URL 属于知乎且登录 Cookie 有 d_c0，返回相应 ZSE 请求头。"
  (when (zhihu--zhihu-cookie-host-p url)
    (let* ((host (ignore-errors (url-host (url-generic-parse-url url))))
           (dc0 (zhihu--cookie-value "d_c0" (zhihu--cookie-header host))))
      (when (and dc0 (not (string-empty-p dc0)))
        `(("x-zse-93" . ,zhihu--zse93)
          ("x-zse-96" . ,(zhihu--zse96-header url dc0 body)))))))

;;;; Requests

(defvar zhihu--last-http-response nil
  "最近一次 JSON 请求的响应，供失败诊断使用。")

(defun zhihu--zhihu-cookie-host-p (url)
  "URL 是否属于可以接收知乎登录 Cookie 的 zhihu.com 域。"
  (let ((host (ignore-errors (url-host (url-generic-parse-url url)))))
    (and host
         (or (string-equal host "zhihu.com")
             (string-suffix-p ".zhihu.com" host t)))))

(defun zhihu--request-headers (&optional content-type include-cookie cookie-host)
  "构造通用请求头 alist。
INCLUDE-COOKIE 非 nil 才注入登录 Cookie，避免把 Cookie 发给 OSS、图床
或其它第三方域名。COOKIE-HOST 是请求目标的知乎域名。"
  (let ((h `(("User-Agent" .
              "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36")
             ("Accept-Language" . "zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2")
             ("x-requested-with" . "fetch")))
        (cookie (and include-cookie (zhihu--cookie-header cookie-host))))
    (when content-type
      (push (cons "Content-Type" content-type) h))
    (when cookie
      (push (cons "Cookie" cookie) h))
    h))

(defun zhihu--encode-request-headers (headers)
  "把 HEADERS 的名称和值编码成 url.el 可直接发送的 UTF-8 字节串。"
  (mapcar (lambda (header)
            (cons (encode-coding-string (car header) 'utf-8)
                  (encode-coding-string (cdr header) 'utf-8)))
          headers))

(defun zhihu--http (method url &optional body content-type extra-headers raw-body)
  "同步发送 HTTP 请求，返回 plist (:status N :headers ALIST :body STRING)。
METHOD 是 \"GET\" / \"POST\" / \"PATCH\" / \"PUT\"。BODY 字符串可空。
EXTRA-HEADERS 追加到默认头之后（同 key 后写覆盖前写）。
RAW-BODY 非 nil 时 BODY 直接当 unibyte 字节流发，不做 utf-8 编码（OSS PUT 用）。"
  (let* ((cookie-host
          (and (zhihu--zhihu-cookie-host-p url)
               (ignore-errors (url-host (url-generic-parse-url url)))))
         (request-data (and body (if raw-body body
                                   (encode-coding-string body 'utf-8))))
         (request-headers
          (let ((base (zhihu--request-headers
                       content-type (and cookie-host t) cookie-host)))
            (dolist (h extra-headers) (push h base))
            base))
         (url-request-method method)
         (url-request-data request-data)
         ;; url-http 会把 header 和 body 拼成一个 unibyte 请求；Firefox 的
         ;; SQLite API 即使只返回 ASCII Cookie，也可能给出 multibyte 字符串。
         (url-request-extra-headers
          (zhihu--encode-request-headers request-headers))
         ;; 默认 User-Agent / Accept-Encoding 会和上面的显式 header 重复。
         (url-user-agent nil)
         (url-mime-encoding-string nil)
         (url-mime-language-string nil)
         ;; 不要让 url.el 自动管 cookie——我们已经在 Cookie 头手动指定
         (url-cookie-untrusted-urls '(".*"))
         (buf (url-retrieve-synchronously url t t 30)))
    (unless buf
      (error "zhihu: %s %s 无响应（30s 超时？）" method url))
    (with-current-buffer buf
      (unwind-protect
          (let* ((status (and (boundp 'url-http-response-status)
                              url-http-response-status))
                 (headers-end (or (and (boundp 'url-http-end-of-headers)
                                       url-http-end-of-headers)
                                  (point-min)))
                 (raw-headers (buffer-substring-no-properties (point-min) headers-end))
                 (headers (zhihu--parse-headers raw-headers))
                 (body (decode-coding-string
                        (buffer-substring-no-properties headers-end (point-max))
                        'utf-8)))
            (when cookie-host
              (zhihu--remember-xsrf-token headers))
            (list :status status
                  :buffer-min (point-min)
                  :buffer-max (point-max)
                  :headers-end headers-end
                  :raw-headers raw-headers
                  :headers headers
                  :body body))
        (kill-buffer buf)))))

(defun zhihu--parse-headers (raw)
  "把 url.el 给的原始 header 块解析为 ((NAME . VALUE) ...) alist（小写化 NAME）。"
  (let (out)
    (dolist (line (split-string raw "\r?\n" t))
      (when (string-match "^\\([^:]+\\):[ \t]*\\(.*\\)$" line)
        (push (cons (downcase (match-string 1 line)) (match-string 2 line))
              out)))
    (nreverse out)))

(defun zhihu--http-json (method url &optional plist-body extra-headers)
  "PLIST-BODY 用 `json-serialize' 序列化后发送，响应 body 也按 JSON 解析。
返回 plist (:status N :headers ALIST :json PARSED :body STRING)。"
  (let* ((data (and plist-body
                    (json-serialize plist-body
                                    :null-object :json-null
                                    :false-object :json-false)))
         ;; DATA 只序列化一次：签名和 `zhihu--http' 实际发送的是同一个字符串。
         (headers (append (and zhihu--sign-http-json
                               (zhihu--zse-request-headers url data))
                          extra-headers))
         (resp (zhihu--http method url data "application/json" headers))
         (body (plist-get resp :body)))
    (setq zhihu--last-http-response
          (list :status (plist-get resp :status)
                :headers (plist-get resp :headers)
                :buffer-min (plist-get resp :buffer-min)
                :buffer-max (plist-get resp :buffer-max)
                :headers-end (plist-get resp :headers-end)
                :raw-headers (plist-get resp :raw-headers)
                :json (and body
                           (not (string-empty-p body))
                           (condition-case nil
                               (json-parse-string body
                                                  :null-object :json-null
                                                  :false-object :json-false
                                                  :object-type 'plist)
                             (error nil)))
                :body body))))

(defun zhihu--ensure-xsrf-token ()
  "返回可用于修改请求的 XSRF token，必要时先 GET 知乎取得 session cookie。"
  (or (zhihu--cookie-value
       "_xsrf" (zhihu--cookie-header "www.zhihu.com"))
      (progn
        (let ((resp (zhihu--http "GET" "https://www.zhihu.com/")))
          (unless (eq (plist-get resp :status) 200)
            (error "zhihu: 获取 XSRF token 失败 (HTTP %s)"
                   (plist-get resp :status))))
        (zhihu--cookie-value
         "_xsrf" (zhihu--cookie-header "www.zhihu.com")))
      (error "zhihu: 知乎没有下发 XSRF token；请确认 Firefox 已登录知乎")))

(defun zhihu--mutation-headers (referer origin)
  "构造修改请求需要的 XSRF、REFERER 与 ORIGIN 请求头。"
  `(("x-xsrftoken" . ,(zhihu--ensure-xsrf-token))
    ("Referer" . ,referer)
    ("Origin" . ,origin)))

;;;; Source metadata

;; URL and ID parsing

(defun zhihu--parse-id-or-url (s)
  "S 可以是裸数字 id（=question id）或知乎 URL。
返回 plist (:question-id Q :answer-id A)，A 可能为 nil。"
  (cond
   ((string-match-p "^[0-9]+$" s)
    (list :question-id s :answer-id nil))
   ((string-match
     "https?://[^/]*zhihu\\.com/question/\\([0-9]+\\)\\(?:/answer/\\([0-9]+\\)\\)?" s)
    (list :question-id (match-string 1 s)
          :answer-id (match-string 2 s)))
   (t (error "zhihu: 不认识的 id/URL: %s" s))))

(defun zhihu--parse-column-id (s)
  "把专栏 ID 或专栏 URL S 归一化为专栏 token。
专栏 ID 是类似 `hackers' 的字符串 slug，不要求是数字。"
  (let* ((s (string-trim s))
         (id
          (cond
           ((string-match
             "https?://www\\.zhihu\\.com/columns?/\\([^/?#]+\\)" s)
            (url-unhex-string (match-string 1 s)))
           ((string-match
             "https?://zhuanlan\\.zhihu\\.com/\\([^/?#]+\\)" s)
            (url-unhex-string (match-string 1 s)))
           ((string-match-p "\\`[^/?#[:space:]]+\\'" s) s))))
    (cond
     ((equal id "p")
      (user-error "zhihu: 这是文章 URL，不是专栏 URL"))
     ((and id (string-match-p "\\`[^/?#[:space:]]+\\'" id)) id)
     (t (user-error "zhihu: 不认识的专栏 ID/URL: %s" s)))))

;; Typst metadata
;;
;; 默认格式是模板无关的 Typst 原生 metadata：
;;
;;   #metadata((question-id: "...", answer-id: "...", ...)) <zhihu>
;;
;; 读取走 `typst eval ... query(<zhihu>)'。

(declare-function yaml-parse-string "yaml")
(declare-function org-collect-keywords "org" (keywords &optional unique directory))
(declare-function org-element-map "org-element" (data types fun &rest args))
(declare-function org-element-parse-buffer "org-element" (&rest args))
(declare-function org-element-property
                  "org-element" (property node &optional dflt force-undefer))

(defun zhihu--typst-root (file)
  "推断 typst `--root'，不依赖任何项目约定。
typst 里 `#import \"/p\"' / `#include \"/p\"' 这种 `/' 开头的路径是
相对 --root 解析的。扫 FILE 里所有这类绝对路径，往上找能让它们
解析成功的最近祖先目录（即存在 `<root>/p' 的那个 root）。文件没有
绝对 import 时 fallback 到 FILE 所在目录（typst 不带 --root 的默认
行为，独立文件也能编译）。"
  (let* ((file (expand-file-name file))
         (dir (file-name-directory file))
         (abs-paths
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (let (ps)
              (while (re-search-forward
                      "^[ \t]*#\\(?:import\\|include\\)[ \t]+\"\\(/[^\"]+\\)\""
                      nil t)
                (push (substring (match-string 1) 1) ps))
              (nreverse ps)))))
    (or (cl-loop
         for rel in abs-paths
         for hit = (locate-dominating-file
                    dir
                    (lambda (d) (file-exists-p (expand-file-name rel d))))
         when hit return (directory-file-name (expand-file-name hit)))
        (directory-file-name dir))))

(defun zhihu--typst-eval-json (file expression)
  "在 FILE 上执行 Typst EXPRESSION，返回 JSON 解析结果。"
  (let ((out (zhihu--shell-convert
              "typst"
              (list "eval" "--features" "html" "--target" "html"
                    "--input" "target=html"
                    "--root" (zhihu--typst-root file)
                    "--in" (expand-file-name file)
                    expression)
              "")))
    (condition-case err
        (json-parse-string out :object-type 'plist
                           :array-type 'list
                           :null-object :json-null
                           :false-object :json-false)
      (error (error "zhihu: typst eval 输出非合法 JSON: %s\n%s"
                    (error-message-string err) out)))))

(defun zhihu--typst-query-metadata (file)
  "查询 FILE 的 `<zhihu>' metadata，没有时返回 nil。"
  (let ((result
         (zhihu--typst-eval-json
          file
          (concat "let own = query(<zhihu>); "
                  "if own.len() > 0 { own.first().value } else { none }"))))
    (unless (eq result :json-null) result)))

(defun zhihu--value-string (value)
  "把 YAML/JSON 中的字符串或数字 VALUE 归一化成非空字符串。"
  (let ((s (cond ((stringp value) value)
                 ((integerp value) (number-to-string value))
                 ((numberp value) (format "%.0f" value)))))
    (and s (not (string-empty-p s)) s)))

(defun zhihu--metadata-true-p (value)
  "VALUE 是否表示 metadata 中的布尔真值。"
  (or (eq value t)
      (and (stringp value)
           (not (null
                 (member (downcase (string-trim value))
                         '("true" "yes" "t" "1")))))))

(defun zhihu--cache-key-string (key)
  "把 plist/hash 的 KEY 归一化成图片 hash 字符串。"
  (let ((s (cond ((symbolp key) (symbol-name key))
                 ((stringp key) key)
                 (t (format "%s" key)))))
    (if (string-prefix-p ":" s) (substring s 1) s)))

(defun zhihu--image-cache-table (object)
  "把 JSON/YAML OBJECT 归一化为 equal-test hash-table。"
  (let ((cache (make-hash-table :test 'equal)))
    (cond
     ((hash-table-p object)
      (maphash (lambda (k v) (puthash (zhihu--cache-key-string k) v cache))
               object))
     ((listp object)
      (cl-loop for (k v) on object by #'cddr
               when k do (puthash (zhihu--cache-key-string k) v cache))))
    cache))

(defun zhihu--zhihu-meta-from-plist (z title &optional style)
  "把 Z 归一化成发布流程共用的 metadata plist。
TITLE 提供文档标题；STYLE 标记 Typst、Markdown 或 Org 后端。
数字 ID 会可靠转换为字符串；空 article-id 仍表示尚未发布的文章。"
  (let* ((z (or z nil))
         (qid-present-p (and (plist-member z :question-id) t))
         (aid-present-p (and (plist-member z :answer-id) t))
         (article-present-p (and (plist-member z :article-id) t))
         (qid (zhihu--value-string (plist-get z :question-id)))
         (aid (zhihu--value-string (plist-get z :answer-id)))
         (art (zhihu--value-string (plist-get z :article-id)))
         (column (zhihu--value-string (plist-get z :column-id)))
         (draft (zhihu--metadata-true-p (plist-get z :draft))))
    (when (and (or article-present-p column)
               (or qid-present-p aid-present-p))
      (error "zhihu: metadata 不能同时包含文章/专栏 ID 与回答 ID"))
    (when (and qid-present-p (not qid))
      (error "zhihu: question-id 不能为空"))
    (when (and aid-present-p (not qid))
      (error "zhihu: answer-id 必须与 question-id 一起出现"))
    (list :kind (cond (qid 'answer)
                      (article-present-p 'article))
          :question-id qid
          :answer-id aid
          :article-id art
          :column-id column
          ;; 标题属于文档本身：Typst 用 `set document'，Markdown 用
          ;; frontmatter。绝不从原生 `<zhihu>' 发布 metadata 取标题。
          :title (zhihu--value-string title)
          ;; 只在首次发布被中断时暂存；成功确认后立即删除。
          :draft draft
          :metadata-style style
          :image-cache (zhihu--image-cache-table (plist-get z :image-cache)))))

(defun zhihu--typst-zhihu-meta (file)
  "从 FILE 里读取模板无关的 `<zhihu>' metadata。
返回 plist (:question-id S :answer-id S/nil :article-id S/nil :column-id S/nil
:title S/nil :image-cache HASH)。"
  (when-let ((value (zhihu--typst-query-metadata file)))
    (let ((meta (zhihu--zhihu-meta-from-plist value nil 'native)))
      (unless (plist-get meta :kind)
        (error "zhihu: Typst 的 <zhihu> metadata 缺少 question-id 或 article-id"))
      meta)))

(defun zhihu--image-cache-lines (cache fmt)
  "CACHE 每个 (key . url) 按 FMT 两参格式化成一行，按字典序排序后返回行列表。
typst dict 与 YAML 两种序列化共用。"
  (let (lines)
    (maphash (lambda (k v) (push (format fmt k v) lines)) cache)
    (sort lines #'string<)))

(defun zhihu--format-image-cache-typst (cache)
  "HASH-TABLE → 格式化的 typst dict 字符串（多行）。
空 cache → \"(:)\"。"
  (if (zerop (hash-table-count cache))
      "(:)"
    (concat "(\n"
            (mapconcat #'identity
                       (zhihu--image-cache-lines cache "    %S: %S,") "\n")
            "\n  )")))

(defun zhihu--typst-literal (value)
  "把 VALUE 格式化为 Typst 字面量；nil 输出 none。"
  (if value (format "%S" value) "none"))

(defun zhihu--format-typst-zhihu-metadata
    (question-id answer-id article-id column-id image-cache &optional meta)
  "生成模板无关、可安全重写的 Typst `<zhihu>' metadata 块。"
  (let ((image-cache (or image-cache (make-hash-table :test 'equal))))
    (concat
     "#metadata((\n"
     (if (not question-id)
         (concat
          (format "  article-id: %s,\n" (zhihu--typst-literal article-id))
          (if column-id
              (format "  column-id: %s,\n" (zhihu--typst-literal column-id))
            ""))
       (concat
        (format "  question-id: %s,\n" (zhihu--typst-literal question-id))
        (format "  answer-id: %s,\n" (zhihu--typst-literal answer-id))))
     (if (plist-get meta :draft) "  draft: true,\n" "")
     (if (zerop (hash-table-count image-cache))
         ""
       (format "  image-cache: %s,\n"
               (zhihu--format-image-cache-typst image-cache)))
     ")) <zhihu>\n")))

(defvar zhihu--typst-metadata-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    ;; Typst supports both // line comments and /* block comments */.
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table used only to locate a labeled Typst metadata call.")

(defun zhihu--typst-native-metadata-region ()
  "返回唯一的 `#metadata(...) <zhihu>' 调用区域 (BEG . END)。
允许 metadata 单行或多行排版，并忽略字符串和 Typst 注释中的伪调用。"
  (save-excursion
    (with-syntax-table zhihu--typst-metadata-syntax-table
      (let ((parse-sexp-ignore-comments t)
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

(defun zhihu--typst-write-native-metadata
    (file question-id answer-id article-id column-id image-cache &optional meta)
  "把模板无关的 `<zhihu>' metadata 安全写入 FILE。"
  (let ((block (zhihu--format-typst-zhihu-metadata
                question-id answer-id article-id column-id image-cache meta)))
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
            (goto-char (point-min))
            (re-search-forward "<zhihu>" nil t))
          (error "zhihu: <zhihu> 不是标准 #metadata((...)) 块，拒绝自动重写"))
         (t
          (goto-char (point-min))
          (insert block "\n"))))
      (write-region (point-min) (point-max) file nil 'silent))))

(defun zhihu--typst-write-zhihu-meta
    (file question-id answer-id article-id column-id image-cache &optional meta)
  "用模板无关的 `<zhihu>' metadata 写回 FILE 的知乎状态。"
  (zhihu--typst-write-native-metadata
   file question-id answer-id article-id column-id image-cache meta))

;; Markdown YAML front matter
;;
;; FILE 形如：
;;   ---
;;   title: ...
;;   zhihu:
;;     question-id: "12345"
;;     answer-id: "67890"
;;     image-cache:
;;       <hash>: "https://..."
;;   ---
;;   正文...
;;
;; 读走 yaml.el，写做 surgical regex（找 ^zhihu:\n（缩进续行块）替换）。

(defun zhihu--md-split-frontmatter (text)
  "TEXT → (FRONTMATTER-STRING-OR-NIL . BODY-STRING)。
没有 frontmatter 时第一项为 nil、第二项是原 text。"
  (if (string-match "\\`---[ \t]*\n\\(\\(?:.\\|\n\\)*?\\)\n---[ \t]*\n" text)
      (cons (match-string 1 text) (substring text (match-end 0)))
    (cons nil text)))

(defun zhihu--md-frontmatter-zhihu-meta (fm)
  "解析 Markdown front matter 字符串 FM 中的 zhihu 字段。"
  (when fm
    (unless (require 'yaml nil t)
      (error "zhihu: 读取 Markdown metadata 需要 yaml.el"))
    (let* ((parsed
            (condition-case err
                (yaml-parse-string fm
                                   :object-type 'plist
                                   :sequence-type 'list)
              (error
               (error "zhihu: Markdown front matter 解析失败：%s"
                      (error-message-string err)))))
           (z (and (listp parsed) (plist-get parsed :zhihu)))
           (title (and (listp parsed) (plist-get parsed :title))))
      (when (and (listp parsed) (plist-member parsed :zhihu))
        (unless (listp z)
          (error "zhihu: Markdown front matter 的 zhihu 必须是 mapping"))
        (let ((meta (zhihu--zhihu-meta-from-plist z title 'markdown)))
          (unless (plist-get meta :kind)
            (error "zhihu: Markdown 的 zhihu 块缺少 question-id 或 article-id"))
          meta)))))

(defun zhihu--md-zhihu-meta (file)
  "读 FILE 的 YAML frontmatter zhihu 字段。返回 plist 或 nil。"
  (let* ((text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))
         (fm (car (zhihu--md-split-frontmatter text))))
    (zhihu--md-frontmatter-zhihu-meta fm)))

(defun zhihu--md-write-zhihu-meta
    (file question-id answer-id article-id column-id image-cache &optional meta)
  "把 zhihu 字段 surgical 写入 FILE 的 YAML frontmatter。"
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((text (buffer-string))
           (split (zhihu--md-split-frontmatter text))
           (fm (car split))
           (body (cdr split))
           (new-zhihu (zhihu--format-zhihu-yaml
                       question-id answer-id article-id column-id image-cache
                       meta)))
      (cond
       ((null fm)
        ;; 整文件没 frontmatter，整段塞顶上
        (erase-buffer)
        (insert "---\n" new-zhihu "---\n" body))
       ((string-match "^zhihu:[^\n]*\\(?:\n[ \t]+.*\\)*\n?" fm)
        ;; 已有 zhihu 块，替换
        (let ((new-fm (replace-match new-zhihu t t fm)))
          (erase-buffer)
          (insert "---\n" new-fm "\n---\n" body)))
       (t
        ;; frontmatter 有但没 zhihu 字段，append
        (erase-buffer)
        (insert "---\n" fm "\n" new-zhihu "---\n" body))))
    (write-region (point-min) (point-max) file nil 'silent)))

(defun zhihu--format-zhihu-yaml
    (question-id answer-id article-id column-id image-cache &optional meta)
  "格式化 zhihu YAML 块（含末尾换行）。
QUESTION-ID 非空时写回答字段；否则总是写 article-id，空值表示新文章。"
  (let ((image-cache (or image-cache (make-hash-table :test 'equal)))
        lines)
    (push "zhihu:" lines)
    (if (not question-id)
        (progn
          (push (if article-id
                    (format "  article-id: %S" article-id)
                  "  article-id: null")
                lines)
          (when column-id
            (push (format "  column-id: %S" column-id) lines)))
      (push (format "  question-id: %S" (or question-id "")) lines)
      (push (format "  answer-id: %S" (or answer-id "")) lines))
    (when (plist-get meta :draft)
      (push "  draft: true" lines))
    (unless (zerop (hash-table-count image-cache))
      (push "  image-cache:" lines)
      (setq lines
            (append (zhihu--image-cache-lines image-cache "    %S: %S")
                    lines)))
    (concat (mapconcat #'identity (nreverse lines) "\n") "\n")))

;; Org metadata
;;
;; 标题使用标准 `#+TITLE:'；知乎渠道状态使用包自己的关键字：
;;   #+ZHIHU_QUESTION_ID: 123
;;   #+ZHIHU_ANSWER_ID: 456
;; 或：
;;   #+ZHIHU_ARTICLE_ID:
;;   #+ZHIHU_COLUMN_ID: hackers
;; 发布后空的 ARTICLE_ID 会替换为真实 ID。
;; 图片缓存是单行 JSON object。

(defconst zhihu--org-keyword-names
  '("TITLE"
    "ZHIHU_QUESTION_ID" "ZHIHU_ANSWER_ID"
    "ZHIHU_ARTICLE_ID" "ZHIHU_COLUMN_ID" "ZHIHU_DRAFT"
    "ZHIHU_IMAGE_CACHE")
  "本包从 Org 文件读取的文档级关键字。")

(defconst zhihu--org-owned-keyword-names
  (cdr zhihu--org-keyword-names)
  "本包负责重写的 Org 关键字；不含用户自己的 TITLE。")

(defun zhihu--org-collect-keywords ()
  "收集当前 buffer 的文档级知乎关键字，忽略源码块里的同名文本。"
  (require 'org)
  (unless (derived-mode-p 'org-mode)
    (delay-mode-hooks (org-mode)))
  (org-collect-keywords zhihu--org-keyword-names))

(defun zhihu--org-keyword-value (key keywords)
  "从 KEYWORDS 返回 Org KEY 的第一个非空值。"
  (when-let ((value (cadr (assoc-string key keywords t))))
    (zhihu--value-string (string-trim value))))

(defun zhihu--org-image-cache (raw)
  "把 Org 关键字里的 RAW JSON object 解析为图片缓存。"
  (if (not raw)
      (make-hash-table :test 'equal)
    (condition-case err
        (let ((object
               (json-parse-string raw
                                  :object-type 'hash-table
                                  :null-object :json-null
                                  :false-object :json-false)))
          (unless (hash-table-p object)
            (error "值必须是 JSON object"))
          (zhihu--image-cache-table object))
      (error
       (error "zhihu: ZHIHU_IMAGE_CACHE 不是合法 JSON object：%s"
              (error-message-string err))))))

(defun zhihu--org-zhihu-meta (file)
  "从 Org FILE 的 `#+ZHIHU_*' 关键字读取知乎 metadata。"
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((keywords (zhihu--org-collect-keywords))
           (qid-entry
            (assoc-string "ZHIHU_QUESTION_ID" keywords t))
           (qid (zhihu--org-keyword-value "ZHIHU_QUESTION_ID" keywords))
           (aid-entry
            (assoc-string "ZHIHU_ANSWER_ID" keywords t))
           (aid (zhihu--org-keyword-value "ZHIHU_ANSWER_ID" keywords))
           (article-id-entry
            (assoc-string "ZHIHU_ARTICLE_ID" keywords t))
           (article-id
            (zhihu--org-keyword-value "ZHIHU_ARTICLE_ID" keywords))
           (column-id
            (zhihu--org-keyword-value "ZHIHU_COLUMN_ID" keywords))
           (draft (zhihu--org-keyword-value "ZHIHU_DRAFT" keywords))
           (cache-raw
            (zhihu--org-keyword-value "ZHIHU_IMAGE_CACHE" keywords))
           (title (zhihu--org-keyword-value "TITLE" keywords)))
      (when (or qid-entry aid-entry article-id-entry column-id
                draft cache-raw)
        (let ((z (list :column-id column-id
                       :draft (zhihu--metadata-true-p draft)
                       :image-cache (zhihu--org-image-cache cache-raw))))
          (when qid-entry
            (setq z (plist-put z :question-id qid)))
          (when aid-entry
            (setq z (plist-put z :answer-id aid)))
          (when article-id-entry
            (setq z (plist-put z :article-id article-id)))
          (let ((meta (zhihu--zhihu-meta-from-plist z title 'org)))
            (unless (plist-get meta :kind)
              (error "zhihu: Org 缺少 ZHIHU_QUESTION_ID 或 ZHIHU_ARTICLE_ID"))
            meta))))))

(defun zhihu--format-org-zhihu-metadata
    (question-id answer-id article-id column-id image-cache &optional meta)
  "生成 Org 的 `#+ZHIHU_*' metadata 行。"
  (let ((image-cache (or image-cache (make-hash-table :test 'equal)))
        lines)
    (if (not question-id)
        (progn
          (push (if article-id
                    (format "#+ZHIHU_ARTICLE_ID: %s" article-id)
                  "#+ZHIHU_ARTICLE_ID:")
                lines)
          (when column-id
            (push (format "#+ZHIHU_COLUMN_ID: %s" column-id) lines)))
      (when question-id
        (push (format "#+ZHIHU_QUESTION_ID: %s" question-id) lines))
      (when answer-id
        (push (format "#+ZHIHU_ANSWER_ID: %s" answer-id) lines)))
    (when (plist-get meta :draft)
      (push "#+ZHIHU_DRAFT: true" lines))
    (unless (zerop (hash-table-count image-cache))
      (push (concat "#+ZHIHU_IMAGE_CACHE: "
                    (json-serialize image-cache
                                    :null-object :json-null
                                    :false-object :json-false))
            lines))
    (concat (mapconcat #'identity (nreverse lines) "\n") "\n")))

(defun zhihu--org-write-zhihu-meta
    (file question-id answer-id article-id column-id image-cache &optional meta)
  "把知乎 metadata 关键字写入 Org FILE，并保留其它 Org 关键字。"
  (let ((block (zhihu--format-org-zhihu-metadata
                question-id answer-id article-id column-id image-cache meta)))
    (with-temp-buffer
      (insert-file-contents file)
      (require 'org)
      (delay-mode-hooks (org-mode))
      (let (regions)
        (org-element-map (org-element-parse-buffer) 'keyword
			 (lambda (node)
			   (when (member (org-element-property :key node)
					 zhihu--org-owned-keyword-names)
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

(defun zhihu--file-format (file)
  "返回 FILE 对应的 `typst'、`markdown' 或 `org'；其它返回 nil。"
  (pcase (downcase (or (file-name-extension file) ""))
    ("typ" 'typst)
    ((or "md" "markdown") 'markdown)
    ("org" 'org)
    (_ nil)))

(defun zhihu--read-zhihu-meta (file)
  "从 FILE 读取统一的知乎 metadata；文件没有知乎 metadata 时返回 nil。"
  (pcase (zhihu--file-format file)
    ('typst (zhihu--typst-zhihu-meta file))
    ('markdown (zhihu--md-zhihu-meta file))
    ('org (zhihu--org-zhihu-meta file))
    (_ (error "zhihu: 不支持的文件类型 %s" file))))

(defun zhihu--write-zhihu-meta
    (file question-id answer-id article-id column-id image-cache &optional meta)
  "把统一知乎 metadata 写回 FILE，并尽量保留原 metadata 风格。"
  (pcase (zhihu--file-format file)
    ('typst (zhihu--typst-write-zhihu-meta
             file question-id answer-id article-id column-id image-cache meta))
    ('markdown (zhihu--md-write-zhihu-meta
                file question-id answer-id article-id column-id image-cache meta))
    ('org (zhihu--org-write-zhihu-meta
           file question-id answer-id article-id column-id image-cache meta))))

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
       ((stringp child) (insert child))
       ((consp child)   (dom-print child nil nil))))
    (buffer-string)))

;; Source formats
;;
;; Pandoc 负责 Markdown/Org → HTML，也强制规范化 Typst HTML/MathML；
;; Typst CLI 负责先把 Typst 源稿编译成语义 HTML。
;; 转换都通过临时 buffer + call-process-region，避免 shell 转义。

(defun zhihu--pandoc-normalize-html (html)
  "用 pandoc 把 HTML 规范化为便于转换的 HTML5 fragment。
尤其会把 Typst MathML 还原成带 TeX 内容的 `.math' span。"
  (zhihu--shell-convert
   "pandoc"
   '("-f" "html" "-t" "html5" "--mathjax" "--wrap=none" "--no-highlight")
   html))

(defun zhihu--math-span-p (node)
  "NODE 是否是 pandoc 输出的 math span。"
  (and (consp node)
       (eq (dom-tag node) 'span)
       (zhihu--node-has-class-p node "math")))

(defun zhihu--math-span-tex (node)
  "从 pandoc math span NODE 取出 TeX，去掉 \\(…\\) / \\[…\\]。"
  (let ((text (string-trim (dom-text node))))
    (cond
     ((and (string-prefix-p "\\(" text) (string-suffix-p "\\)" text))
      (substring text 2 -2))
     ((and (string-prefix-p "\\[" text) (string-suffix-p "\\]" text))
      (substring text 2 -2))
     (t text))))

(defun zhihu--code-language (pre code)
  "从 PRE/CODE 的属性中提取代码语言。"
  (or (dom-attr pre 'lang)
      (dom-attr pre 'data-lang)
      (dom-attr code 'data-lang)
      (cl-loop for cls in (split-string
                           (or (dom-attr code 'class) "") "[ \t\n]+" t)
               when (string-prefix-p "language-" cls)
               return (substring cls (length "language-")))
      (cl-loop for cls in (split-string
                           (or (dom-attr code 'class) "") "[ \t\n]+" t)
               unless (member cls '("sourceCode" "code")) return cls)
      ""))

(defun zhihu--zhihuify-node (node)
  "把规范 HTML 的 NODE 递归转换为知乎方言 DOM 节点。"
  (cond
   ((stringp node) node)
   ((not (consp node)) node)
   ((zhihu--math-span-p node)
    (let* ((display (zhihu--node-has-class-p node "display"))
           (tex (zhihu--math-span-tex node)))
      `(img ((eeimg . ,(if display "2" "1"))
             (src . ,(concat "//www.zhihu.com/equation?tex="
                             (url-hexify-string tex)))
             (alt . ,(replace-regexp-in-string "[\n\r]+" " " tex))))))
   ((eq (dom-tag node) 'pre)
    (let ((code (car (dom-by-tag node 'code))))
      (if code
          `(pre ((lang . ,(zhihu--code-language node code))) ,(dom-text code))
        `(pre ((lang . ,(or (dom-attr node 'lang) ""))) ,(dom-text node)))))
   (t
    (let* ((tag (dom-tag node))
           ;; 知乎会过滤 style；保留语义/data 属性，class 仅在普通节点保留。
           (attrs (cl-remove-if
                   (lambda (a) (eq (car a) 'style))
                   (copy-tree (cadr node))))
           (children (mapcar #'zhihu--zhihuify-node (dom-children node))))
      (when (eq tag 'table)
        (dolist (a '((data-draft-node . "block")
                     (data-draft-type . "table")
                     (data-size . "normal")))
          (setf (alist-get (car a) attrs) (cdr a))))
      ;; 知乎编辑器把一级标题留给文章标题。
      (when (eq tag 'h1) (setq tag 'h2))
      (cons tag (cons attrs children))))))

(defun zhihu--zhihuify-html (html)
  "把 HTML fragment 转成知乎可接受的公式、代码和表格节点。"
  (let* ((dom (zhihu--parse-html (concat "<html><body>" html "</body></html>")))
         (body (car (dom-by-tag dom 'body)))
         (new-body `(body nil ,@(mapcar #'zhihu--zhihuify-node
                                        (dom-children body)))))
    (zhihu--inner-html new-body)))

(defun zhihu--normalize-html (html)
  "用 Pandoc 规范化 Typst HTML，再转换为知乎方言 HTML。"
  (zhihu--zhihuify-html (zhihu--pandoc-normalize-html html)))

(defun zhihu--md->html (md)
  "Markdown → 知乎方言 HTML，pandoc gfm 方言。"
  (zhihu--zhihuify-html
   (zhihu--shell-convert
    "pandoc"
    '("-f" "gfm" "-t" "html5" "--mathjax" "--wrap=none" "--no-highlight")
    md)))

(defun zhihu--org->html (org-text)
  "ORG-TEXT → 知乎方言 HTML。"
  (zhihu--zhihuify-html
   (zhihu--shell-convert
    "pandoc"
    '("-f" "org" "-t" "html5" "--mathjax" "--wrap=none" "--no-highlight")
    org-text)))

;; Source conversion entry points

(defun zhihu--typst-compile-html (file)
  "把 Typst FILE 编译成保留 `<head>' 的完整 HTML。"
  (zhihu--shell-convert
   "typst"
   (list "compile" "--features=html"
         "--input" "target=html"
         "--root" (zhihu--typst-root file) "-f" "html"
         (expand-file-name file) "-")
   ""))

(defun zhihu--html-document-title (html)
  "从完整 HTML 的 `<title>' 返回纯文本标题；缺失或为空时返回 nil。"
  (when-let* ((dom (zhihu--parse-html html))
              (node (car (dom-by-tag dom 'title)))
              (title (string-trim (dom-text node))))
    (unless (string-empty-p title) title)))

(defun zhihu--source-to-html (file)
  "把 Typst、Markdown 或 Org FILE 转为知乎 HTML。
Markdown 会先剥掉 YAML frontmatter 再交给 pandoc。"
  (pcase (zhihu--file-format file)
    ('typst
     (let ((full (zhihu--typst-compile-html file)))
       (zhihu--normalize-html full)))
    ('markdown
     (let* ((text (with-temp-buffer (insert-file-contents file) (buffer-string)))
            (body (cdr (zhihu--md-split-frontmatter text))))
       (zhihu--md->html body)))
    ('org
     (zhihu--org->html
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string))))))

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

(defconst zhihu--oss-host "https://zhihu-pics-upload.zhimg.com")
(defconst zhihu--oss-bucket "zhihu-pics")

(defun zhihu--guess-mime (path)
  "按扩展名猜 MIME。不识别返回 application/octet-stream。"
  (pcase (downcase (or (file-name-extension path) ""))
    ("png"  "image/png")
    ((or "jpg" "jpeg") "image/jpeg")
    ("gif"  "image/gif")
    ("svg"  "image/svg+xml")
    ("webp" "image/webp")
    ("bmp"  "image/bmp")
    (_      "application/octet-stream")))

(defun zhihu--read-file-bytes (path)
  "PATH 读为 unibyte 字节字符串。"
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun zhihu--bytes-md5 (bytes)
  "BYTES (unibyte string) 的 hex md5。"
  (secure-hash 'md5 bytes))

(defun zhihu--hmac-sha1-base64 (key data)
  "HMAC-SHA1(KEY, DATA)，返回 base64 字符串（无换行）。
KEY/DATA 都是 string；内部按 unibyte 处理。"
  (require 'gnutls)
  (base64-encode-string
   (gnutls-hash-mac 'SHA1
                    (encode-coding-string key 'utf-8)
                    (encode-coding-string data 'utf-8))
   t))

(defun zhihu--image-prefetch (md5-hex source)
  "POST /images，告诉服务端要传一张 hash 是 MD5-HEX 的图。
SOURCE 是 \"answer\" / \"article\"。返回 parsed JSON plist：
  :upload_file (:image_id :object_key :state)  state=1 已存在 / state=2 待传
  :upload_token (:access_id :access_key :access_token :access_timestamp)"
  (let* ((resp (zhihu--http-json "POST" "https://api.zhihu.com/images"
                                 `(:image_hash ,md5-hex :source ,source))))
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
          "/" zhihu--oss-bucket "/" object-key))

(defun zhihu--image-oss-put (object-key bytes mime token)
  "PUT 二进制到 OSS。TOKEN 是 prefetch 拿到的 :upload_token plist。"
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
         (url (concat zhihu--oss-host "/" object-key))
         (extra `(("x-oss-user-agent" . ,oss-user-agent)
                  ("x-oss-date" . ,date)
                  ("x-oss-security-token" . ,access-token)
                  ("Authorization" . ,(format "OSS %s:%s" access-id signature))))
         (resp (zhihu--http "PUT" url bytes mime extra t)))
    (unless (memq (plist-get resp :status) '(200 203))
      (error "zhihu: OSS PUT %s 失败 (%s) %s"
             url (plist-get resp :status) (plist-get resp :body)))
    resp))

(defun zhihu--image-poll (image-id)
  "GET /images/<id> 直到 status=success，返回最终 src URL。"
  (let ((url (format "https://api.zhihu.com/images/%s" image-id))
        result)
    (cl-loop repeat 10 do
             (let* ((resp (zhihu--http "GET" url))
                    (j (and (eq (plist-get resp :status) 200)
                            (condition-case nil
                                (json-parse-string (plist-get resp :body)
                                                   :object-type 'plist
                                                   :null-object :json-null
                                                   :false-object :json-false)
                              (error nil)))))
               (when (and j (string= (plist-get j :status) "success"))
                 (setq result (plist-get j :src))
                 (cl-return)))
             (sleep-for 0.5))
    (or result (error "zhihu: 图片 %s poll 超时" image-id))))

(defun zhihu--upload-bytes (bytes mime &optional source)
  "上传二进制图片到知乎，返回最终 picx URL。
缓存策略由调用方负责（本函数总是发起 prefetch）；服务端按 hash 去重所以
重复传同一 hash 也会走 state=1 快速路径，但还是有一次 RTT。"
  (let* ((src (or source "answer"))
         (md5-hex (zhihu--bytes-md5 bytes))
         (pf (zhihu--image-prefetch md5-hex src))
         (file (plist-get pf :upload_file))
         (state (plist-get file :state))
         (image-id (plist-get file :image_id)))
    (cond
     ;; state=1: 服务端已有该 hash，不需要 PUT
     ((eq state 1)
      (zhihu--image-poll image-id))
     ;; state=2: 走 PUT 上传 + poll
     ((eq state 2)
      (zhihu--image-oss-put (plist-get file :object_key)
                            bytes mime
                            (plist-get pf :upload_token))
      (zhihu--image-poll image-id))
     (t
      (error "zhihu: prefetch 返回未知 state=%S" state)))))

;;;###autoload
(defun zhihu-upload-image-file (path)
  "选一张图上传，返回 picx URL（也存入 kill-ring）。"
  (interactive "f图片路径: ")
  (let* ((bytes (zhihu--read-file-bytes path))
         (mime (zhihu--guess-mime path))
         (url (zhihu--upload-bytes bytes mime)))
    (kill-new url)
    (message "zhihu: 已上传 → %s（已 kill-ring）" url)
    url))

;; HTML image rewriting
;;
;; 输入：HTML 字符串 + base-dir（源文件所在目录）+ image-cache (hash → URL hashtable)
;; 输出：(REWRITTEN-HTML . NEW-CACHE)
;; 行为：
;;   - data:image/...;base64,... → 解码为 bytes
;;   - 相对路径 → 解析到 base-dir 读文件
;;   - http(s):// 外链 → 跳过（保留原 src）
;;   - 算 md5 → 查 cache，命中复用 URL，miss 上传

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

(defun zhihu--img-bytes-and-mime (src base-dir)
  "把 SRC 解释成 (MIME . BYTES) 二元组；外链 / 解析失败 / 文件不存在返回 nil。"
  (cond
   ((string-prefix-p "data:" src)
    (zhihu--decode-data-url src))
   ((string-match-p "^https?://" src)
    nil)  ; 外链不传
   (t
    (let ((path (expand-file-name src (or base-dir default-directory))))
      (when (file-readable-p path)
        (cons (zhihu--guess-mime path)
              (zhihu--read-file-bytes path)))))))

(defun zhihu--collect-img-nodes (dom)
  "DFS 收集 DOM 里所有 img 节点。"
  (let (out)
    (cl-labels
        ((walk (n)
           (when (and (consp n) (symbolp (car n)))
             (when (eq (car n) 'img) (push n out))
             (dolist (c (dom-children n))
               (when (consp c) (walk c))))))
      (walk dom))
    (nreverse out)))

(defun zhihu--rewrite-img-srcs (html base-dir image-cache &optional source)
  "扫 HTML 所有 <img>，按需上传，返回 (NEW-HTML . NEW-CACHE)。
IMAGE-CACHE 是 hash (hex md5) → URL 的 hash-table；本函数只追加不删。
SOURCE 透传给 `zhihu--upload-bytes'（\"answer\" / \"article\"，默认 answer）。"
  (let* ((dom (zhihu--parse-html (concat "<html><body>" html "</body></html>")))
         (cache (or image-cache (make-hash-table :test 'equal)))
         (uploaded 0)
         (cached 0)
         (skipped 0))
    (dolist (img (zhihu--collect-img-nodes dom))
      (let* ((src (dom-attr img 'src))
             (mb (and src (zhihu--img-bytes-and-mime src base-dir))))
        (cond
         ((null mb)
          (cl-incf skipped))
         (t
          (let* ((mime (car mb))
                 (bytes (cdr mb))
                 (hash (zhihu--bytes-md5 bytes))
                 (cached-url (gethash hash cache))
                 (url (or (and cached-url (cl-incf cached) cached-url)
                          (let ((u (zhihu--upload-bytes bytes mime source)))
                            (cl-incf uploaded)
                            (puthash hash u cache)
                            u))))
            ;; 直接改 dom 节点的 attribute alist
            (let ((attrs (dom-attributes img)))
              (let ((cell (assq 'src attrs)))
                (if cell (setcdr cell url)
                  (push (cons 'src url) (cdr img))))))))))
    (let ((body (car (dom-by-tag dom 'body))))
      (message "zhihu: 图片 %d 上传 / %d 缓存命中 / %d 跳过"
               uploaded cached skipped)
      (cons (zhihu--inner-html body) cache))))

;;;; Publishing

;; Zhihu API
;;
;; 端点列表（参见 zhihu.nvim 的 lua/zhihu/api/）：
;;   POST  https://zhuanlan.zhihu.com/api/articles/drafts   (新文章草稿)
;;   PATCH https://zhuanlan.zhihu.com/api/articles/AID/draft(更新文章草稿)
;;   POST  https://www.zhihu.com/api/v4/content/publish     (发布)
;;   POST  https://www.zhihu.com/api/v4/columns/CID/items   (收录进专栏)

(defun zhihu--response-error-message (json &optional body)
  "从知乎响应 JSON 或原始 BODY 中提取错误信息。"
  (let ((nested (and json (plist-get json :error))))
    (or (and nested (plist-get nested :message))
        (and json (plist-get json :message))
        (and (stringp body)
             (string-empty-p (string-trim body))
             "服务端返回空响应")
        "未知错误")))

(defun zhihu--successful-status-p (status)
  "STATUS 是否是 HTTP 2xx 成功状态码。"
  (and (integerp status) (<= 200 status) (< status 300)))

(defun zhihu--response-request-id (resp)
  "从 RESP 中取得知乎请求 ID，便于追查空响应。"
  (cdr (assoc-string "zhi-request-id" (plist-get resp :headers) t)))

(defun zhihu--create-article-draft (title html)
  "创建标题为 TITLE、正文为 HTML 的文章草稿，并返回 article ID。"
  (let* ((url "https://zhuanlan.zhihu.com/api/articles/drafts")
         (body `(:title ,title
			:content ,html
			:delta_time ,(alist-get 'delta_time zhihu-publish-defaults)
			:can_reward
			,(alist-get 'can_reward zhihu-publish-defaults)))
         (resp (let ((zhihu--sign-http-json nil))
                 (zhihu--http-json
                  "POST" url body
                  (zhihu--mutation-headers
                   "https://zhuanlan.zhihu.com/write"
                   "https://zhuanlan.zhihu.com"))))
         (json (plist-get resp :json))
         (article-id
          (or (zhihu--value-string (and json (plist-get json :id)))
              (zhihu--value-string
               (and json (plist-get (plist-get json :draft) :id))))))
    (unless (zhihu--successful-status-p (plist-get resp :status))
      (error "zhihu: 创建文章草稿失败 (%s)：%s"
             (plist-get resp :status)
             (zhihu--response-error-message json (plist-get resp :body))))
    (when (plist-get json :error)
      (error "zhihu: 创建文章草稿失败：%s"
             (zhihu--response-error-message json)))
    (unless article-id
      (error "zhihu: 创建文章草稿后未拿到 article-id"))
    article-id))

(defun zhihu--article-in-column-p (article-id column-id)
  "查询 ARTICLE-ID，并判断它是否已收录于 COLUMN-ID。"
  (let* ((article-id (zhihu--value-string article-id))
         (column-id (zhihu--value-string column-id))
         (url (format "https://www.zhihu.com/api/v4/articles/%s"
                      (url-hexify-string article-id)))
         (resp (zhihu--http-json "GET" url))
         (status (plist-get resp :status))
         (json (plist-get resp :json)))
    (unless (eq status 200)
      (error "zhihu: 查询文章专栏失败 (%s)：%s"
             status
             (zhihu--response-error-message json (plist-get resp :body))))
    (unless (listp json)
      (error "zhihu: 查询文章专栏失败：服务端没有返回 JSON object"))
    (unless (equal (zhihu--value-string (plist-get json :id)) article-id)
      (error "zhihu: 查询文章专栏失败：响应中的文章 ID 不匹配"))
    (let ((column (plist-get json :column)))
      (and (listp column)
           (equal (zhihu--value-string (plist-get column :id))
                  column-id)))))

(defun zhihu--add-article-to-column (column-id article-id)
  "把 ARTICLE-ID 对应的已发布文章收录进 COLUMN-ID。"
  (let* ((url (format "https://www.zhihu.com/api/v4/columns/%s/items"
                      (url-hexify-string column-id)))
         (body `(:type "article" :id ,article-id))
         (resp (let ((zhihu--sign-http-json nil))
                 (zhihu--http-json
                  "POST" url body
                  (zhihu--mutation-headers
                   (format "https://zhuanlan.zhihu.com/p/%s/edit" article-id)
                   "https://zhuanlan.zhihu.com"))))
         (status (plist-get resp :status))
         (json (plist-get resp :json)))
    (unless (zhihu--successful-status-p status)
      (error "zhihu: 收录进专栏 %s 失败 (%s)：%s"
             column-id status
             (zhihu--response-error-message json (plist-get resp :body))))
    (when (and (listp json) (plist-get json :error))
      (error "zhihu: 收录进专栏 %s 失败：%s"
             column-id (zhihu--response-error-message json)))
    resp))

(defun zhihu--patch-article-draft (article-id title html)
  "PATCH 文章草稿。"
  (let* ((url (format "https://zhuanlan.zhihu.com/api/articles/%s/draft" article-id))
         (referer (format "https://zhuanlan.zhihu.com/p/%s/edit" article-id))
         (body `(:title ,title
			:content ,html
			:delta_time ,(alist-get 'delta_time zhihu-publish-defaults)
			:table_of_contents
			,(alist-get 'table_of_contents zhihu-publish-defaults)
			:can_reward
			,(alist-get 'can_reward zhihu-publish-defaults)))
         (resp (let ((zhihu--sign-http-json nil))
                 (zhihu--http-json
                  "PATCH" url body
                  (zhihu--mutation-headers
                   referer "https://zhuanlan.zhihu.com")))))
    (unless (zhihu--successful-status-p (plist-get resp :status))
      (error "zhihu: 存文章草稿失败 (%s) %s"
             (plist-get resp :status)
             (zhihu--response-error-message (plist-get resp :json))))
    resp))

;; 发布端点的 body 与当前知乎 web bundle 的 answer builder 逐字段
;; 对齐，并用当前 zhihu-cli 交叉验证。include 是前端要求后端
;; 一起返回的字段列表。

(defconst zhihu--publish-include-string
  "is_contain_ai_content,is_visible,paid_info,paid_info_content,has_column,admin_closed_comment,reward_info,annotation_action,annotation_detail,collapse_reason,is_normal,is_sticky,collapsed_by,suggest_edit,comment_count,thanks_count,favlists_count,can_comment,content,editable_content,voteup_count,reshipment_settings,comment_permission,created_time,updated_time,review_info,relevant_info,question,excerpt,attachment,content_source,is_labeled,endorsements,reaction_instruction,reaction,ip_info,relationship.is_authorized,voting,is_thanked,is_author,is_nothelp,is_favorited;author.vip_info,kvip_info,badge[*].topics;settings.table_of_content.enabled")

(defconst zhihu--publish-pc-business-params
  ;; 这一坨被 JSON.stringify 后塞进 extra_info.pc_business_params 字符串字段里
  '(:reward_setting (:can_reward :json-false :tagline "")
		    :reshipment_settings "allowed"
		    :thank_inviter ""
		    :comment_permission "all"
		    :commercial_zhitask_bind_info :json-null
		    :is_report :json-false
		    :push_activity :json-false
		    :thank_inviter_status "close"
		    :table_of_contents_enabled :json-false
		    :disclaimer_status "close"
		    :disclaimer_type "none"
		    :commercial_report_info (:is_report :json-false)))

(defun zhihu--gen-trace-id ()
  "生成 traceId：\"<epoch-ms>,<uuid-v4>\"。"
  (format "%d,%08x-%04x-4%03x-%x%03x-%012x"
          (floor (* 1000 (float-time)))
          (random #x100000000) (random #x10000)
          (random #x1000)
          (logior #b1000 (random 4)) (random #x1000)
          (random #x1000000000000)))

(defun zhihu--publish-body (kind item-id question-id html is-published)
  "构造发布请求的 JSON body（plist 形式）。
KIND 取 `answer' 或 `article'。HTML 是知乎正文 HTML。
新回答的 ITEM-ID 为 nil，此时必须完全省略 draft.contentId。"
  (when (and (eq kind 'article) (null item-id))
    (error "zhihu: 发布文章需要 article-id"))
  (let* ((draft `(:disabled 1
			    :isPublished ,(if is-published t :json-false)
			    ,@(when item-id
				(list (if (eq kind 'answer) :contentId :id) item-id))))
         (extra `(:publisher "pc"
			     :include ,zhihu--publish-include-string
			     :pc_business_params
			     ,(json-serialize zhihu--publish-pc-business-params
					      :null-object :json-null
					      :false-object :json-false)
			     ,@(when question-id (list :question_id question-id))))
         (data `(:hybridInfo ,(make-hash-table :test 'equal)
			     :toFollower ,(make-hash-table :test 'equal)
			     :publish (:traceId ,(zhihu--gen-trace-id))
			     :extra_info ,extra
			     :draft ,draft
			     :reprint (:reshipment_settings
				       ,(alist-get 'reshipment_settings zhihu-publish-defaults))
			     :publishSwitch (:draft_type
					     ,(alist-get 'draft_type zhihu-publish-defaults))
			     :creationStatement
			     (:disclaimer_type
			      ,(alist-get 'disclaimer_type zhihu-publish-defaults)
			      :disclaimer_status
			      ,(alist-get 'disclaimer_status zhihu-publish-defaults))
			     :contentsTables (:table_of_contents_enabled :json-false)
			     :commercialReportInfo (:isReport 0)
			     :thanksInvitation
			     (:thank_inviter_status
			      ,(alist-get 'thank_inviter_status zhihu-publish-defaults)
			      :thank_inviter
			      ,(alist-get 'thank_inviter zhihu-publish-defaults))
			     :commentsPermission
			     (:comment_permission
			      ,(alist-get 'comment_permission zhihu-publish-defaults))
			     :appreciate
			     (:can_reward ,(alist-get 'can_reward zhihu-publish-defaults)
					  :tagline ""))))
    (when (eq kind 'answer)
      (setq data
            (plist-put data :hybrid
                       `(:html ,html))))
    `(:action ,(if (eq kind 'answer) "answer" "article")
	      :data ,data)))

(defun zhihu--publish (kind item-id question-id html is-published)
  "POST /api/v4/content/publish，并严格验证返回的 publish 对象。
ITEM-ID 为 nil 表示创建新回答；返回服务端确认的内容 ID。"
  (let* ((url "https://www.zhihu.com/api/v4/content/publish")
         (body (zhihu--publish-body kind item-id question-id html is-published))
         ;; 浏览器端的 /content/publish 请求不带 ZSE；签名是
         ;; zhihu-cli 的传输层行为，混入这里会得到 HTTP 200 空响应而不发布。
         (resp (let ((zhihu--sign-http-json nil))
                 (zhihu--http-json
                  "POST" url body nil)))
         (status (plist-get resp :status))
         (response-body (plist-get resp :body))
         (json (plist-get resp :json)))
    (when (and (eq status 403)
               (stringp response-body)
               (string-match-p
                "\\\"need_login\\\"[[:space:]]*:[[:space:]]*true"
                response-body))
      (user-error
       "zhihu: Firefox 中的知乎登录状态不可用；请在浏览器登录后重试"))
    (unless (eq status 200)
      (let ((request-id (zhihu--response-request-id resp)))
        (error "zhihu: 发布 HTTP 失败 (%s)：%s%s"
               status
               (zhihu--response-error-message json response-body)
               (if request-id
                   (format "（zhi-request-id: %s）" request-id)
                 ""))))
    (unless json
      (error "zhihu: 发布失败：HTTP 200，但服务端没有返回 JSON"))
    (let* ((code (plist-get json :code))
           (message-text (plist-get json :message))
           (result-value (plist-get (plist-get json :data) :result))
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
           (published (and result (plist-get result :publish)))
           (published-id
            (and published (zhihu--value-string (plist-get published :id)))))
      (unless (and (eq code 0) (equal message-text "success"))
        (error "zhihu: 发布业务失败 (%s)：%s"
               (or code "无错误码")
               (zhihu--response-error-message json)))
      (unless published
        (error "zhihu: 发布响应缺少 data.result.publish，不能确认已经更新"))
      (unless published-id
        (error "zhihu: 发布响应缺少 data.result.publish.id"))
      (when (and item-id
                 (not (equal published-id (zhihu--value-string item-id))))
        (error "zhihu: 发布响应的内容 ID 不匹配（期望 %s，收到 %s）"
               item-id (or published-id "nil")))
      published-id)))

;; Source creation

(defun zhihu--question-title (question-id)
  "从知乎读取 QUESTION-ID 对应的非空问题标题。"
  (let ((question-id (zhihu--value-string question-id)))
    (unless (and question-id
                 (string-match-p "\\`[0-9]+\\'" question-id))
      (error "zhihu: 无效的 question-id：%s" question-id))
    (let* ((url (format "https://www.zhihu.com/api/v4/questions/%s"
                        (url-hexify-string question-id)))
           (resp (zhihu--http-json "GET" url))
           (status (plist-get resp :status))
           (json (plist-get resp :json)))
      (unless (zhihu--successful-status-p status)
        (error "zhihu: 获取问题 %s 标题失败 (HTTP %s)：%s"
               question-id status
               (zhihu--response-error-message json (plist-get resp :body))))
      (unless json
        (error "zhihu: 获取问题 %s 标题失败：服务端没有返回 JSON"
               question-id))
      (when (plist-get json :error)
        (error "zhihu: 获取问题 %s 标题失败：%s"
               question-id (zhihu--response-error-message json)))
      (let ((title (plist-get json :title)))
        (unless (stringp title)
          (error "zhihu: 问题 %s 的响应缺少标题" question-id))
        (setq title (string-trim title))
        (when (or (string-empty-p title)
                  (string-match-p "[[:cntrl:]]" title))
          (error "zhihu: 问题 %s 返回了无效标题" question-id))
        title))))

(defun zhihu--source-target-window ()
  "返回当前 frame 中适合显示源稿的主编辑窗口，找不到时返回 nil。"
  (cl-labels
      ((usable-p
        (window)
        (and (window-live-p window)
             (not (window-minibuffer-p window))
             (not (window-parameter window 'window-side))
             (not (window-parameter window 'no-other-window))
             (not (window-dedicated-p window)))))
    (let ((selected (selected-window)))
      (or (and (usable-p selected) selected)
          (cl-find-if #'usable-p
                      (window-list (selected-frame) 'nomini))))))

(defun zhihu--visit-source-file (file)
  "在当前 frame 的主编辑窗口访问源稿 FILE，并返回其 buffer。"
  (let ((buffer (find-file-noselect file))
        (window (zhihu--source-target-window)))
    (if (window-live-p window)
        (progn
          (select-window window)
          ;; 用户可能全局启用了 `switch-to-buffer-obey-display-actions'；
          ;; 源稿已经明确选好主窗口，不能再被 display action 分流或拆窗。
          (let ((switch-to-buffer-obey-display-actions nil))
            (switch-to-buffer buffer)))
      (pop-to-buffer buffer))
    buffer))

(defun zhihu--current-source-file ()
  "返回当前源文件，并拒绝让磁盘发布覆盖未保存编辑。"
  (let* ((file (or buffer-file-name
                   (user-error "zhihu: 当前 buffer 没绑文件")))
         (visiting (find-buffer-visiting (expand-file-name file))))
    (when (and visiting (buffer-modified-p visiting))
      (user-error "zhihu: 当前源文件有未保存修改；请先保存，再执行发布/写 metadata"))
    file))

(defun zhihu--refresh-file-buffer (file)
  "FILE 被安全写回后，刷新其未修改的 visiting buffer。"
  (when-let ((buf (find-buffer-visiting (expand-file-name file))))
    (with-current-buffer buf
      (unless (buffer-modified-p)
        (revert-buffer t t t)))))

(defun zhihu--new-source-spec (file)
  "校验新源稿 FILE，返回 (绝对路径 格式 标题)。
格式只由扩展名决定；标题取最后一个扩展名前的文件名。"
  (unless (and (stringp file) (not (string-empty-p (string-trim file))))
    (user-error "zhihu: 源稿文件名不能为空"))
  (let* ((file (expand-file-name file))
         (format (zhihu--file-format file))
         (title (string-trim
                 (file-name-sans-extension
                  (file-name-nondirectory file))))
         (parent (file-name-directory file)))
    ;; 相对路径可能从远程 `default-directory' 展开成 TRAMP 路径，因此必须
    ;; 检查归一化后的绝对路径。
    (when (file-remote-p file)
      (user-error "zhihu: 新建源稿不支持远程路径"))
    (when (file-directory-p file)
      (user-error "zhihu: 源稿目标不能是目录：%s" file))
    (unless format
      (user-error
       "zhihu: 源稿文件必须使用 .typ、.md、.markdown 或 .org 扩展名"))
    (when (or (string-empty-p title)
              (member title '("." ".."))
              (string-match-p "[[:cntrl:]]" title))
      (user-error "zhihu: 文件名不能生成有效标题：%s"
                  (file-name-nondirectory file)))
    (unless (file-directory-p parent)
      (user-error "zhihu: 父目录不存在：%s" parent))
    (list file format title)))

(defun zhihu--new-source-content
    (format title question-id answer-id article-id column-id)
  "生成 FORMAT 源稿的初始内容。
TITLE 是要写入源稿的文档标题；其余参数是知乎对象字段。"
  (let ((cache (make-hash-table :test 'equal)))
    (pcase format
      ('typst
       (concat
        (zhihu--format-typst-zhihu-metadata
         question-id answer-id article-id column-id cache)
        "\n"
        (format "#set document(title: %S)\n\n" title)))
      ('markdown
       (concat
        "---\n"
        (format "title: %s\n" (json-serialize title))
        (zhihu--format-zhihu-yaml
         question-id answer-id article-id column-id cache)
        "---\n\n"))
      ('org
       (concat
        (format "#+TITLE: %s\n" title)
        (zhihu--format-org-zhihu-metadata
         question-id answer-id article-id column-id cache)
        "\n"))
      (_ (error "zhihu: 不支持的源稿格式 %s" format)))))

(defun zhihu--open-matching-source-or-error
    (file kind &optional question-id answer-id article-id column-id)
  "打开与给定知乎对象相同的 FILE；同名但对象不同则报错。"
  (let* ((meta (zhihu--read-zhihu-meta file))
         (matches
          (pcase kind
            ('answer
             (and (eq (plist-get meta :kind) 'answer)
                  (equal (plist-get meta :question-id) question-id)
                  (or (null answer-id)
                      (equal (plist-get meta :answer-id) answer-id))))
            ('article
             (and (eq (plist-get meta :kind) 'article)
                  (equal (plist-get meta :article-id) article-id)
                  (equal (plist-get meta :column-id) column-id))))))
    (unless matches
      (user-error "zhihu: 同名源稿已存在但指向另一个知乎对象：%s"
                  (file-name-nondirectory file)))
    (zhihu--visit-source-file file)
    (message "zhihu: 源稿已存在，未覆盖 %s" file)))

(defun zhihu--create-source-file
    (file kind &optional question-id answer-id article-id column-id)
  "创建 FILE 作为 KIND 源稿，或打开指向同一知乎对象的已有文件。"
  (pcase-let ((`(,file ,format ,title) (zhihu--new-source-spec file)))
    (cond
     ((and (file-symlink-p file) (not (file-exists-p file)))
      (user-error "zhihu: 目标是失效的符号链接，拒绝覆盖：%s" file))
     ((file-exists-p file)
      (unless (file-regular-p file)
        (user-error "zhihu: 目标不是普通文件：%s" file))
     (zhihu--open-matching-source-or-error
       file kind question-id answer-id article-id column-id))
     (t
      (unless (file-writable-p (file-name-directory file))
        (user-error "zhihu: 父目录不可写：%s" (file-name-directory file)))
      (let ((source-title
             (if (eq kind 'answer)
                 (zhihu--question-title question-id)
               title)))
        (with-temp-buffer
          (insert (zhihu--new-source-content
                   format source-title question-id answer-id
                   article-id column-id))
          ;; 即使检查后发生竞争，也不会覆盖刚被其它进程创建的文件。
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region (point-min) (point-max) file nil 'silent nil 'excl))))
      (zhihu--visit-source-file file)
      (message "zhihu: 已创建 %s" file)))))

;;;###autoload
(defun zhihu-new-answer (question file)
  "创建知乎回答源稿 FILE。
QUESTION 可以是问题 ID 或完整问题/回答 URL。FILE 的扩展名决定使用
Typst、Markdown 或 Org；对应问题的标题会成为初始文档标题。"
  (interactive
   (list (read-string "知乎问题 ID 或 URL: ")
         (read-file-name "源稿文件（.typ/.md/.markdown/.org）: "
                         default-directory nil nil)))
  (let* ((parsed (zhihu--parse-id-or-url question))
         (qid (plist-get parsed :question-id))
         (aid (plist-get parsed :answer-id)))
    (zhihu--create-source-file file 'answer qid aid)))

;;;###autoload
(defun zhihu-new-article (column file)
  "创建知乎文章源稿 FILE，并可选择发布后收录到 COLUMN。
COLUMN 是专栏 ID/URL，也可以是 nil 或空字符串，表示直接发布独立文章。
FILE 的扩展名决定使用 Typst、Markdown 或 Org，文件名主体作为初始文档
标题。首次发布后会把 article-id 写回源稿。"
  (interactive
   (list (read-string "知乎专栏 ID 或 URL（可留空）: ")
         (read-file-name "源稿文件（.typ/.md/.markdown/.org）: "
                         default-directory nil nil)))
  (let ((column (and column (string-trim column))))
    (zhihu--create-source-file
     file 'article nil nil nil
     (unless (string-empty-p (or column ""))
       (zhihu--parse-column-id column)))))

;; Publishing workflows

(defun zhihu--publish-answer-file (file fmt meta)
  "把格式为 FMT 的 FILE/META 作为知乎答案发布。
自动：编译 → 扫图 → 上传/查 cache → 重写 src → POST publish → 回写 metadata。"
  (let* ((qid (or (plist-get meta :question-id)
                  (user-error
                   "zhihu: question-id 为空；请在 metadata 中添加 question-id")))
         (aid (plist-get meta :answer-id))
         (cache (plist-get meta :image-cache))
         (base-dir (file-name-directory (expand-file-name file)))
         (raw-html (progn (message "zhihu: 编译 %s..." fmt)
                          (zhihu--source-to-html file)))
         (rewritten (zhihu--rewrite-img-srcs raw-html base-dir cache))
         (html (car rewritten))
         (new-cache (cdr rewritten))
         (newly-created (null aid)))
    ;; 回答由 /content/publish 直接创建或更新，不使用草稿恢复状态。
    (setq meta (plist-put (copy-sequence meta) :draft nil))
    ;; 图片上传成功就先持久化 cache；后续网络失败也不会丢掉上传状态。
    (zhihu--write-zhihu-meta file qid aid nil nil new-cache meta)
    (zhihu--refresh-file-buffer file)
    (message "zhihu: 发布中...")
    (setq aid (zhihu--publish 'answer aid qid html (not newly-created)))
    (zhihu--write-zhihu-meta file qid aid nil nil new-cache meta)
    (zhihu--refresh-file-buffer file)
    (message "zhihu: 已发布 question/%s/answer/%s" qid aid)))

(defun zhihu--publish-article-file (file fmt meta)
  "把格式为 FMT 的 FILE/META 发布为知乎文章。
已有 article-id 时更新文章；否则创建文章。column-id 非空时，发布后检查
文章当前专栏，尚未收录于目标专栏时再发起收录。
Typst 标题取 `#set document(title: ...)'，Markdown/Org 取各自标题字段。
自动：编译 → 扫图上传/查 cache → 重写 src → patch 草稿 → publish
  → 回写 image-cache。"
  (let* ((aid (plist-get meta :article-id))
         (column-id (plist-get meta :column-id))
         (newly-created (null aid))
         (draft-p (or newly-created (plist-get meta :draft)))
         (cache (plist-get meta :image-cache))
         (base-dir (file-name-directory (expand-file-name file)))
         (compiled-html
          (when (eq fmt 'typst)
            (message "zhihu: 编译 %s..." fmt)
            (zhihu--typst-compile-html file)))
         (title
          (or (if compiled-html
                  (zhihu--html-document-title compiled-html)
                (plist-get meta :title))
              (user-error
               (if compiled-html
                   "zhihu: 缺少 #set document(title: ...)，无法作为文章发布"
                 "zhihu: Markdown/Org 缺少标题，无法作为文章发布"))))
         (raw-html
          (if compiled-html
              (zhihu--normalize-html compiled-html)
            (message "zhihu: 编译 %s..." fmt)
            (zhihu--source-to-html file)))
         (rewritten (zhihu--rewrite-img-srcs raw-html base-dir cache "article"))
         (html (car rewritten))
         (new-cache (cdr rewritten)))
    ;; 图片 cache 独立于发布结果，上传完成后立即安全写回。
    (zhihu--write-zhihu-meta
     file nil nil aid column-id new-cache meta)
    (zhihu--refresh-file-buffer file)
    (when newly-created
      (message "zhihu: 创建文章草稿...")
      (setq aid (zhihu--create-article-draft title html))
      ;; 草稿 ID 立即写盘；后续失败时不会重复创建文章。
      (setq meta (plist-put (copy-sequence meta) :draft t))
      (zhihu--write-zhihu-meta
       file nil nil aid column-id new-cache meta)
      (zhihu--refresh-file-buffer file))
    (message "zhihu: patch 文章草稿...")
    (zhihu--patch-article-draft aid title html)
    (message "zhihu: 发布中...")
    (zhihu--publish 'article aid nil html (not draft-p))
    ;; 发布已经得到服务端确认。先清除 draft 状态再检查并收录专栏；
    ;; 即使收录失败，下次也会按已发布文章更新，而不是误当草稿。
    (setq meta (plist-put (copy-sequence meta) :draft nil))
    (zhihu--write-zhihu-meta
     file nil nil aid column-id new-cache meta)
    (zhihu--refresh-file-buffer file)
    (when column-id
      (let ((already-in-column
             (condition-case err
                 (zhihu--article-in-column-p aid column-id)
               (error
                (error "zhihu: 文章已发布为 p/%s，但查询专栏 %s 失败：%s；\
column-id 已保留，再次发布即可重试"
                       aid column-id (error-message-string err))))))
        (if already-in-column
            (message "zhihu: p/%s 已收录于专栏 %s" aid column-id)
          (message "zhihu: 收录进专栏 %s..." column-id)
          (condition-case err
              (zhihu--add-article-to-column column-id aid)
            (error
             (error "zhihu: 文章已发布为 p/%s，但收录进专栏 %s 失败：%s；\
column-id 已保留，再次发布即可重试"
                    aid column-id (error-message-string err)))))))
    (message "zhihu: 已发布 p/%s" aid)))

;; Interactive entry points

;;;###autoload
(defun zhihu-publish ()
  "保存当前知乎源稿，并按 metadata 自动发布回答或文章。"
  (interactive)
  (message "zhihu: 开始发布...")
  (redisplay)
  (unless buffer-file-name
    (user-error "zhihu: 当前 buffer 没有对应的源稿文件"))
  (when (buffer-modified-p)
    (save-buffer))
  (let* ((file (zhihu--current-source-file))
         (fmt (or (zhihu--file-format file)
                  (user-error "zhihu: 不支持的文件类型 %s" file)))
         (meta (or (zhihu--read-zhihu-meta file)
                   (user-error "zhihu: %s 缺少知乎 metadata"
                               (file-name-nondirectory file)))))
    (pcase (plist-get meta :kind)
      ('answer (zhihu--publish-answer-file file fmt meta))
      ('article (zhihu--publish-article-file file fmt meta))
      (_ (user-error "zhihu: metadata 没有指定回答或文章")))))

;;;; User interface

;; Source minor mode

(defvar zhihu-source-mode-map
  (make-sparse-keymap)
  "Keymap for persistent Zhihu source buffers.")

;;;###autoload
(define-minor-mode zhihu-source-mode
  "Minor mode for persistent Zhihu source files.
调用 `zhihu-publish' 保存待处理编辑并发布；本包不预设键位。"
  :lighter " ZhihuSrc"
  :keymap zhihu-source-mode-map
  ;; Keep the source commands ahead of unrelated minor-mode bindings in this
  ;; buffer; ordinary Typst buffers are unaffected.
  (setq-local
   minor-mode-overriding-map-alist
   (cl-remove 'zhihu-source-mode minor-mode-overriding-map-alist
              :key #'car :test #'eq))
  (when zhihu-source-mode
    (push (cons 'zhihu-source-mode zhihu-source-mode-map)
          minor-mode-overriding-map-alist)))

(defun zhihu--buffer-has-source-metadata-p ()
  "当前文件 buffer 是否含有本包支持的知乎 metadata 标记。"
  (when buffer-file-name
    (save-excursion
      (goto-char (point-min))
      (pcase (zhihu--file-format buffer-file-name)
        ('typst
         (condition-case nil
             (when-let ((region (zhihu--typst-native-metadata-region)))
               (save-restriction
                 (narrow-to-region (car region) (cdr region))
                 (goto-char (point-min))
                 (with-syntax-table zhihu--typst-metadata-syntax-table
                   (let (found)
                     (while (and (not found)
                                 (re-search-forward
                                  "\\_<\\(?:question-id\\|article-id\\)\\_>[ \\t]*:"
                                  nil t))
                       (unless (nth 8 (syntax-ppss (match-beginning 0)))
                         (setq found t)))
                     found))))
           (error nil)))
        ('markdown
         (let ((frontmatter
                (car (zhihu--md-split-frontmatter
                      (buffer-substring-no-properties
                       (point-min) (point-max))))))
           (condition-case nil
               (and (plist-get
                     (zhihu--md-frontmatter-zhihu-meta frontmatter) :kind)
                    t)
             (error nil))))
        ('org
         (let ((keywords (zhihu--org-collect-keywords)))
           (cl-some (lambda (key) (assoc-string key keywords t))
                    '("ZHIHU_QUESTION_ID"
                      "ZHIHU_ARTICLE_ID"))))
        (_ nil)))))

;;;###autoload
(defun zhihu-auto-enable-source-mode ()
  "当当前文件包含知乎 metadata 时启用 `zhihu-source-mode'。"
  (when (zhihu--buffer-has-source-metadata-p)
    (zhihu-source-mode 1)))

;;;###autoload
(add-hook 'find-file-hook #'zhihu-auto-enable-source-mode)

(provide 'zhihu)
;;; zhihu.el ends here
