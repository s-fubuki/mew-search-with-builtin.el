;;; multipart-decode.el -- Multipart Decode.
;; Copyright (C) 2018, 2021 fubuki@frill.org
;; @(#)$Revision: 1.6 $$Name:  $

;; Author: fubuki@frill.org
;; Keywords: tools

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; When executed in the mail buffer, the buffer is decoded into human readable form.
;; Wherever the cursor at the start is located,
;; it covers from the beginning to the end of the buffer.

;;; Installation:

;; (require 'mew-search-with-builtin)

;;; Code:
(require 'rx)
;; (require 'mm-uu)

(defgroup multipart-decode nil
  "Multipart decode."
  :group 'mew
  :version "28.0.50"
  :prefix "mpd-")

(defcustom mpd-multipart-regexp
  (rx bol "Content-Type: "
      (group-n 1 "multipart/" (or "alternative" "mixed" "related" "report"))
      ";"
      (*? anychar) "boundary=" (? (syntax string-quote))
      (group-n 2 (+? print)) (? (syntax string-quote)) eol)
  "Content-Type Multipart header."
  :type  'regexp
  :group 'multipart-decode)

(defcustom mpd-text-regexp
  (rx bol "Content-Type: "
      (group-n 1 (or "text" "message") "/" (or "plain" "html" "rfc822"))
      (? (and ";"
              (*? space) "charset="
              (? (syntax string-quote))
              (group-n 2 (+ (not (any "\"\n;"))))
              (? (syntax string-quote)))))
  "Content-Type Type header."
  :type  'regexp
  :group 'multipart-decode)

(defcustom  mpd-enco-regexp
  "^Content-Transfer-Encoding: \\(?1:.+\\)$"
  "Content-Type Concoding type header."
  :type  'regexp
  :group 'multipart-decode)

(defcustom mpd-decode-function-alist
  '(("\\<base64\\|B\\>"           . base64-decode-string)
    ("\\<quoted-printable\\|Q\\>" . quoted-printable-decode-string-silence))
  "Decode function Symbol alist."
  :type  '(repeat (cons string function))
  :group 'multipart-decode)

(defcustom mpd-regular-alist
  '((shift-jis       . shift_jis)
    (ascii           . iso-8859-1)
    (cp-850          . cp437)
    (cp-1252         . iso-8859-1)
    (134             . gb2312)
    (ansi_x3\.4-1968 . utf-8))
  "ほぼ spam にしか現われないので設定しなくても問題ない."
  :type  '(repeat (cons (choice symbol number) coding-system))
  :group 'multipart-decode)

;; `mpd-vec-sym' のベクタ出し入れ関数生成.
(eval-and-compile
  (defvar mpd-vec-sym '(type boundary char enco beg end))
  (let ((i 0))
    (dolist (a mpd-vec-sym)
      (let* ((sym    (intern (concat "mpd-type-" (symbol-name a))))
             (setsym (intern (concat "mpd-type-set-" (symbol-name a)))))
        (eval `(defmacro ,sym (vec) (list 'elt vec ,i)) t)
        (eval `(defmacro ,setsym (vec val) (list 'aset vec ,i val)) t)
        (setq i (1+ i))))))

(defvar mpd-header-field-regexp
  (rx bol (or "From: " "To: " "Subject: " "Cc: " "Reply-To: " "Return-Receipt-To: ")))

(defsubst quoted-printable-decode-string-silence (string &optional coding)
  "\"Malformed quoted-printable text\" のエコーエリア出力を抑止. *Messages* には残る."
  (let ((inhibit-message t))
    (quoted-printable-decode-string string coding)))

;;;###autoload
(defun multipart-decode (&optional buffer)
  "BUFFER の encode 部分等をインライン展開する.
BUFFER は壊れるので保持したい場合コピーを使う等する.
シングルパート/マルチパート の base64 quoted-printable のプレーン text と html に対応.
zip jpg pdf 等のバイナリは誤マッチ防止のため残さないで削除します."
  (interactive)
  (let (boundary match term)
    (save-excursion
      (with-current-buffer (or buffer (current-buffer))
        (goto-char (point-min))
        (setq term (or (save-excursion (re-search-forward "^\n" nil t)) (point-max)))
        ;; (mail-decode-encoded-word-region (point) term)
        (mpd-header-field-decode)
        (setq boundary
              (and (re-search-forward mpd-multipart-regexp term t)
                   (match-string 2)))
        (cond
         (boundary
          (mpd-multipart-buffer-decode boundary))
         (t
          (mpd-single-buffer-decode mpd-text-regexp)))))))

(defsubst mpd-regular (string)
  "STRING を intern して symbol にして返し、数字文字列なら数値にして返す.
但し連想リスト `mpd-regular-alist' に置換するシンボルが設定されていればそれを返す."
  (let* ((string (or string "iso-8859-1"))
         (num (string-to-number string))
         (sym (intern (downcase string))))
    (or (cdr (assq (if (zerop num) sym num) mpd-regular-alist)) sym)))

(defun mpd-content-type ()
  "開始ポイントが \"boundary\" Search 直後に在ると想定し、
続く \"Content-Type: \" 行のパラメータをベクターにして返す.
対応しているのは変数 `mpd-multipart-regexp' `mpd-text-regexp' `mpd-enco-regexp'
に設定されたもの.
終了時のポイントは \"Content-Type: \" 行の後の別フィールドの行の行頭に在る."
  (let ((vec (make-vector (length mpd-vec-sym) nil)))
    (catch 'out
      (while (zerop (forward-line))
        (cond
         ((looking-at mpd-multipart-regexp)
          (mpd-type-set-type vec (match-string 1))
          (mpd-type-set-boundary vec (match-string 2))
          (goto-char (match-end 0)))
         ((looking-at mpd-text-regexp)
          (mpd-type-set-type vec (match-string 1))
          (mpd-type-set-char vec (mpd-regular (match-string 2)))
          (goto-char (match-end 0)))
         ((looking-at mpd-enco-regexp)
          (mpd-type-set-enco vec (downcase (match-string 1))) 
          (goto-char (match-end 0)))
         ((not (looking-at "^\n"))
          (end-of-line))
         (t
          (throw 'out nil)))))
    vec))

(defun mpd-header-field-decode (&optional regexp)
  "Message buffer ヘッダ内の REGEXP にマッチする箇所が\
エンコードされていれば当該コードにデコードし差し替える.
REGEXP が nil なら `mpd-header-field-regexp' で指定された正規表現が使われる.
最後に `buffer-modified-p' を実行するので
デコードが起きバッファが壊されていれば non-nil を返し、さもなくば nil を返す."
  (let ((regexp (or regexp mpd-header-field-regexp))
        term beg end string non-ascii field)
    (save-excursion
      (save-excursion
        (setq term (if (re-search-forward "^\n" nil t)
                       (point-marker)
                     (point-max-marker))))
      (while (re-search-forward regexp term t)
        (setq beg   (point)
              field (match-string 0))
        (catch 'out
          (while (< (point) term)
            (cond
             ((eolp)
              (forward-line)
              (if (looking-at "[^ \t]")
                  (throw 'out (setq string (concat string "\n")))))
             ((looking-at "[ \t]+")
              (when (and (not (bolp)) (not (string-equal field "Subject: ")))
                (setq string (concat string (match-string 0))))
              (goto-char (match-end 0)))
             ((looking-at
               "\"?=\\?\\(?1:.+?\\)\\?\\(?2:.\\)\\?\\(?3:.+?\\)\\?=\"?")
              (let* ((chr (mpd-regular (match-string 1)))
                     (enc (match-string 2))
                     (str (match-string 3))
                     (fun (assoc-default (upcase enc) mpd-decode-function-alist
                                         #'string-match-p)))
                (setq non-ascii t)
                (setq end  (match-end 0))
                (setq string
                      (concat
                       string (decode-coding-string (funcall fun str) chr)))
                (goto-char end)))
             ((looking-at "\\(?1:[^ \n]+\\)")
              (setq string (concat string (match-string 1)))
              (goto-char (match-end 0))))))
        (when non-ascii
          (delete-region beg (point))
          (insert string))
        (setq non-ascii nil
              string    nil))
      (buffer-modified-p))))

(defun mpd-multipart-buffer-decode (boundary)
  "`multipart-decode' の実体. 再帰でループする."
  (let (types beg)
    (while (search-forward boundary nil t) ; メタ文字が使われている場合があるので普通のサーチ
      (setq types (mpd-content-type))
      (and
       (or
        (mpd-type-type types)
        (mpd-type-char types)
        (mpd-type-boundary types)
        (mpd-type-enco types))
       (cond
        ((mpd-type-boundary types)
         (mpd-multipart-buffer-decode (mpd-type-boundary types)))
        ((or (string-match "rfc822" (or (mpd-type-type types) ""))
             (and (mpd-type-enco types)
                  (assoc-default
                   (mpd-type-enco types) mpd-decode-function-alist #'string-match-p)))
         (mpd-decode-body boundary types)))))))

(defun mpd-single-buffer-decode (regexp) ; mpd-text-regexp
  "非マルチパートの場合のヘッダの掴み."
  (let (type chr enc term)
    (save-excursion
      (setq term (re-search-forward "^\n" nil t)))
    (save-excursion
      (when (re-search-forward regexp term t)
        (setq type (match-string 1))
        (setq chr (mpd-regular (match-string 2))))
      (unless chr
        (goto-char (point-min))
        (when (re-search-forward mpd-text-regexp term t)
          (setq chr (mpd-regular (match-string 2)))))
      (goto-char (point-min)) ; 順不動なので仕切り直す
      (when (re-search-forward mpd-enco-regexp term t)
        (setq enc (match-string 1)))
      (when (and enc (assoc-default enc mpd-decode-function-alist #'string-match-p))
        ;; 次の関数の為にポントを本文に合わせておく
        (goto-char (1- term))
        (mpd-decode-body nil (vector type nil chr enc nil nil))))))

(defun mpd-decode-body (boundary types)
  "デコーダ本体.
現在ポイントの次の行から BOUNDARY の 1行上の行末までを
変数 `mpd-decode-function-alist' の中の ENCO に対応した関数で
CODE にデコードしたものと差替える.

BOUNDARY が NIL だとバッファエンドまでが対象となる.
但し base64 の場合はその後にテキストのあるケースがあった為連続改行までとする.
Decode の必要の無いプレーンテキスト等の場合差替は発生せずカーソルだけが移動する.

`types' の中にも `boundary' はあるが次の `boundary' や
NIL である可能性もあるので個別になっている."
  (let ((func (and
               (mpd-type-enco types)
               (assoc-default
                (mpd-type-enco types) mpd-decode-function-alist #'string-match-p)))
        string)
    (forward-line)
    (mpd-type-set-beg types (point))
    (cond
     ;; Single Part 切出しポイントゲット
     ((null boundary)
      (if (string-equal (mpd-type-enco types) "base64")
          (or (prog1 (search-forward "\n\n" nil t) (backward-char))
              (goto-char (point-max)))
        (goto-char (point-max))))
     ;; Multi Part 切出しポイントゲット
     ((search-forward boundary nil t)
      (end-of-line 0))
     (t
      (goto-char (point-max))))
    (mpd-type-set-end types (point))
    ;; 差替本体 仮に関数化する場合に備えて分離しやすくしてある
    (let ((beg (mpd-type-beg types))
          (end (mpd-type-end types)))
      (cond
       ((string-match "rfc822" (or (mpd-type-type types) ""))
        (let ((string (buffer-substring beg end)))
          (setq string
                (with-temp-buffer
                  (insert string)
                  (multipart-decode) ; 更に再帰
                  (buffer-string)))
          (delete-region beg end)
          (insert string)))
       ((not (string-match "text\\|html" (or (mpd-type-type types) "")))
        (delete-region beg end))
       (t
        (setq string
              (decode-coding-string
               (funcall func (buffer-substring beg end))
               (mpd-type-char types)))
        (delete-region beg end)
        (insert string))))))

(provide 'multipart-decode)
;; fin.
