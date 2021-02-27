;;; mew-search-with-builtin.el -- Mew Builtin Search.
;; Copyright (C) 2018, 2019, 2020, 2021 fubuki -*- coding: utf-8-emacs; -*-

;; Author: fubuki@frill.org
;; Version: $Revision: 1.5 $$Name:  $
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

;; SEE ALSO THE END OF THIS FILE FOR ADDITIONAL NOTES.

;;; Commentary:

;; Mew Builtin Search.

;;; Installation:

;; (require 'mew-search-with-builtin)

;;; Update:
;; Sat Jan 30 08:58:12 2021
;;  Change: 除外フォルダ指定変数名の変更.
;;    `mbs-ignore-directory-list' -> `mbs-ignore-directory'.
;;    list で指定できなくなったので注意.
;;  Change: 進捗状況を mode-line に表示するよう変更.
;;  Fix: Single Thread mode のテストと調整.
;;       使わないしバグの温床になるのでいずれ無くす可能性あり.
;;  Add: kill-emacs-hook でも後始末.
;;  multipart-decode: quoted-printable のワーニング表示を抑止.

;; Sat Jun 27 11:27:05 2020
;;  BG β版.

;; Wed May 27 15:21:39 2020
;;  Fix: Directory Search のパーミッションチェックを修正.
;;       (Thanks. Takashi Hirayama)
;;  Fix: `multipart-decode' も少し fix されているのでついでに替えておくのを推奨.

;; Tue May 21 17:04:56 2019
;;  Fix: mew-folder-local のルートにメールがあるとエラーになるのを修正
;;       `mbs-path-to-mew-folder'.

;;; Code:

(require 'mew)
(require 'qp)               ; quoted-printable liblary.
(require 'multipart-decode) ; base64 quoted-printable 部分をインライン展開

(defconst mew-search-with-builtin-version "$Revision: 1.5 $$Name:  $")

(defgroup mew-builtin-search nil
  "Mew Builtin Search."
  :group   'mew
  :version "26.1"
  :prefix  "mbs-")

;; `mew-search-method' が nil なら 'builtin に設定.
(and
 (boundp 'mew-search-method)
 (null mew-search-method)
 (setq mew-search-method 'builtin))

(defcustom mbs-builtin t
  "NIL なら `mew-prog-grep' が使われる."
  :type  'boolean
  :group 'mew-builtin-search)

(defcustom mbs-case-fold-search case-fold-search
  "Mew Builtin Search default case."
   :type  'boolean
   :group 'mew-builtin-search)

(defcustom mbs-match-function 'mbs-first-match
  "検索に使う 3つの引数を取る関数です.
マッチしたら非NIL を返し、しなければ NIL を返します.
\\\\(func regexp message-file case\\\\)"
  :type  'function
  :group 'mew-builtin-search)

(defcustom mbs-progress-threshold 400
  "サーチする Folder にこの数値以上のファイル数があれば経過割合を表示."
   :type  'integer
   :group 'mew-builtin-search)

(defcustom mbs-ignore-directory "\\(trash\\|draft\\|backup\\)" ;; queue
  "除外したいディレクトリの正規表現."
  :type  'regexp
  :group 'mew-builtin-search)

(make-obsolete-variable 'mbs-ignore-directory-list 'mbs-ignore-directory "1.4")

(defcustom mbs-thread-slice-time 0.0005
  "カレントループを休ませる時間.
要はスレッドのスライスタイム.
NIL でシングルスレッドモード."
   :type  '(choice number (const nil))
   :group 'mew-builtin-search)

(defcustom mbs-thread-timer 5
  "BG 進捗リスト `mbs-search-location-list' を更新するタイマーの秒間隔.
表示には mode-line が更新されて反映さるので、
これを短かくしても反映自体が早まるわけではない."
   :type  'number
   :group 'mew-builtin-search)

(defcustom mbs-thread-unswitch 'open
  "nil なら完了時にウィンドウを開いてカレントバッファにする.
t か silent なら完了時生成されたバッファを開かない.
open なら完了時にオープンはするがカレントバッファにはしない.
BG 実行しなくても有効なので注意."
  :type  '(choice (const nil)
                  (const t)
                  (const silent)
                  (const open))
  :group 'mew-builtin-search)

(defface mbs-mode-line
  '((t :inherit mode-line-inactive :weight bold))
  "mbs mode-line face."
  :group 'mew-builtin-search-faces)

(eval-and-compile
  ((lambda ()
     (let ((vec '(thread timer opattern pattern flds filter location point start))
           (i 0))
       (dolist (s vec)
         (fset (intern (concat "mbs-vector-" (symbol-name s)))
               `(lambda () ,i))
         (setq i (1+ i)))))))

(defmacro mbs-progress (all cnt)
  `(/ (* (- ,all ,cnt) 100) ,all))

(defmacro mbs-progress-message (all cnt msg folder)
  "MSG は %s %d をこの順序で持った文字列.
%s に FOLDER が入り %d に ALL(総数) CNT(残数) の計算結果が入る."
  `(let ((per (mbs-progress ,all ,cnt)))
     (and (null mbs-thread-slice-time)
          (< mbs-progress-threshold ,all)
          (zerop (% per 5))
          (let (message-log-max)
            (message ,msg ,folder per)))))

(defcustom mbs-mew-pick-field-list
  '("^to: .*" "^cc: .*" "^subject: .*" "^dcc: .*" "^fcc: .*" "^bcc: .*"
    "^date: .*" "^reply-to: .*" "^followup-to: .*" "^from: .*"
    "^newsgroups: .*" "^content-.*")
  "Mew の補完リスト `mew-pick-field-list' に付け足す補完リスト.
不要なら nil にしておく."
  :type  '(repeat string)
  :group 'mew-builtin-search)

(if mbs-mew-pick-field-list
    (setq mew-pick-field-list
          (append mew-pick-field-list mbs-mew-pick-field-list)))

;; 検索キーに SPACE や正規表現 `?' も使えるようにする措置.
;; デフォルトでは補完になっていて入力できないが補完は C-i 等でもできる.
(add-hook 'mew-init-hook
          #'(lambda ()
              (define-key mew-input-map " " nil)
              (define-key mew-input-map "?" nil)
              (define-key mew-input-map "\M-c"  'mbs-toggle-case-fold-search)))

(define-key-after
  (lookup-key mew-summary-mode-map [menu-bar Mew Select])
  [pic] '("Message Search" . mew-summary-selection-by-search))

;; Original function wrapper part.

(defun mew-summary-pick-with-grep-wrap (func prog opts pattern folder src-msgs)
  "for MARK. 変数 `mbs-builtin' が NON-NIL ならアクティブ."
  (if (null mbs-builtin)
      (funcall func prog opts pattern folder src-msgs)
    (mew-summary-pick-with-builtin pattern folder src-msgs)))

(defun mew-summary-selection-by-pick-with-grep1-wrap
    (func prog opts pattern folder msgs)
  "for SELECTION. 変数 `mbs-builtin' が NON-NIL ならアクティブ."
  (if (null mbs-builtin)
      (funcall func prog opts pattern folder msgs)
    (mew-summary-selection-by-pick-with-builtin pattern folder msgs)))

(defun mew-pick-canonicalize-pattern-wrap (func pattern)
  "字句解析をスキップさせるラッパー."
  (if (null mbs-builtin)
      (funcall func pattern)
    pattern))

(defun mbs-toggle-case-fold-search ()
  "Builtin search ignore case toggle."
  (interactive)
  (setq mbs-case-fold-search (not mbs-case-fold-search))
  (message "mbs-case-fold-search: %s" 
           (if mbs-case-fold-search "CASE INSENSITIVE." "Case sensitive.")))

;; (add-hook 'mew-summary-mode-hook
;;           #'(lambda ()
;;               (local-set-key "z\C-i" 'mbs-toggle-case-fold-search)
;;               (local-set-key "k\C-i" 'mbs-toggle-case-fold-search)))

;; Folder search part.

;; For MARK.
(defun mew-summary-pick-with-builtin (regexp folder src-msgs)
  "A function to pick messages matching REGEXP.
注意: SRC-MCGS は拡張子があるが返値 MSGS は拡張子があると動作しないので
この関数の中で拡張子を除去しないといけいない."
  (let* ((dir (mew-expand-folder folder))
	 (default-directory dir) ;; buffer local
         (case mbs-case-fold-search)
         (all (length src-msgs))
         (cnt all)
         msgs)
    (cd dir)
    (mbs-upush regexp)
    (dolist (message src-msgs)
      (let ((match (funcall mbs-match-function regexp message case)))
        (mbs-progress-message all cnt "Scan message %s...%d%%" folder)
        (if match
            (setq msgs (cons (file-name-sans-extension message) msgs))))
      (setq cnt (1- cnt)))
    (sort msgs #'(lambda (a b) (< (string-to-number a) (string-to-number b))))))

;; For SELECTION.
(defun mew-summary-selection-by-pick-with-builtin (regexp folder msgs)
  "FOLDER の MSGS から REGEXP をサーチしてセレクションを作る."
  (let* ((dir (mew-expand-folder folder))
	 (file (mew-make-temp-name))
	 (rttl 0)
         (case mbs-case-fold-search)
         (all (length msgs))
         (cnt all)
         result-msgs)
    (cd dir)
    (mbs-upush regexp 'clear) ; 仕切り直しなので履歴はクリア
    (dolist (message msgs)
      (let ((match (funcall mbs-match-function regexp message case)))
        (mbs-progress-message all cnt "Scan message %s...%d%%" folder)
        (if match
            (setq result-msgs
                  (cons (file-name-sans-extension message) result-msgs))))
      (setq cnt (1- cnt)))
    (setq result-msgs
          (sort result-msgs
                #'(lambda (a b) (< (string-to-number a) (string-to-number b)))))
    (with-temp-buffer
      (setq rttl (length result-msgs))
      (insert "CD: " folder "\n")
      (mapc (lambda (x) (insert (mew-msg-get-filename x) "\n")) result-msgs)
      (mew-frwlet mew-cs-text-for-read mew-cs-text-for-write
	(write-region (point-min) (point-max) file nil 'no-msg))
      (list file rttl))))

(defcustom mbs-pattern-tail-empty t
  "パターンの末尾が \"\\|\" ならエラーで停止させる.
そのまま実行すると総てのメッセージにマッチして
その数が多すぎるとフリーズしてしまうことがある."
  :type  'boolean
  :group 'mew-builtin-search)

;; Search Core.
(defun mbs-first-match (regexp file &optional case)
  "REGEXP にマッチする箇所が FILE 内にひとつでもあればそれを返す.
なければ NIL を返す.
Decode に失敗したファイルはエラーメッセージを表示しサーチはせず NIL を返す.
CASE が NON-NIL なら ASCII 大文字小文字を別のものとする."
  (and mbs-pattern-tail-empty (string-match "\\\\|\\'" regexp)
       (error "`%s' 空文字が含まれている可能性があります" regexp))
  (with-temp-buffer
    (let ((case-fold-search (or case mbs-case-fold-search))
          ;; ↓Mew 環境実行の場合これが無いとデコードで化けるケースが在り
          ;;   正しくマッチできない
          (coding-system-for-read nil)) 
      (insert-file-contents file)
      (goto-char (point-min))
      (if (condition-case err
              (multipart-decode)
            (error (message "%s: %s" file (error-message-string err))))
          nil
        (catch 'break
          (while (re-search-forward regexp nil t)
            (throw 'break (match-string 0))))))))

(advice-add 'mew-pick-canonicalize-pattern
            :around 'mew-pick-canonicalize-pattern-wrap)
(advice-add 'mew-summary-pick-with-grep
            :around 'mew-summary-pick-with-grep-wrap)
(advice-add 'mew-summary-selection-by-pick-with-grep1
            :around 'mew-summary-selection-by-pick-with-grep1-wrap)

;; Match Hilight part.

(defgroup mew-builtin-search-faces nil
  "Faces for mew-builtin-search."
  :group 'mew-builtin-search
  :group 'faces)

(defface mbs-match-1
  '((((background light))
     :foreground "black" :background "yellow" :weight bold)
    (((background dark))
     :foreground "black" :background "#ffffaa" :weight bold)
    (t :weight bold))
  "Mew builtin search matched highlight 1."
  :group 'mew-builtin-search-faces)
(make-obsolete-variable 'mbs-match-1 "削除されました" "1.4")

(defface mbs-match-2
  '((((background light))
     :foreground "black" :background "palegreen" :weight bold)
    (((background dark))
     :foreground "black" :background "#ddffdd" :weight bold)
    (t :weight bold))
  "Mew builtin search matched highlight 2."
  :group 'mew-builtin-search-faces)
(make-obsolete-variable 'mbs-match-2 "削除されました" "1.4")

(defface mbs-match-3
  '((((background light))
     :foreground "black" :background "lightskyblue" :weight bold)
    (((background dark))
     :foreground "black" :background "#cfdeee" :weight bold)
    (t :weight bold))
  "Mew builtin search matched highlight 3."
  :group 'mew-builtin-search-faces)
(make-obsolete-variable 'mbs-match-3 "削除されました" "1.4")

(defface mbs-match-4
  '((((background light))
    :foreground "black" :background "hotpink" :weight bold)
   (((background dark))
    :foreground "black" :background "#ffdddd" :weight bold)
   (t :weight bold))
  "Mew builtin search matched highlight 4."
    :group 'mew-builtin-search-faces)
(make-obsolete-variable 'mbs-match-4 "削除されました" "1.4")

(defcustom mbs-face-list '(mbs-match-1 mbs-match-2 mbs-match-3 mbs-match-4)
  "Mew Builtin Search Matched Highlight face cycle list."
  :type  '(repeat face)
  :group 'mew-builtin-search)

(defvar mbs-key        nil "Search word stack work.")
(defvar mbs-face-point nil "Work value.")
(defvar mbs-overlay    nil "Matched overlay stack work.")

(defun mbs-upush (key &optional clear)
  "`mbs-key' に KEY が無ければ push.
CLEAR が非NIL なら `mbs-key' をクリアして push."
  (cond
   (clear
    (setq mbs-key (cons (cons key (car mbs-face-list)) nil)))
   ((not (assoc key mbs-key))
    (setq mbs-key (cons (cons key (mbs-inc-face)) mbs-key))))
  mbs-key)

(defun mbs-inc-face ()
  "`mbs-face-point' をひとつ進めて car を返す.
NIL になると `mbs-face-list' がセットされる. "
  (car (setq mbs-face-point (or (cdr mbs-face-point) mbs-face-list))))

(defun mbs-match-highlight (&optional alist)
  "ALIST は (KEY . FACE) の alist.
buffer 内の KEY にマッチした文字列すべてを FACE でハイライト表示."
  (let ((lst (or alist mbs-key)))
    (save-excursion
      (dolist (key lst)
        (goto-char (point-min))
        (while (re-search-forward (car key) nil t)
          (setq mbs-overlay
                (cons
                 (make-overlay (match-beginning 0) (match-end 0)
                               (current-buffer))
                 mbs-overlay))
          (overlay-put (car mbs-overlay) 'face (cdr key)))))))

(defun mbs-delte-overlay ()
  (dolist (ov mbs-overlay) (delete-overlay ov))
  (setq mbs-overlay    nil
        mbs-face-point nil
        mbs-key        nil))

(defun mew-summary-display-postscript-wrap (func &rest args)
  (mbs-match-highlight)
  (apply func args))

(advice-add 'mew-summary-undo-all :after 'mbs-delte-overlay)

;; サーチワードが正規表現のとき M-n 等するとマッチハイライトがハミるのを回避.
(defvar mbs-isearch-highlight-org (symbol-function 'isearch-highlight))

(defun mbs-isearch-highlight (beg end)
  (let ((beg (- end (length (match-string 0)))))
    (funcall mbs-isearch-highlight-org beg end)))

(advice-add 'mew-summary-find-keyword-down
            :around 'mew-summary-find-keyword-down-new)
(defun mew-summary-find-keyword-down-new (org &optional arg)
  (advice-add 'isearch-highlight :override 'mbs-isearch-highlight)
  (funcall org arg)
  (advice-remove 'isearch-highlight 'mbs-isearch-highlight))

(advice-add 'mew-summary-find-keyword-up
            :around 'mew-summary-find-keyword-up-new)
(defun mew-summary-find-keyword-up-new (org &optional arg)
  (advice-add 'isearch-highlight :override 'mbs-isearch-highlight)
  (funcall org arg)
  (advice-remove 'isearch-highlight 'mbs-isearch-highlight))

;; `,' を押して表示される html source 等の生バッファでも
;; マッチハイライト(overlay)が効きます.
;; 生表示用バッファでは mew-message-hook が効かない(ように敢えてしてある)ための
;; 措置ですが、何か理由があってのことだと思いますが具体的な理由が判らないため、
;; 問題があった場合の為に変数で切れるようにしてあります.
;; この機能を切りたい場合は .mew.el や init.el 等に以下の1文を加えてください.
;; (setq mbs-highlight-func-asis nil) 

(defvar mbs-highlight-func-asis t
  "*生バッファのハイライトをアクティブにするか?")
(if mbs-highlight-func-asis
    (advice-add 'mew-summary-display-postscript
                :around 'mew-summary-display-postscript-wrap))

(defun mbs-remove-highlight-function()
  (interactive)
  (advice-remove 'mew-summary-display-postscript
                 'mew-summary-display-postscript-wrap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mew Builtin Search
;;;

(setq mew-search-switch
      (cons 
       '(builtin "Builtin" nil
                 mew-search-with-builtin
                 mew-search-virtual-with-builtin
                 nil nil nil nil nil)
       mew-search-switch))

(defvar mbs-no-dot-directory      "[^.][^.]?\\'")
(defvar mbs-mew-message-file-name (concat "[0-9]+\\" mew-suffix "\\'"))

(defvar mbs-lc-list
  '("" (:propertize
        (:eval (mapconcat #'identity mbs-search-location-list " "))
        face mbs-mode-line)
    " "))

;; (defvar mbs-mail-files-total     nil "Mail folder file total.")
(defvar mbs-thread-table         nil "BG thread work.")
(defvar mbs-search-location-list nil "Search location staring list work.")

(make-obsolete 'mbs-ignore-directory nil "1.4")

(defun mbs-path-to-mew-folder (path)
  (string-match (concat (mew-expand-folder mew-folder-local) "/") path)
  (directory-file-name
   (or (file-name-directory (replace-match "" 'fixed-case nil path)) "")))

(defun mbs-file-only-length (list)
  (let ((c 0))
    (dolist (f list c)
      (if (string-match "\\`CD:" f)
          nil
        (setq c (1+ c))))))

(defun mew-search-with-builtin (regexp folder &optional _dummy)
  "A function to pick messages matching REGEXP.
注意: この関数の中で返り値の拡張子を除去しないといけいない."
  (let* ((files (directory-files
                 (mew-expand-folder folder) nil mbs-mew-message-file-name))
         (case mbs-case-fold-search)
         (all (length files))
         (cnt all)
         msgs)
    (mbs-upush regexp)
    (dolist (message files)
      (let ((match (funcall mbs-match-function regexp message case)))
        (mbs-progress-message all cnt "Scan message %-8s %d%%..." folder)
        (if match
            (setq msgs (cons (file-name-sans-extension message) msgs))))
      (setq cnt (1- cnt)))
    (sort msgs
          #'(lambda (a b) (< (string-to-number a) (string-to-number b))))))

(defun *mbs-sort (list)
  (reverse (sort list #'string-version-lessp)))

(defun mbs-sort (list)
  "\"c:/foo/1.mew\" \"c:/foo/11.mew\" \"c:/foo/2.mew\" ...
等の文字列リストを数値として(逆順)ソート.
数値文字列でなくてもエラーにならず 0 になるので問題はない."
  (sort
   list
   #'(lambda (a b)
       (> (string-to-number
           (file-name-nondirectory (file-name-sans-extension a)))
          (string-to-number
           (file-name-nondirectory (file-name-sans-extension b)))))))

(defun mew--search-virtual-with-builtin (regexp path &optional _dummy)
  "PATH の中を再帰的に降りて集めた `mbs-mew-message-file-name' から
REGEXP にマッチする内容を持つファイルの list を \"CD:\" 付で返す.

PATH が atom なら `directory-files' に渡して file の list にし
list ならそのまま file list として処理をする.

`current-prefix-arg' が NIL なら
`mbs-ignore-directory' で指定された ディレクトリは除外される."
  (let* ((files  (if (consp path)
                     path
                   (directory-files path t mbs-no-dot-directory)))
         (all    (length files))
         (count  all)
         (case   mbs-case-fold-search)
         (prefix current-prefix-arg)
         (thread (current-thread))
         (vector (mbs-get-thread-table thread mbs-thread-table))
         chdir result att temp)
    (dolist (file files)
      (setq att (file-attributes file))
      (and (null mbs-thread-slice-time)
           (mbs-progress-message
            all count "Scan folder %s...%d%%"
            (mew-path-to-folder (substring (file-name-directory file) 0 -1))))
      (cond
       ((null att)
        (message "File can't open %s." file))
       ((and (car att)
             (string-equal "drwx" (substring (nth 8 att) 0 4))             
             (or prefix (not (string-match mbs-ignore-directory file))))
        (and (null mbs-thread-slice-time)
             (message "Scan folder %s..." (mew-path-to-folder file)))
        (aset vector (mbs-vector-location) file)
        (setq result
              (append (mew--search-virtual-with-builtin regexp file) result)))
       ((string-match mbs-mew-message-file-name file)
        (when (funcall mbs-match-function regexp file case)
          (or chdir
              (setq chdir
                    (cons
                     (concat "CD:" mew-folder-local
                             (mbs-path-to-mew-folder file))
                     nil)))
          (setq temp (cons (concat (file-name-nondirectory file)) temp)))))
      (and mbs-thread-slice-time (sleep-for mbs-thread-slice-time))
      (aset vector (mbs-vector-point) (1+ (aref vector (mbs-vector-point))))
      (setq count (1- count)))
    (append (mbs-sort temp) chdir result)))

(defun mbs-split-arg-to-path (folder)
  (if (consp folder)
      (mapcar #'(lambda(f) (mew-expand-folder f)) folder)
    (mew-expand-folder folder)))

(defun mew-search-virtual-with-builtin (regexp flds &optional _dummy)
  (let* ((path (mbs-split-arg-to-path (or flds mew-folder-local))) ; "~/Mail"
	 (file (mew-make-temp-name))
         (msgs (nreverse (mew--search-virtual-with-builtin regexp path))))
    (mbs-upush regexp 'clear)  ; 仕切り直しなので履歴はクリア
    (insert (mapconcat #'identity msgs "\n"))
    (mew-frwlet mew-cs-text-for-read mew-cs-text-for-write
      (write-region (point-min) (point-max) file nil 'no-msg))
    (list file (mbs-file-only-length msgs))))

(defun mbs-directories-total (directories flds)
  "DIRECTORIES 以下のアクセス可能な総ファイル数を返す.
手早く済ませる為に `mbs-mew-message-file-name' 以外も計上されるのでメール数の概算でしかない.
FLDS が non-nil なら `mbs-ignore-directory' の設定は無視される."
  (let* ((files (if (consp directories) directories
                  (directory-files directories t "[^.][^.]?\\'" 'nosort)))
         (total (length files)))
    (dolist (f files total)
      (when (and 
             (or flds (not (string-match mbs-ignore-directory f)))
             (file-accessible-directory-p f))
        (setq total (+ (mbs-directories-total f flds) total))))))

;; コマンド立ち上げ導入部の前部のインタラクティブ部と
;; 後部のバッファ生成部を分割して後部を BG 実行.
;; (インタラクティブパートを裏実行させる知識と技術がない)
(advice-add 'mew-summary-selection-by-search
            :override 'mew-summary-selection-by-search-wrap)

(defvar mbs-front-hook nil)
(defvar mbs-after-hook nil)

;; ;; マルチスレッド実行中相性の悪い tooltip / migemo サーチを切る.
;; (add-hook 'mbs-front-hook
;;           #'(lambda ()
;;               (tooltip-mode -1)
;;               (and migemo-isearch-enable-p (migemo-isearch-toggle-migemo))))
;; (add-hook 'mbs-after-hook
;;           #'(lambda ()
;;               (tooltip-mode 1)
;;               (or migemo-isearch-enable-p (migemo-isearch-toggle-migemo))))

(defun mew-summary-selection-by-search-wrap (&optional ask-folder)
  "Making selection according to a specified pick pattern
with a search method."
  (interactive "P")
  (if (not mew-search-method)
      (message "No search method")
    (let* ((ent (mew-search-get-ent mew-search-method))
	   (func (mew-search-get-func-virtual ent))
	   (name (mew-search-get-name ent))
	   (canon-func (mew-search-get-func-canonicalize-pattern ent))
	   (flt-func (mew-search-get-func-filter ent))
	   opattern pattern flds filter total)
      (run-hooks 'mbs-front-hook)      
      (when (and mbs-thread-slice-time
                 (not (member mbs-lc-list global-mode-string)))
        (setq global-mode-string (cons mbs-lc-list global-mode-string)))
      (if (not (fboundp func))
	  (message "This command cannot be used")
	(when ask-folder
          (setq flds (mew-input-folders
                      (or (mew-summary-folder-name)
                          (mew-case-folder
                           mew-case
                           (mew-proto-inbox-folder (mew-proto mew-case)))))))
	(setq opattern (if flt-func
			   (read-string (concat name " virtual pattern: "))
			 (mew-input-pick-pattern (concat name " virtual"))))
	(if (and (string= opattern "") (not (fboundp flt-func)))
	    (message (mew-substitute-for-summary "Keyword must be specified"))
	  (if (string= opattern "") (setq opattern " "))
	  (if (and canon-func (fboundp canon-func))
	      (setq pattern (funcall canon-func opattern))
	    (setq pattern opattern))
	  (when (fboundp flt-func)
	    (setq filter (funcall flt-func))
	    (if (string= opattern " ") (setq opattern ""))
	    (setq opattern (concat opattern filter)))
          (when mbs-thread-slice-time
            (message "Prescan...")
            (setq total (mbs-directories-total
                         (mapcar #'mew-expand-folder
                                 (or flds (list mew-folder-local)))
                         flds))
            (message "Prescan...done (%d files)" total))
          (setq mbs-thread-table
                (cons
                 ;;  0      1     2        3       4    5      6        7     8
                 ;; [thread timer opattern pattern flds filter location point start]
                 (vector nil nil opattern pattern flds filter "+" 0 (current-time))
                 mbs-thread-table))
          (if (null mbs-thread-slice-time)
              (mew-summary-selection-by-search-core)
            (with-mutex (make-mutex)
              (aset (car mbs-thread-table) (mbs-vector-timer)
                    (run-at-time
                     t mbs-thread-timer 'mbs-search-location-set opattern total))
              (message nil)
              (mbs-search-location-set opattern total)
              (force-mode-line-update)
              (make-thread
               'mew-summary-selection-by-search-core opattern))))))))

(defun mbs-get-thread-table (thread table)
  "thread の値の一致する vector を vector の束である TABLE から返す."
  (catch 'break
    (dolist (v table)
      (when (equal thread (aref v (mbs-vector-thread)))
        (throw 'break v)))))

(defun mbs-thread-remove-table (thread table)
  "THREAD が一致する vector を TABLE から削除し, その table のコピーを返す."
  (let (result)
    (dolist (v table result)
      (when (not (eq thread (aref v (mbs-vector-thread))))
        (setq result (cons v result))))))

(defun mbs-thread-cleanup (&optional thread)
  "thread-signal を発行するとそれ以降が実行されないので、
スレッドを停止するのを最後にしその前にやることを済ませる."
  (let* ((tmp (copy-sequence mbs-thread-table))
         (th (and thread
                  (catch 'out
                    (dolist (a tmp)
                      (if (eq thread (aref a (mbs-vector-thread)))
                          (throw 'out (copy-sequence a))))))))
    (cond
     ((and thread th)
      (setq mbs-search-location-list
            (mbs-delete-keyword (aref th (mbs-vector-pattern))
                                mbs-search-location-list))
      (setq mbs-thread-table (delete th mbs-thread-table))
      (unless mbs-thread-table
        (setq global-mode-string (delete mbs-lc-list global-mode-string))
        (force-mode-line-update))
      (cancel-timer (aref th (mbs-vector-timer)))
      (thread-signal (aref th (mbs-vector-thread)) 'quit nil))
     ((null thread)
      (setq mbs-thread-table         nil
            mbs-search-location-list nil)
      (setq global-mode-string (delete mbs-lc-list global-mode-string))
      (force-mode-line-update)
      (dolist (v tmp)
        (cancel-timer (aref v (mbs-vector-timer)))
        (thread-signal (aref v (mbs-vector-thread)) 'quit nil))))))

(add-hook 'mew-quit-hook   'mbs-thread-cleanup)
(add-hook 'kill-emacs-hook 'mbs-thread-cleanup)
(defvar mbs-laptime nil "*Display laptime.")

(defun mew-summary-selection-by-search-core ()
  "全文検索バッファ生成部.
オリジナルではサーチ開始前に器を用意しておくが、
BGサーチ完了後に用意するようにして不用意な操作等で誤って壊されにくくしてある."
  (let* ((ofolder (mew-summary-folder-name 'ext))
         (ent (mew-search-get-ent mew-search-method))
         (func (mew-search-get-func-virtual ent))
         (name (mew-search-get-name ent))
         (canon-func (mew-search-get-func-canonicalize-pattern ent))
         (flt-func (mew-search-get-func-filter ent))
         (vector   (mbs-get-thread-table nil mbs-thread-table))
         (timer    (aref vector (mbs-vector-timer)))
         (opattern (aref vector (mbs-vector-opattern)))
         (pattern  (aref vector (mbs-vector-pattern)))
         (flds     (aref vector (mbs-vector-flds)))
         (filter   (aref vector (mbs-vector-filter)))
         (thread   (current-thread))
         (inhibit-quit t)
         vfolder dfunc file opts rttl file-rttl)
    (aset vector (mbs-vector-thread) thread)
    (when (mew-summary-exclusive-p)
      (condition-case err
          (with-temp-buffer
            (mew-set-buffer-multibyte t)
            (mew-piolet mew-cs-text-for-read mew-cs-text-for-write
              (setq file-rttl (funcall func pattern flds filter))))
        (error
         (let ((th (thread-last-error))
               (ms (error-message-string err)))
           (if mbs-thread-slice-time
               (progn
                 (message "Thread error: %s: %s" th ms)
                 (mbs-thread-cleanup thread))
             (setq mbs-thread-table nil)
             (error "%s" ms))))))
    (mew-set '(file rttl) file-rttl)
    (setq vfolder (mew-folder-to-selection opattern))
    (mew-summary-switch-to-folder vfolder (and mbs-thread-unswitch 'set-buffer))
    (mew-vinfo-set-mode 'selection)
    (mew-vinfo-set-physical-folder nil)
    (mew-vinfo-set-original-folder ofolder)
    (mew-sinfo-set-find-key opattern)
    (make-local-variable 'mew-summary-form-mark-delete)
    (setq mew-summary-form-mark-delete nil)
    (make-local-variable 'mew-summary-form-mark-spam)
    (setq mew-summary-form-mark-spam nil)
    (setq dfunc `(lambda () (mew-delete-file ,file)))
    (setq opts (list "-i" file))
    (if mbs-laptime
        (message "%s %s...done."
                 (aref vector (mbs-vector-pattern))
                 (format-seconds
                  "%hh %mm %z%ss" 
                  (time-subtract (current-time) (aref vector (mbs-vector-start))))))
    (when mbs-thread-slice-time
      (and timer (cancel-timer timer))
      (when (null (setq mbs-search-location-list
                        (mbs-delete-keyword opattern mbs-search-location-list)))
        (setq global-mode-string
              (delete mbs-lc-list global-mode-string))))
    (setq mbs-thread-table (mbs-thread-remove-table thread mbs-thread-table))
    (mew-local-retrieve 'vir opts dfunc nil nil rttl)
    (and (eq 'open mbs-thread-unswitch) (display-buffer vfolder))
    (run-hooks 'mbs-after-hook)))

(defun mbs-get-pattern-table (pattern table)
  (catch 'break
    (dolist (v table)
      (if (string-equal pattern (aref v (mbs-vector-pattern)))
          (throw 'break v)))))

(defun mbs-delete-keyword (regexp lst)
  "フォーマットされたサーチキーワード REGEXP を取り除いた LST を返す."
  (delq nil
        (mapcar
         #'(lambda (str)
             (if (string-equal
                  (substring
                   str (progn
                         (string-match "\\`[0-9]+?%%" str)
                         (match-end 0)))
                  regexp)
                 nil
               str))
         lst)))

(defun mbs-search-location-set (regexp total)
  "REGEXP の進捗率を `mbs-search-location-list' にセット.
TOTAL は対象になるメッセージの総数.( 旧 mbs-mail-files-total)
既にセットされていれば最新の情報に更新する."
  (let* ((vector (mbs-get-pattern-table regexp mbs-thread-table))
         ;; (loc (aref vector (mbs-vector-location)))
         (point  (aref vector (mbs-vector-point)))
         (str    (format
                  "%d%%%%%s"
                  (- 100 (mbs-progress total point)) regexp))
         ;; (str      (propertize str 'help-echo loc))
         (others (mbs-delete-keyword regexp mbs-search-location-list)))
    (setq mbs-search-location-list
          (sort (cons str others)
                #'(lambda (a b)
                    (< (string-to-number a) (string-to-number b)))))))

;; この拡張を実現するにあたり、オリジナル Source のかなりの部分をそのまま使っています.
;; オリジナルのコードが持っているコピーライトや配布規定はこの次にある引用によるものとし、
;; 拡張部分の配布規定等はこのファイルヘッダにあるよう GPL3 とします.
;; 引用部出典は Mew 6.8 の配布パッケージの中の記載ファイル名のものです.

;;; Original Copyright.

;; ━━━━━ここから━━━━━
;;; Copyright Notice:

;; Copyright (C) 2005-2015 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-search.el ends here
;; ━━━━━ここまで━━━━━

(provide 'mew-search-with-builtin)
;; fin.
