;;; personal-common.el -- preferences and setup for all instances of Emacs

;;;============================================================================
;;; BASIC CONFIGURATION

;; Miscellaneous preference variables
(setq
 calendar-latitude 42.73
 calendar-longitude -73.69
 calendar-location-name "Albany, NY"
 column-number-mode t
 completion-ignore-case t
 confirm-kill-emacs 'y-or-n-p
 default-fill-column 79
 default-input-method "latin-1-postfix"
 default-tab-width 8
 display-time-24hr-format t
 display-time-day-and-date t
 font-lock-maximum-decoration t
 frame-title-format '("%f")
 grep-command "grep -inr "
 hourglass-delay 2
 inhibit-default-init t
 inhibit-startup-echo-area-message user-login-name
 inhibit-startup-message t
 kill-whole-line t
 magic-mode-alist '()
 message-signature-separator "^-- *$"
 mouse-wheel-progressive-speed nil
 mouse-yank-at-point t
 read-file-name-completion-ignore-case t
 require-final-newline t
 ring-bell-function (lambda () nil)
 save-abbrevs nil
 scroll-conservatively 20
 sentence-end "[.?!][]\"')}]*\\($\\|[ \t]\\)[ \t\n]*"
 sentence-end-double-space nil
 speedbar-show-unknown-files t
 speedbar-use-images nil
 use-dialog-box nil
 woman-use-own-frame nil)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(uniquify-buffer-name-style (quote forward) nil (uniquify))
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; Unicode/i18n stuff
(prefer-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)

;; Adding/removing safeties
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Function aliases for typos and such
(fset 'grpe 'grep)

;; Some random file extentions that I need to set the mode for
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;;;============================================================================
;;; FUNCTION DEFINITIONS

(defun unfill-paragraph (current-point)
  "Unfill the paragraph containing point; i.e. remove all newlines between
the paragraph separator prior to point and the one after it."
  (interactive "d")
  (let ((fill-column (point-max)))
    (mark-paragraph)
    (fill-region (point) (mark))
    (goto-char current-point)))

(defun beginning-of-line-dwim ()
  "Move point to the beginning of the line, unless it is already there,
in which case move point to the first non-whitespace character on the line."
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(defun back-to-indentation-dwim ()
  "Move point to the first non-whitespace character on the line, unless it
is already there, in which case move point to the beginning of the line."
  (interactive)
  (if (= (point)
	  (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(defun buffer-beginning-contains (str &optional bound)
  "Search for STR in the first BOUND (default 1,024) characters of the current
buffer. Returns the character offset of the end of the first match, or nil if
no matches are found."
  (interactive)
  (setq bound (or bound 1024))
  (save-excursion
    (goto-char (point-min))
    (search-forward str bound t)))

;;;============================================================================
;;; ZENCODING MODE

;; This minor mode is only useful when editing HTML markup, i.e. when in
;; PSGML mode, so we bind its main function to a key in sgml-mode-map and
;; autoload the mode the first time it's invoked.

(autoload 'zencoding-expand-line "zencoding-mode"
  "Unfold CSS-selector-like expressions to markup." t)

(eval-after-load
    "psgml"
  '(progn
     (define-key sgml-mode-map (kbd "C-c C-l") 'zencoding-expand-line)))

(eval-after-load
    "zencoding-mode"
  '(progn
     (define-key zencoding-preview-keymap
       ;; This keybinding is specified as (kbd "<return>") in the actual
       ;; zencoding-mode package, which doesn't seem to work in a terminal.
       (kbd "RET") 'zencoding-preview-accept)))

;;;============================================================================
;;; MINOR MODES

;; Minor modes we DO want
(delete-selection-mode t)
(global-font-lock-mode t)
(transient-mark-mode t)

;; Use ido-mode if available, otherwise try iswitchb-mode
(cond
 ((fboundp 'ido-mode)
  (ido-mode t)
  (setq ido-confirm-unique-completion t
        ido-enable-flex-matching t
        ido-enable-prefix t
        ido-save-directory-list-file nil))
 ((fboundp 'iswitchb-mode)
  (iswitchb-mode t)))

;; Minor modes we DON'T want
(menu-bar-mode -1)

;; Minor mode and buffer-local variable setup for all buffers corresponding to
;; actual files (i.e. not "*Buffer List*" et al.)
(add-hook 'find-file-hooks
          (lambda ()
            (show-paren-mode t)
            (setq indent-tabs-mode nil
                  indicate-empty-lines t
                  show-trailing-whitespace t
                  truncate-lines t)))

;;;============================================================================
;;; UNICODE INPUT

(setq unicode-character-list-file (locate-library "unichars"))

(autoload 'unicode-character-insert "xmlunicode"
  "Insert a Unicode character referenced by Unicode character name." t)
(autoload 'iso8879-character-insert "xmlunicode"
  "Insert a Unicode character referenced by ISO-8879 entity name." t)
(autoload 'unicode-character-shortcut-insert "xmlunicode"
  "Insert a Unicode character referenced by a two-character shortcut." t)

(autoload 'unicode-smart-single-quote "xmlunicode")
(autoload 'unicode-smart-double-quote "xmlunicode")
(autoload 'unicode-smart-hyphen "xmlunicode")
(autoload 'unicode-smart-period "xmlunicode")

;;;============================================================================
;;; HIPPIE EXPAND

(autoload 'hippie-expand "hippie-exp" "Hippie-expand library" t)
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;;============================================================================
;;; YASNIPPET

(add-to-list 'load-path (concat shared-lisp-dir "/yasnippet") t)
(require 'yasnippet)

(setq
 yas/extra-mode-hooks '(javascript-mode-hook sgml-mode-hook xml-mode-hook)
 yas/trigger-key (kbd "`")
 yas/use-menu nil)

(yas/initialize)
(yas/load-directory (concat shared-lisp-dir "/yasnippet/snippets"))

;; Reindent all lines of a snippet after insertion...
(add-hook 'yas/after-exit-snippet-hook
          (lambda ()
            (save-excursion
              (indent-region yas/snippet-beg yas/snippet-end nil))
            (indent-according-to-mode)))

;;;============================================================================
;;; KEYBINDINGS

;; I find myself wanting to do hippie-expand much more frequently than I want
;; to type a backtick into a buffer
(define-key global-map (kbd "`") 'hippie-expand)

;; Override default `move-beginning-of-line' keybinding with DWIM version
(define-key global-map (kbd "C-a") 'beginning-of-line-dwim)

;; The rest of these keybindings are either already free or attached by
;; default to functions I rarely or never use
(define-key global-map (kbd "M-g")         'goto-line)
(define-key global-map (kbd "M-'")         'expand-abbrev)
(define-key global-map (kbd "M-Q")         'unfill-paragraph)
(define-key global-map (kbd "C-c w")       'delete-trailing-whitespace)
(define-key global-map (kbd "C-c \\")      'toggle-truncate-lines)
(define-key global-map (kbd "C-c q")       'query-replace-string)
(define-key global-map (kbd "C-c M-q")     'query-replace-regexp)
(define-key global-map (kbd "C-c r")       'replace-string)
(define-key global-map (kbd "C-c M-r")     'replace-regexp)
(define-key global-map (kbd "C-c C-x C-f") 'find-file-at-point)
(define-key global-map (kbd "C-c l")       'linum-mode)
(define-key global-map (kbd "C-x g")       'uncomment-region)
(define-key global-map (kbd "C-c g")       'comment-region)

(define-key global-map (kbd "<f1>")      'switch-to-buffer)
(define-key global-map (kbd "ESC <f1>")  'kill-buffer)
(define-key global-map (kbd "<f2>")      'buffer-menu)
(define-key global-map (kbd "<f3>")      'start-kbd-macro)
(define-key global-map (kbd "ESC <f3>")  'end-kbd-macro)
(define-key global-map (kbd "<f4>")      'call-last-kbd-macro)
(define-key global-map (kbd "<f5>")      'occur)
(define-key global-map (kbd "ESC <f5>")  'grep)
(define-key global-map (kbd "<f6>")      'string-rectangle)
(define-key global-map (kbd "<f7>")      'query-replace)
(define-key global-map (kbd "ESC <f7>")  'replace-string)
(define-key global-map (kbd "<f8>")      'query-replace-regexp)
(define-key global-map (kbd "ESC <f8>")  'replace-regexp)

;;;============================================================================
;;; PYTHON

(add-to-list 'load-path (concat shared-lisp-dir "/python") t)
(autoload 'python-mode "python-mode" "Major mode for Python files." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;;;============================================================================
;;; C (and derived modes)

(c-add-style "k&r-modified"
             '("k&r"
               (c-basic-offset . 4)
               (c-offsets-alist . ((case-label . +)
                                   (arglist-close . 0)
                                   (defun-open . 0)
                                   (inline-open . 0)))
               (c-hanging-braces-alist
                (substatement-open after))))

(add-hook
 'c-mode-common-hook
 (lambda ()
   (c-set-style "k&r-modified")
   ;; Activate c-subword-mode if available (in Emacs 22.1 and up)
   (when (fboundp 'c-subword-mode)
     (c-subword-mode))))

;;;============================================================================
;;; JAVASCRIPT

(autoload 'js2-mode
  "js2-mode" "Steve Yegge's major mode for JavaScript files." t)
(autoload 'javascript-mode
  "javascript-mode" "Major mode for JavaScript files." t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

(setq
 js2-allow-keywords-as-property-names nil
 js2-basic-offset 4
 js2-enter-indents-newline nil
 js2-mirror-mode nil
 js2-strict-inconsistent-return-warning nil)



;;;============================================================================
;;; SQL

(autoload 'sql-mode
  "sql" "Major mode for SQL files." t)
(add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-mode))
(add-to-list 'load-path (concat shared-lisp-dir "/sql") t)


;;;============================================================================
;;; PLSQL

(autoload 'plsql-mode
  "plsql" "Major mode for SQL files." t)
(add-to-list 'auto-mode-alist '("\\.pk[sb]\\'" . plsql-mode))

(add-to-list 'load-path (concat shared-lisp-dir "/sql") t)


;;;============================================================================
;;; PHP

(add-to-list 'load-path (concat shared-lisp-dir "/php") t)
(autoload 'php-mode "php-mode" "Major mode for PHP files." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))

(setq php-completion-file (concat shared-lisp-dir "/php/php-builtins.txt"))

(eval-after-load
    "php-mode"
  '(progn
     ;; Take over the keybinding for dabbrev-expand since we already have
     ;; hippie-expand on backtick
     (define-key php-mode-map (kbd "M-/") 'php-complete-function)))

;;;============================================================================
;;; CSS

(autoload 'css-mode "css-mode" "Major mode for CSS files." t)
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))

(setq
 css-tab-mode 'indent
 css-indent-offset 2)

;;;============================================================================
;;; (X)HTML

(add-to-list 'load-path (concat shared-lisp-dir "/psgml") t)
(autoload 'sgml-mode "psgml" "Major mode for HTML/SGML files." t)
(autoload 'xml-mode  "psgml" "Major mode for XHTML/XML files." t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.html.php\\'" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.html.twig\\'" . sgml-mode))

(add-to-list 'load-path (concat shared-lisp-dir "/nxml") t)
(load-library "rng-auto")

(autoload 'xxml-mode-routine "xxml")
(add-hook 'sgml-mode-hook 'xxml-mode-routine)
(add-hook 'xml-mode-hook 'xxml-mode-routine)

(setq
 sgml-files-dir (concat shared-lisp-dir "/psgml/dtd")
 sgml-catalog-files (list (concat sgml-files-dir "/catalog"))
 sgml-ecat-files (list (concat sgml-files-dir "/ecat"))
 sgml-ignore-undefined-elements nil
 sgml-auto-insert-required-elements t
 sgml-insert-missing-element-comment nil
 sgml-validate-command "onsgmls -s %s %s")

(setq
 sgml-custom-dtd
 '(("HTML 4.01 Transitional"
    "<!DOCTYPE HTML PUBLIC \
\"-//W3C//DTD HTML 4.01 Transitional//EN\" \
\"http://www.w3.org/TR/html4/loose.dtd\">\n")
   ("HTML 4.01 Strict"
    "<!DOCTYPE HTML PUBLIC \
\"-//W3C//DTD HTML 4.01//EN\" \
\"http://www.w3.org/TR/html4/strict.dtd\">\n")
   ("XHTML 1.0 Transitional"
    "<!DOCTYPE html PUBLIC \
\"-//W3C//DTD XHTML 1.0 Transitional//EN\" \
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n")
   ("XHTML 1.0 Strict"
    "<!DOCTYPE html PUBLIC \
\"-//W3C//DTD XHTML 1.0 Strict//EN\" \
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n")))

(defun sgml-set-parent-document ()
  "Set `sgml-parent-document' to a skeleton HTML 4.01 Transitional document
so that HTML fragments can be edited properly with PSGML."
  (interactive)
  (setq sgml-parent-document
        (list (concat shared-lisp-dir "/psgml/parent.html")
              "html" "body" '("div"))))

(add-hook
 'sgml-mode-hook
 (lambda ()
   "If the buffer appears to contain markup but no doctype declaration, run
`sgml-set-parent-document' to treat it as HTML 4.01 Transitional."
   (if (and (buffer-beginning-contains "<")
            (not (buffer-beginning-contains "<!DOCTYPE")))
       (sgml-set-parent-document))))

;; PSGML's SGML/XML modes inherit from text-mode, but smart-punctuation-mode
;; isn't a good idea, considering the number of quotes and stuff in markup
;; (add-hook 'sgml-mode-hook (lambda () (smart-punctuation-mode -1)))
;; (add-hook 'xml-mode-hook  (lambda () (smart-punctuation-mode -1)))

(eval-after-load
    "psgml"
  '(progn
     (define-key sgml-mode-map (kbd "C-c e") 'sgml-insert-element)
     (define-key sgml-mode-map (kbd "C-c h") 'sgml-set-parent-document)))

;;;============================================================================
;;; MULTIPLE-MAJOR-MODES

(add-to-list 'load-path (concat shared-lisp-dir "/mmm") t)
(require 'mmm-auto)

(setq
 mmm-global-mode 'maybe
 mmm-submode-decoration-level 2)

(eval-after-load
    "mmm-mode"
  '(progn
     (define-key mmm-mode-map (kbd "M-p") 'mmm-parse-buffer)
     (set-face-attribute 'mmm-default-submode-face nil
                         :background "unspecified-bg")
     (set-face-attribute 'mmm-code-submode-face nil
                         :background "unspecified-bg")))

(mmm-add-group
 'html-plus
 '((javascript-block
    :front "<script[^>]*>"
    :back "</script>"
    :submode javascript-mode)
   (javascript-handler
    :front (concat
            "on\\("
            (mapconcat
             'identity
             '("blur" "change" "click" "dblclick" "focus" "keydown" "keypress"
               "keyup" "load" "mousedown" "mousemove" "mouseout" "mouseover"
               "mouseup" "reset" "select" "submit" "unload") "\\|")
            "\\)=\"")
    :back "\""
    :submode javascript-mode)
   (css-block
    :front "<style[^>]*>"
    :back "</style>"
    :submode css-mode)
   (css-inline
    :front "style=\""
    :back "\""
    :submode css-mode)
   (php-block
    :front "<[?]" ;; formerly "<[?]\\S-*"
    :back "[?]>"
    :submode php-mode)))

(add-to-list 'mmm-mode-ext-classes-alist '(sgml-mode nil html-plus))
(add-to-list 'mmm-mode-ext-classes-alist '(xml-mode  nil html-plus))

;;;============================================================================
;;; YML config files

(autoload 'yaml-mode "yaml-mode" "Major mode for YML files." t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;;============================================================================
;;; EDIFF

(eval-after-load
    "ediff"
  '(progn
     (setq ediff-window-setup-function 'ediff-setup-windows-plain)))

;;;============================================================================
;;; ORG-MODE

;; Org-mode is distributed with Emacs as of version 22-point-something...
;; use it for *.org files if available, otherwise try outline-mode (from
;; which org-mode is derived)

(cond
 ((fboundp 'org-mode)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)))
 ((fboundp 'outline-mode)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . outline-mode))))

;;;============================================================================
;;; Flyspell

(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))

(add-hook 'fundamental-mode-hook (lambda () (flyspell-mode 1)))

;; (dolist (hook '(text-mode-hook))
;;       (add-hook hook (lambda () (flyspell-mode 1))))
;; (dolist (hook '(fundamental-mode-hook))
;;       (add-hook hook (lambda () (flyspell-mode 1))))

;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (flyspell-prog-mode)
;;             ; ...
;;           ))