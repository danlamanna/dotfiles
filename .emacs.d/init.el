(defvar emacs-config-dir (expand-file-name "~/.emacs.d"))
(defvar emacs-tmp-dir    (expand-file-name (concat emacs-config-dir "/" "tmp")))

;; add all of the everythings to the load path, cause, performance.
(let ((default-directory emacs-config-dir))
  (normal-top-level-add-subdirs-to-load-path))

;; must-have libraries/utilities
(require 's)
(require 'dash)

;; custom variables
(custom-set-variables
 '(asl/cache-enabled t)
 '(browse-url-browser-function (quote browse-url-chromium))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(dired-listing-switches "-alh")
 '(enable-recursive-minibuffers t)
 '(geben-dbgp-default-proxy (quote ("127.0.0.1" 9001 "dan" nil t)))
 '(geben-dbgp-feature-list
   (quote
    ((:set max_data 32768)
     (:set max_depth 1)
     (:set max_children 1024)
     (:get breakpoint_types geben-dbgp-breakpoint-store-types))))
 '(geben-dbgp-redirect-buffer-init-hook nil)
 '(geben-temporary-file-directory "~/.emacs.d/tmp/geben")
 '(go-command "/home/dan/src/go/bin/go")
 '(grep-command "grep -rin")
 '(indent-tabs-mode nil)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs))))
 '(php-completion-file
   (expand-file-name
    (format "%s/etc/php-completion.txt" emacs-config-dir)))
 '(php-manual-path
   (expand-file-name
    (format "%s/etc/php-manual" emacs-config-dir)))
 '(php-template-compatibility nil)
 '(save-interprogram-paste-before-kill t)
 '(vc-follow-symlinks t)
 '(virtualenv-root "~/.envs/")
 '(yank-pop-change-selection t))

(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(pending-delete-mode t)
(prefer-coding-system 'utf-8)

;; custom faces
(custom-set-faces
 '(flymake-errline ((t (:background "brightblack"))))
 '(magit-item-highlight ((t (:inherit default)))))

;; ace-jump-mode
(autoload 'ace-jump-word-mode "ace-jump-mode" t)
(autoload 'ace-jump-char-mode "ace-jump-mode" t)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; autopair
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; autosaves
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat emacs-tmp-dir "/backups")))))

;; coding standards
;; both these lists should be lowercased
(setq no-cleanup-filenames '("makefile"))
(setq no-cleanup-extensions '("md"))

(defun should-cleanup-buffer?()
  "Returns t if the buffer is an actual file, the files extension isn't in no-cleanup-extensions,
and it's name isn't in no-cleanup-filenames."
  (and (buffer-file-name)
       (not (-contains? no-cleanup-extensions (downcase (file-name-extension (buffer-file-name)))))
       (not (-contains? no-cleanup-filenames (downcase (file-name-nondirectory (buffer-file-name)))))))

(defun buffer-cleanup()
  "A less safe buffer cleanup, indents everything."
  (interactive)
  (buffer-cleanup-safe)
  (indent-region (point-min) (point-max)))

(defun buffer-cleanup-safe()
  (interactive)
  (when (should-cleanup-buffer?)
    (whitespace-cleanup)
    (untabify (point-min) (point-max))
    (set-buffer-file-coding-system 'utf-8)))

(add-hook 'before-save-hook 'buffer-cleanup-safe)
(global-set-key (kbd "C-c n") 'buffer-cleanup)

;; comint
(eval-after-load "comint-mode"
  (add-hook 'comint-mode-hook
            (lambda()
              (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
              (define-key comint-mode-map (kbd "<down>") 'comint-next-input)
              (define-key comint-mode-map (kbd "C-<up>") 'windmove-up)
              (define-key comint-mode-map (kbd "C-<down>") 'windmove-down))))

;; dired
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
     (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)))

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)

;; expand-region
(autoload 'er/expand-region "expand-region" t)
(define-key global-map (kbd "C-q") 'er/expand-region)

;; tramp
(require 'tramp)

(setq tramp-default-method "ssh")
(setq tramp-auto-save-directory (concat emacs-tmp-dir "/backups"))

(defun sudo-tramp-current-file()
  (interactive)
  (when buffer-file-name
    (let ((pos (point)))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
      (goto-char pos))))

;; geben
(defun geben-safely-end-proxy()
  "Tries to call `dbgp-proxy-unregister', but silently
   returns `nil' if it throws an error."
  (interactive)
  (condition-case nil
      (dbgp-proxy-unregister "dan")
    (error nil)))

(defun geben-single-or-proxy()
  "Tries calling geben, if it throws an error because it needs to use
   `geben-proxy', it tries that.
   TODO: make it toggle.."
  (interactive)
  (condition-case nil
      (geben)
    (error (geben-proxy "127.0.0.1" 9001 "dan"))))

(defadvice geben-display-context(before clear-windows-for-vars activate)
  (delete-other-windows))

(add-hook 'kill-emacs-hook 'geben-safely-end-proxy)

;; guide key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x n"))
(guide-key-mode 1)

;; ido
(require 'ido)
(ido-mode 'both)

(setq
 ido-save-directory-list-file (format "%s/ido.last" emacs-tmp-dir)
 ido-ignore-buffers '(".*Completion"
                      "\\*")
 ido-work-directory-list '("~/" "~/projects")
 ido-enable-flex-matching t
 ido-case-fold t
 ido-enable-last-directory-history t
 ido-max-directory-size 500000
 ido-max-work-directory-list 10
 ido-max-work-file-list 20
 ido-use-filename-at-point nil
 ido-use-url-at-point nil
 ido-max-prospects 7
 ido-create-new-buffer 'always
 ido-confirm-unique-completion nil)

(setq confirm-nonexistent-file-or-buffer nil)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read geben-find-file 'geben)
(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

;; key-chord
(require 'key-chord)

(key-chord-define-global "ww" 'ace-jump-word-mode)
(key-chord-define-global "jj" 'ace-jump-char-mode)
(key-chord-define-global "hh" '(lambda() (interactive) (insert "_")))
(key-chord-define-global "99" '(lambda() (interactive) (insert "(")))
(key-chord-define-global "00" '(lambda() (interactive) (insert ")")))
(key-chord-define-global "77" '(lambda() (interactive) (insert "&")))
(key-chord-define-global "==" '(lambda() (interactive) (insert "+")))
(key-chord-define-global "[[" '(lambda() (interactive) (insert "{")))
(key-chord-define-global "]]" '(lambda() (interactive) (insert "}")))
(key-chord-define-global "uu" 'undo-tree-visualize)

(key-chord-mode +1)

;; magit
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(eval-after-load "magit"
  '(progn
     (add-hook 'magit-mode-hook 'hl-line-mode)
     (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))

(define-key global-map (kbd "C-c s") 'magit-status)

(autoload 'magit-status "magit" t)

;; occur
(defun multi-occur-in-all-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))

(define-key global-map (kbd "C-c o") 'occur)
(define-key global-map (kbd "C-c O") 'multi-occur-in-matching-buffers)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; saveplace
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".saveplace" emacs-tmp-dir))

;; yasnippet
(require 'yasnippet)

(yas-global-mode 1)
(setq yas-trigger-key "TAB")

(setq yas-snippet-dirs
      '("~/.emacs.d/etc/snippets"))

(yas/reload-all)

;; smart-tab
(require 'smart-tab)
(global-smart-tab-mode 1)
(setq smart-tab-using-hippie-expand t)

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-complete-file-name-partially
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; php-eldoc
(eval-after-load "php-mode"
  '(progn
     (require 'php-eldoc)
     (define-key php-mode-map (kbd "C-c C-f") 'php-search-local-documentation)
     (define-key php-mode-map (kbd "<backtab>") 'php-complete-function)))

;; web-mode
(eval-after-load "php-mode"
  '(progn
     (require 'web-mode)
     (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
     (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))))

(eval-after-load "web-mode"
  '(setq web-mode-engine-file-regexps
         '(("asp"        . "\\.asp\\'")
           ("aspx"       . "\\.as[cp]x\\'")
           ("blade"      . "\\.blade")
           ("django"     . "\\.\\(djhtml\\|tmpl\\|dtl\\)\\'")
           ("django"     . "twig")
           ("erb"        . "\\.\\(erb\\|rhtml\\)\\'")
           ("freemarker" . "\\.ftl\\'")
           ("go"         . "\\.go\\(html\\|tmpl\\)\\'")
           ("handlebars" . "\\(handlebars\\|.\\hbs\\'\\)")
           ("jsp"        . "\\.jsp\\'")
           ("mustache"   . "\\.mustache\\'")
           ("php"        . "\\.\\(php\\|ctp\\|psp\\|inc\\|phtml\\)\\'")
           ("python"     . "\\.pml\\'")
           ("razor"      . "play\\|\\.scala\\.\\|\\.cshtml\\'\\|\\.vbhtml\\'")
           ("smarty"     . "\\.tpl\\'")
           ("velocity"   . "\\.\\(vsl\\|vtl\\|vm\\)\\'"))))

;; python-mode
;; (require 'python-mode)

;; (setq-default py-shell-name "ipython")
;; (setq-default py-which-bufname "IPython")
;;                                         ; use the wx backend, for both mayavi and matplotlib
;; (setq py-python-command-args
;;       '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
;; (setq py-force-py-shell-name-p t)

;; (setq py-shell-switch-buffers-on-execute-p nil)
;; (setq py-switch-buffers-on-execute-p nil)
;;                                         ; don't split windows
;; (setq py-split-windows-on-execute-p nil)
;;                                         ; try to automagically figure out indentation
;; (setq py-smart-indentation t)

;; ruby-mode
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; c-mode
(setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
(load "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

(require 'auto-complete)
(require 'irony)
(irony-enable 'ac)

(defun c-hooks()
  "Enable the hooks in the preferred order: 'yas -> auto-complete -> irony'."
  (yas/minor-mode-on)
  (auto-complete-mode 1)
  ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: php-mode
  (when (member major-mode irony-known-modes)
    (irony-mode 1)))

(add-hook 'c-mode-hook 'c-hooks)

(add-hook 'c-mode-hook '(lambda()
                          (global-flycheck-mode t)
                          (define-key c-mode-map (kbd "C-c C-c") 'comment-or-uncomment-line-or-region)))

;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; multiple-cursors
(autoload 'multiple-cursors "mc/mark-next-like-this" t)
(global-set-key (kbd "C-c SPC") 'set-rectangular-region-anchor)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; org-mode
;(eval-after-load "org-mode"
;  (require 'org-bullets)
;  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; package.el
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

;; restclient
(autoload 'restclient-mode "restclient" t)
(define-key global-map (kbd "C-c R") 'restclient-mode)

;; sql-mode
(eval-after-load "sql-mode"
  '(progn
     (add-hook 'sql-mode-hook 'sql-highlight-mysql-keywords)))

;; themes
(color-theme-sanityinc-tomorrow-night)

;; uniquify
(require 'uniquify)

(setq
 uniquify-buffer-name-style 'reverse
 uniquify-separator " - "
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*")

;;(require 'vlfi)

;; webjump
(require 'webjump)

(setq webjump-sites
      '(("stackoverflow" . [simple-query "stack overflow" "http://stackoverflow.com/search?q=" ""])
        ("google"        . [simple-query "google" "www.google.com/search?q=" ""])))

(global-set-key (kbd "C-x g") 'webjump)

;; winner-mode
(require 'winner)
(winner-mode t)

;; some better default keybindings
(define-key global-map (kbd "C-z") 'quoted-insert)
(define-key global-map (kbd "C-b") 'revert-buffer)
(define-key global-map (kbd "C-B") 'revert-all-buffers)
(define-key global-map (kbd "M-g") 'goto-line)
(define-key global-map (kbd "C-c g") 'grep)
(define-key global-map (kbd "C-c r") 'replace-string)
(define-key global-map (kbd "<f3>") 'start-kbd-macro)
(define-key global-map (kbd "ESC <f3>") 'end-kbd-macro)
(define-key global-map (kbd "<f4>") 'call-last-kbd-macro)
(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)
(define-key global-map (kbd "C-c C-c") 'comment-or-uncomment-line-or-region)

(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))
;; remove clutter...
(menu-bar-mode -1)
(show-paren-mode t)
(setq show-paren-style 'mixed)

(setq inhibit-splash-screen t)

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(defun toggle-fullscreen()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(global-set-key (kbd "<f11>") 'toggle-fullscreen)

;; mode-line formatting
(setq default-mode-line-format
      (list
       "-- "
       ;; Displays buffer name bolded
       '(:eval (propertize "%b" 'face 'bold 'help-echo (buffer-name)))
       ;; Displays ** bolded if the file has been modified (and it's not a readonly buffer)
       '(:eval (when (and (buffer-modified-p)
                          (eq buffer-read-only nil)
                          (not (eq (buffer-file-name) nil)))
                 (propertize "**" 'face 'bold)))
       ;; Display percent from top, then line num, col num
       ;; only if its a file buffer
       '(:eval (if (not (eq (buffer-file-name) nil))
                   "  %p (L%l,C%c)"))
       '(:eval (if mark-active (format " [%s]" (length (buffer-substring-no-properties (mark) (point))))))
       ;; Major mode in brackets
       " [%m] "
       ;; Display time, followed by dashes till the end
       '(:eval (propertize (format-time-string "%l:%M%p")))))

(toggle-fullscreen)

;; the following are all just convenience/helper functions, unrelated to libraries
(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun kill-emacs-no-prompt()
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(global-set-key (kbd "C-x C-c") 'kill-emacs-no-prompt)

(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

(define-key global-map (kbd "C-a") 'back-to-indentation-or-beginning)

(defadvice zap-to-char (after zap-until-char (arg char) activate)
  "Makes zap-to-char act like zap-until-char."
  (insert char)
  (backward-char 1))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(defun revert-all-buffers()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (buffer-file-name buffer)
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "done."))

(defun open-line-below ()
  (interactive)
  (if (eolp)
      (newline)
    (end-of-line)
    (newline))
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-S-return>") 'open-line-above)

(fset 'yes-or-no-p 'y-or-n-p)

;; Indent pasted code in these modes:
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(asm-mode
                                     c++-mode
                                     c-mode
                                     clojure-mode
                                     cperl-mode
                                     csharp-mode
                                     espresso-mode
                                     factor-mode
                                     haskell-mode
                                     js-mode
                                     latex-mode
                                     lisp-mode
                                     lua-mode
                                     nxml-mode
                                     objc-mode
                                     php-mode
                                     plain-tex-mode
                                     python-mode
                                     rspec-mode
                                     ruby-mode
                                     rust-mode
                                     scheme-mode
                                     vbnet-mode
                                     emacs-lisp-mode
                                     web-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; copy buffer contents to kill ring before reverting, just in case
(add-hook 'before-revert-hook  (lambda () (kill-ring-save (point-min) (point-max))))

;; remove prompt of killing a buffer with a running process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))
