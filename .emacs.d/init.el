(defvar emacs-config-dir (expand-file-name "~/.emacs.d"))
(defvar emacs-tmp-dir    (expand-file-name (concat emacs-config-dir "/" "tmp")))

;; add all of the everythings to the load path, cause, performance.
(let ((default-directory emacs-config-dir))
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory "~/dotfiles/deps"))
  (normal-top-level-add-subdirs-to-load-path))

;; package.el
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(defvar elpa-required-packages '(ac-etags
                                 ace-jump-mode
                                 ack-and-a-half
                                 auto-complete-exuberant-ctags
                                 autopair
                                 browse-kill-ring
                                 c-eldoc
                                 company-go
                                 eldoc-eval
                                 emmet-mode
                                 etags-select
                                 etags-table
                                 expand-region
                                 flycheck-haskell
                                 flymake-python-pyflakes
                                 framemove
                                 geben
                                 gist
                                 gitconfig-mode
                                 gitignore-mode
                                 go-eldoc
                                 go-errcheck
                                 google-c-style
                                 gtags
                                 guide-key
                                 ido-ubiquitous
                                 ipython
                                 jedi
                                 key-chord
                                 mag-menu
                                 magit
                                 makey
                                 markdown-mode
                                 multi-term
                                 multiple-cursors
                                 org-bullets
                                 php-eldoc
                                 php-mode
                                 puppet-mode
                                 pyvirtualenv
                                 request
                                 restclient
                                 s
                                 skewer-mode
                                 smart-tab
                                 smex
                                 twittering-mode
                                 undo-tree
                                 virtualenv
                                 vlf
                                 web-mode
                                 websocket
                                 xcscope)
  "Default Packages")

(dolist (pkg elpa-required-packages)
  (if (not (package-installed-p pkg))
      (package-install pkg)))

;; must-have libraries/utilities
(require 's)
(require 'dash)
(require 'use-package)

;; custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(asl/cache-enabled t)
 '(auto-save-interval 60)
 '(browse-url-browser-function (quote browse-url-firefox))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("454dc6f3a1e9e062f34c0f988bcef5d898146edc5df4aa666bf5c30bed2ada2e" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "b1ec9b3c5dbd26abea9df6181a2cd149c9f48602ded9bc0e87ce130387456ab3" "1f70ca6096c886ca2a587bc10e2e8299ab835a1b95394a5f4e4d41bb76359633" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
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
 '(large-file-warning-threshold 100000000) ;; 100MB
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
 '(wdired-allow-to-change-permissions t)
 '(yank-pop-change-selection t))

(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(pending-delete-mode t)
(prefer-coding-system 'utf-8)

(define-key global-map (kbd "C-c b") 'browse-url-at-point)

;; custom faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:background "brightblack"))))
 '(magit-item-highlight ((t (:inherit default)))))

;; ace-jump-mode
(use-package ace-jump-mode
  :bind ("C-x SPC" . ace-jump-mode-pop-mark)
  :commands (ace-jump-word-mode
             ace-jump-char-mode)) ;; autoload on either of these

;; ack
(use-package ack-and-a-half
  :bind ("C-c a" . ack)
  :init (progn
          (defalias 'ack 'ack-and-a-half)
          (defalias 'ack-same 'ack-and-a-half-same))
  :config (setq ack-and-a-half-use-ido t))

;; autopair
(use-package autopair
  :init (autopair-global-mode))

;; autosaves/backups
(setq emacs-autosave-dir (concat emacs-tmp-dir "/autosaves/"))
(setq auto-save-list-file-prefix emacs-autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,emacs-autosave-dir t)))

(setq
 vc-make-backup-files t ; backup version controlled files, too
 backup-by-copying t ; no symlinks
 delete-old-versions t ; no confirm
 kept-new-versions 10
 kept-old-versions 0
 version-control t ; number backups
 backup-directory-alist
 `(("." . ,(expand-file-name
            (concat emacs-tmp-dir "/backups")))))

;; bookmarks
(setq bookmark-save-flag 1) ;; save after a bookmark is added once

;; clojure
(add-hook 'clojure-mode-hook (lambda()
                               (require 'cider-mode)))

(add-hook 'cider-mode-hook (lambda()
                             (require 'cider-eldoc)
                             (cider-turn-on-eldoc-mode)))

;; coding standards
;; both these lists should be lowercased
(setq no-cleanup-filenames '("makefile"))
(setq no-cleanup-extensions '("md" "org" "xml"))

(defun should-cleanup-buffer?()
  "Returns t if the buffer is an actual file, the files extension isn't in no-cleanup-extensions,
and it's name isn't in no-cleanup-filenames."
  (and (buffer-file-name)
       (not (-contains? no-cleanup-filenames (downcase (file-name-nondirectory (buffer-file-name)))))
       (not (and (file-name-extension (buffer-file-name)) ;has a file extension
                 (-contains? no-cleanup-extensions (downcase (file-name-extension (buffer-file-name))))))))

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
(use-package comint
  :config (progn
            (bind-key "<up>"     'comint-previous-input comint-mode-map)
            (bind-key "<down>"   'comint-next-input comint-mode-map)
            (bind-key "C-<up>"   'windmove-up comint-mode-map)
            (bind-key "C-<down>" 'windmove-down comint-mode-map)))

(use-package dired
  :ensure gist
  :config (progn
            (setq dired-dwim-target t)
            (setq dired-recursive-deletes 'always)


            (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
            (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

            (defun dired-back-to-top ()
              (interactive)
              (beginning-of-buffer)
              (dired-next-line 4))

            (defun dired-jump-to-bottom ()
              (interactive)
              (end-of-buffer)
              (dired-next-line -1))

            (defun dired-zip-files (zip-file)
              "Create an archive containing the marked files."
              (interactive "sEnter name of zip file: ")
              ;; create the zip file
              (let ((zip-file (if (string-match ".zip$" zip-file) zip-file (concat zip-file ".zip"))))
                (shell-command
                 (concat "zip "
                         zip-file
                         " "
                         (concat-string-list
                          (mapcar
                           '(lambda (filename)
                              (file-name-nondirectory filename))
                           (dired-get-marked-files))))))

              (revert-buffer))

            (defun concat-string-list (list)
              "Return a string which is a concatenation of all elements of the list separated by spaces"
              (mapconcat '(lambda (obj) (format "%s" obj)) list " "))

            (bind-key "z" 'dired-zip-files dired-mode-map)))

;; eldoc-eval
(use-package eldoc-eval)

;; expand-region
(use-package expand-region
  :bind ("C-q" . er/expand-region))

;; tramp
(defun sudo-tramp-current-file()
  (interactive)
  (when buffer-file-name
    (let ((pos (point)))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
      (goto-char pos))))

(use-package tramp
  :config (progn
            (setq tramp-default-method "ssh")
            (setq tramp-auto-save-directory (concat emacs-tmp-dir "/backups"))))

;; framemove
(use-package framemove
  :config (progn
            (windmove-default-keybindings)
            (setq framemove-hook-into-windmove t)))

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

(use-package geben
  :defer t
  :config (progn
            (defadvice geben-display-context(before clear-windows-for-vars activate)
              (delete-other-windows))

            (add-hook 'kill-emacs-hook 'geben-safely-end-proxy)))

;; guide key
(use-package guide-key
  :init (progn
          (setq guide-key/guide-key-sequence '("C-x r" "C-x n" "C-c" "C-c s"))
          (guide-key-mode 1)))

;; ido
(use-package ido
  :config (progn
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

            (fset 'read-directory-name 'ido-read-directory-name)

            (setq confirm-nonexistent-file-or-buffer nil)))

(use-package ido-ubiquitous
  :init (ido-ubiquitous-mode 1))

;; javascript
(add-to-list 'auto-mode-alist '("\\.conkerorrc\\'" . js2-mode))

;; jedi
(use-package jedi
  :defer t
  :bind (("C-c d" . jedi:show-doc)
         ("M-SPC" . jedi:complete)
         ("M-." . jedi:goto-definition))

  :init (progn
          (defun pp:custom-jedi-setup ()
            (jedi:setup)
            (jedi:ac-setup)))

  :config (progn
            (setq jedi:server-command
                  `("python" ,(concat jedi:source-dir "jediepcserver.py")))

            (setq jedi:setup-keys t
                  jedi:tooltip-method nil
                  jedi:get-in-function-call-delay 300
                  jedi:complete-on-dot t)))



;; key-chord
(use-package key-chord
  :init (key-chord-mode +1)
  :config (progn
            (key-chord-define-global "ww" 'ace-jump-word-mode)
            (key-chord-define-global "jj" 'ace-jump-char-mode)
            (key-chord-define-global "hh" '(lambda() (interactive) (insert "_")))
            (key-chord-define-global "uu" 'undo-tree-visualize)))

;; magit
(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(use-package magit
  :bind ("C-x s" . magit-status)
  :config (progn
            (defadvice magit-status (around magit-fullscreen activate)
              (window-configuration-to-register :magit-fullscreen)
              ad-do-it
              (delete-other-windows))

            (add-hook 'magit-mode-hook 'hl-line-mode)

            (bind-key "q" 'magit-quit-session magit-status-mode-map)))


;; occur
(defun multi-occur-in-all-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))

(use-package occur
  :bind (("C-c o" . occur)
         ("C-c O" . multi-occur-in-matching-buffers)))

;; prodigy
(use-package prodigy
  :config (progn
            (prodigy-define-service
              :name "MAT Web"
              :command "MATWeb"
              :args '()
              :cwd "/media/ronon/research/MIST_2_0/src/MAT"
              :path "/media/ronon/research/MIST_2_0/src/MAT/bin"
              :tags '(research)
              :kill-signal 'sigkill
              :kill-process-buffer-on-stop t)

            (prodigy-define-service
              :name "Research Notebook"
              :command "ipython"
              :args '("notebook" "--pylab=inline" "--no-browser")
              :cwd "/media/ronon/research"
              :tags '(research)
              :kill-signal 'sigkill
              :kill-process-buffer-on-stop t)))

;; undo-tree
(use-package undo-tree
  :init (global-undo-tree-mode))

;; saveplace
(use-package saveplace
  :init (progn
          (setq-default save-place t)
          (setq save-place-file (expand-file-name ".saveplace" emacs-tmp-dir))))

;; yasnippet
(use-package yasnippet
  :config (progn
            (yas-global-mode 1)
            (setq yas-trigger-key "TAB")

            (setq yas-snippet-dirs
                  '("~/.emacs.d/etc/snippets"))

            (yas/reload-all)))

;; smart-tab
(use-package smart-tab
  :init (global-smart-tab-mode 1)
  :config (progn
            (setq smart-tab-using-hippie-expand t)

            (setq hippie-expand-try-functions-list
                  '(yas/hippie-try-expand
                    try-complete-file-name-partially
                    try-expand-all-abbrevs
                    try-expand-dabbrev
                    try-expand-dabbrev-all-buffers
                    try-expand-dabbrev-from-kill))))

;; smex
(use-package smex
  :bind ("M-x" . smex))

(use-package php-mode
  :ensure php-eldoc
  :config (progn
            (require 'php-eldoc)
            (bind-key "C-c C-f" 'php-search-local-documentation php-mode-map)
            (bind-key "<backtab>" 'php-complete-function php-mode-map)))

;; web-mode
(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode))
  :config (progn
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
                     ("velocity"   . "\\.\\(vsl\\|vtl\\|vm\\)\\'")))))

;; c-mode
(use-package cc-mode
  :config (progn
            (require 'auto-complete)
            (require 'c-eldoc)
            (require 'flycheck)
            (require 'google-c-style)
            (require 'irony)
            (require 'xcscope)

            (cscope-minor-mode)

            (yas/minor-mode-on)
            (auto-complete-mode 1)

            ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: php-mode
            (when (member major-mode irony-known-modes)
              (irony-mode 1))

            (global-flycheck-mode t)

            (bind-key "C-c C-c" 'comment-or-uncomment-line-or-region c-mode-map)))

(use-package c-eldoc
  :config (progn
            (setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")

            (add-hook 'irony-mode-hook 'c-turn-on-eldoc-mode)))

(use-package google-c-style
  :config (progn
            (add-hook 'c-mode-common-hook 'google-set-c-style)
            (add-hook 'c-mode-common-hook 'google-make-newline-indent)))

(add-hook 'nxml-mode-hook (lambda(&rest args)
                            (require 'i2b2-mode)
                            (if (i2b2-buffer-p)
                                (i2b2-mode 1))))

(add-hook 'i2b2-mode-hook 'whitespace-mode)

(use-package irony
  :ensure auto-complete
  :config (irony-enable 'ac))

;; emacs-lisp-mode
(use-package lisp-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
          (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
          (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-c SPC" . set-rectangular-region-anchor)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; org-mode
(use-package org
  :defer t
  :ensure org-bullets
  :config (progn
            (setq
             org-agenda-files '("/home/dan/files/notes.org")
             org-show-siblings '((default . nil)
                                 (isearch t)
                                 (bookmark-jump t)
                                 (agenda t))
             org-default-notes-file "/home/dan/files/notes.org"
             org-refile-use-outline-path 'file
             org-completion-use-ido t
             org-outline-path-complete-in-steps nil
             org-refile-allow-creating-parent-nodes 'confirm)

            (toggle-truncate-lines -1)

            (add-hook 'org-mode-hook 'org-bullets-mode)))

(use-package org-agenda
  :bind ("C-c A" . org-todo-list))

(use-package org-capture
  :bind ("C-c c" . org-capture))

;; python
(use-package python
  :commands python-mode
  :config (progn
            (setq pylint:epylint-executable "epylint"
                  python-shell-interpreter "ipython"
                  python-shell-interpreter-args "-i"
                  python-shell-buffer-name "Python"
                  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
                  python-shell-prompt-block-regexp ":"
                  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")

                                        ; auto pair
            (use-package autopair)

                                        ; adding hooks
            (add-hook 'python-mode-hook (lambda ()
                                          (unless (tramp-tramp-file-p (buffer-file-name))
                                            (flycheck-mode))))

                                        ; hooks
            (add-hook 'python-mode-hook 'auto-complete-mode)
            (add-hook 'python-mode-hook 'autopair-mode)
            (add-hook 'python-mode-hook 'pp:custom-jedi-setup)))

(add-hook 'inferior-python-mode-hook 'auto-complete-mode)
(add-hook 'inferior-python-mode-hook 'autopair-mode)
(add-hook 'inferior-python-mode-hook 'pp:custom-jedi-setup)

;; restclient
(use-package restclient
  :bind ("C-c R" . restclient-mode))

;; sql
(use-package sql
  :init (add-hook 'sql-mode-hook 'sql-highlight-mysql-keywords))

;; themes
(load-theme 'gruvbox)

;; uniquify
(use-package uniquify
  :init (progn
          (setq
           uniquify-buffer-name-style 'reverse
           uniquify-separator " - "
           uniquify-after-kill-buffer-p t
           uniquify-ignore-buffers-re "^\\*")))

;; webjump
(use-package webjump
  :bind ("C-x g" . webjump)
  :init (progn
          (setq webjump-sites
                '(("stackoverflow" . [simple-query "stack overflow" "http://stackoverflow.com/search?q=" ""])
                  ("google"        . [simple-query "google" "www.google.com/search?q=" ""])))))

;; winner-mode
(use-package winner
  :init (winner-mode t))

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

(setq completion-popup-frame-map (make-sparse-keymap))

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

(global-set-key "\C-c\C-i" '(lambda()
                              (interactive)
                              (if (buffer-file-name)
                                  (insert (buffer-file-name)))))

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

(setq org-capture-templates
      '(("o"
         "Optimization"
         entry
         (file+headline "~/files/notes.org" "Optimizations")
         "* %?\n [[file:%F]] %^g\n")))
