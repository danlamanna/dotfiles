(defvar emacs-config-dir (expand-file-name "~/.emacs.d"))
(defvar emacs-tmp-dir    (expand-file-name (concat emacs-config-dir "/" "tmp")))

;; add all of the everythings to the load path, cause, performance.
(let ((default-directory emacs-config-dir))
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory "~/dotfiles/deps"))
  (normal-top-level-add-subdirs-to-load-path))

;; package.el
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(require 'package)
(package-initialize)

(defvar elpa-required-packages '(ace-jump-mode
                                 ack-and-a-half
                                 auto-complete
                                 autopair
                                 dash
                                 expand-region
                                 gist
                                 guide-key
                                 ido-ubiquitous
                                 key-chord
                                 magit
                                 multiple-cursors
                                 multi-term
                                 php-eldoc
                                 php-mode
                                 s
                                 saveplace
                                 smart-tab
                                 smex
                                 undo-tree
                                 web-mode
                                 yasnippet)
  "Default Packages")

(dolist (pkg elpa-required-packages)
  (if (not (package-installed-p pkg))
      (package-install pkg)))

;; must-have libraries/utilities
(require 's)
(require 'dash)

;; custom variables
(custom-set-variables
 '(asl/cache-enabled t)
 '(auto-save-interval 60)
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

(define-key global-map (kbd "C-c b") 'browse-url-at-point)

;; custom faces
(custom-set-faces
 '(flymake-errline ((t (:background "brightblack"))))
 '(magit-item-highlight ((t (:inherit default)))))

;; ace-jump-mode
(autoload 'ace-jump-word-mode "ace-jump-mode" t)
(autoload 'ace-jump-char-mode "ace-jump-mode" t)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; ack
(require 'ack-and-a-half)
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)

(setq ack-and-a-half-use-ido t)

(define-key global-map (kbd "C-c a") 'ack)

;; autopair
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

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

;; coding standards
;; both these lists should be lowercased
(setq no-cleanup-filenames '("makefile"))
(setq no-cleanup-extensions '("md" "org"))

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
(eval-after-load "comint"
  '(progn
     (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
     (define-key comint-mode-map (kbd "<down>") 'comint-next-input)
     (define-key comint-mode-map (kbd "C-<up>") 'windmove-up)
     (define-key comint-mode-map (kbd "C-<down>") 'windmove-down)))

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
     (require 'gist)
     (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
     (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)))

(setq wdired-allow-to-change-permissions t)

;; add zip functionality to dired
(eval-after-load "dired"
  '(define-key dired-mode-map "z" 'dired-zip-files))
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

;; framemove
(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

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
(setq guide-key/guide-key-sequence '("C-x r" "C-x n" "C-c"))
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

;; key-chord
(require 'key-chord)

(key-chord-define-global "ww" 'ace-jump-word-mode)
(key-chord-define-global "jj" 'ace-jump-char-mode)
(key-chord-define-global "hh" '(lambda() (interactive) (insert "_")))
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
        try-expand-dabbrev-from-kill))

(eval-after-load "completion-ui-sources"
  (global-set-key (kbd "<C-tab>") 'complete-etags))


;; smex
(global-set-key (kbd "M-x") 'smex)

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

;; python
(require 'python)
(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "--pylab")

                                        ;(require 'jedi)

                                        ;(add-hook 'python-mode-hook 'jedi:setup)
                                        ;(setq jedi:setup-keys t)                      ; optional
                                        ;(setq jedi:complete-on-dot t)                 ; optional

                                        ;(setq jedi:server-command
                                        ;     '("python" "/home/dan/dotfiles/.emacs.d/elpa/jedi-20140223.1054/jediepcserver.py"))

;; (defun pp:custom-jedi-setup ()
;;   (jedi:setup)
;; ;  (jedi:ac-setup)
;;                                         ; (custom-jedi-server-setup)
;;   (local-set-key "\C-cd" 'jedi:show-doc)
;;   (local-set-key (kbd "M-SPC") 'jedi:complete)
;;   (local-set-key (kbd "M-.") 'jedi:goto-definition))

;; (add-hook 'python-mode-hook 'pp:custom-jedi-setup)

;; ruby-mode
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; c-mode
(setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
(load "c-eldoc")
(add-hook 'irony-mode-hook 'c-turn-on-eldoc-mode)

(require 'auto-complete)
(require 'irony)
(irony-enable 'ac)

(defun c-hooks()
  "Enable the hooks in the preferred order: 'yas -> auto-complete -> irony'."
  (require 'google-c-style)
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)

  (yas/minor-mode-on)
  (auto-complete-mode 1)
  ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: php-mode
  (when (member major-mode irony-known-modes)
    (irony-mode 1)))

;; iedit and flycheck

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
(add-hook 'org-mode-hook (lambda ()
                           (require 'org-bullets)
                           (org-bullets-mode 1)
                           (toggle-truncate-lines -1)))

(setq org-agenda-files '("/home/dan/files/notes.org"))
(setq org-show-siblings '((default . nil) (isearch t) (bookmark-jump t) (agenda t)))

(setq org-default-notes-file "/home/dan/files/notes.org")
(setq
 org-refile-use-outline-path 'file
 org-completion-use-ido t
 org-outline-path-complete-in-steps nil
 org-refile-allow-creating-parent-nodes 'confirm)

(define-key global-map "\C-cc" 'org-capture)

;; global map
(define-key global-map (kbd "C-c A") 'org-todo-list)

;; restclient
(autoload 'restclient-mode "restclient" t)
(define-key global-map (kbd "C-c R") 'restclient-mode)

;; sensitive-mode http://anirudhsasikumar.net/blog/2005.01.21.html
(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Sensitive"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
      (progn
        ;; disable backups
        (set (make-local-variable 'backup-inhibited) t)
        ;; disable auto-save
        (if auto-save-default
            (auto-save-mode -1)))
                                        ;resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
                                        ;resort to default auto save setting
    (if auto-save-default
        (auto-save-mode 1))))

(setq auto-mode-alist (append '(("\\.gpg$" . sensitive-mode)) auto-mode-alist))

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


;; (defun first-n-lines(file lines)
;;   (with-temp-buffer
;;     (setq n-lines "")
;;     (setq read-bytes 0)
;;     (while (< (s-count-matches "\n" n-lines) lines)
;;       (goto-char (point-max))
;;       (insert-file-contents file nil read-bytes (+ read-bytes 1) t)
;;       (setq n-lines (concat n-lines (buffer-substring-no-properties (point-min) (point-max))))
;;       (setq read-bytes (+ read-bytes 1))))
;;   (butlast (split-string n-lines "\n") (- (length (split-string n-lines "\n")) lines)))

;; (setq test-file "/home/dan/.emacs.d/init.el")
;; (first-n-lines test-file 10)
;; ("(defvar emacs-config-dir (expand-file-name \"~/.emacs.d\"))"
;;  "(defvar emacs-tmp-dir    (expand-file-name (concat emacs-config-dir \"/\" \"tmp\")))"
;;  ""
;;  ";; add all of the everythings to the load path, cause, performance."
;;  "(let ((default-directory emacs-config-dir))"
;;  "  (normal-top-level-add-subdirs-to-load-path))"
;;  ""
;;  "(let ((default-directory \"~/dotfiles/deps\"))"
;;  "  (normal-top-level-add-subdirs-to-load-path))"
;;  "")



;; (defun dired-head-file()
;;   (interactive)
;;   (let ((file (dired-get-file-for-visit))
;;         (tmpfile (concat emacs-tmp-dir "/" (s-replace "/" "_" file))))
;;     (with-temp-file tmpfile
;;       (insert (s-join "\n" (first-n-lines file 10))))
;;     (view-buffer (find-file-noselect tmpfile) (lambda(buf)
;;                                                 (progn
;;                                                   (delete-file tmpfile)
;;                                                   (kill-buffer buf))))))


(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; hslint on the command line only likes this indentation mode;
;; alternatives commented out below.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(fset 'remove-xml-tags
      [?\C-s ?< return left ?\M-z ?> delete ?\C-s ?< return left ?\M-z ?> delete])

(add-hook 'php-mode-hook (lambda(&rest args)
                           (require 'wordpress)
                           (enable-wordpress-mode)))
