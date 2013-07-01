
(defvar emacs-config-dir (expand-file-name "~/.emacs.d"))
(defvar emacs-tmp-dir    (expand-file-name (concat emacs-config-dir "/" "tmp")))

(let ((default-directory emacs-config-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq default-directory "~")

(setq org-src-fontify-natively t)

(load (concat emacs-config-dir "/lib/magit/magit.el"))

(autoload 'assembla "assembla-mode" t)
(autoload 'ace-jump-word-mode "ace-jump-mode" t)
(autoload 'ace-jump-char-mode "ace-jump-mode" t)
(autoload 'er/expand-region "expand-region" t)
(autoload 'inline-string-rectangle "inline-string-rectangle" t)
(autoload 'iy-go-to-char "iy-go-to-char" t)
(autoload 'iy-go-to-char-backward "iy-go-to-char" t)
(autoload 'geben "geben" t)
(autoload 'geben-single-or-proxy "geben-config" t)
(autoload 'shell "shell-config" t)
(autoload 'magit-status "magit" t)
(autoload 'magit-svn-mode "magit-svn" t)
(autoload 'mark-more-like-this "mark-more-like-this" t)
(autoload 'mark-next-like-this "mark-more-like-this" t)
(autoload 'restclient-mode "restclient" t)
(autoload 'rinari-minor-mode "rinari" t)
(autoload 'wordpress-mode "wordpress-mode" t)

(require 'coding-standards)
(require 'ido)

(if (member (expand-file-name "~/.emacs.d/lib/burst") load-path)
    (require 'burst))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(asl/cache-enabled t)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-browser-function 'browse-url-chromium)
 '(dired-listing-switches "-alh")
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#emacs"))))
 '(erc-hide-list (quote ("JOIN" "QUIT")))
 '(grep-command "grep -rnh -e ")
 '(indent-tabs-mode nil)
 '(php-completion-file (expand-file-name (format "%s/etc/php-completion.txt" emacs-config-dir)))
 '(php-manual-path (expand-file-name (format "%s/etc/php-manual" emacs-config-dir)))
 '(php-template-compatibility nil)
 '(vc-follow-symlinks t)
 '(virtualenv-root "~/.envs/"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:background "brightblack"))))
 '(magit-item-highlight ((t (:inherit default)))))

(put 'upcase-region 'disabled nil)

(prefer-coding-system 'utf-8)
(put 'narrow-to-region 'disabled nil)

(setq org-todo-keywords
          '((sequence "TODO" "SUM-2013" "FALL-2013" "WIN-2013" "SPR-2014" "SUM-2014" "FALL-2014" "|" "DONE")))

;; Workaround the annoying warnings:
;;    Warning (mumamo-per-buffer-local-vars):
;;    Already 'permanent-local t: buffer-file-name
(when (and (>= emacs-major-version 24)
           (>= emacs-minor-version 2))
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars))))

(defun truncate-file-contents(filename)
  (with-temp-buffer
    (write-file filename)))

(defun dired-do-truncate()
  (interactive)
  (mapc 'truncate-file-contents (dired-get-marked-files)))

(add-hook 'dired-mode-hook (lambda()
                             (hl-line-mode)))

(require 'tramp)

(setq tramp-default-method "ssh")

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(defun sudo-tramp-current-file()
  (interactive)
  (when buffer-file-name
    (let ((pos (point)))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
      (goto-char pos))))

(custom-set-variables
 '(geben-dbgp-default-proxy '("127.0.0.1" 9001 "dan" nil t))
 '(geben-dbgp-feature-list (quote ((:set max_data 32768) (:set max_depth 1) (:set max_children 1024) (:get breakpoint_types geben-dbgp-breakpoint-store-types))))
 '(geben-dbgp-redirect-buffer-init-hook nil)
 '(geben-temporary-file-directory "~/.emacs.d/tmp/geben"))

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

(add-hook 'kill-emacs-hook 'geben-safely-end-proxy)

(defadvice geben-display-context(before clear-windows-for-vars activate)
  (delete-other-windows))

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

(global-unset-key (kbd "C-z"))

(define-key global-map (kbd "C-c G") 'geben-single-or-proxy)

(define-key global-map (kbd "C-b") 'revert-buffer)
(define-key global-map (kbd "C-B") 'revert-all-buffers)

(define-key global-map (kbd "M-g")         'goto-line)

(define-key global-map (kbd "C-a") 'back-to-indentation-or-beginning)

(define-key global-map (kbd "C-c C-g") 'grep)

(define-key global-map (kbd "C-c r")       'replace-string)

(define-key global-map (kbd "<f3>")      'start-kbd-macro)
(define-key global-map (kbd "ESC <f3>")  'end-kbd-macro)
(define-key global-map (kbd "<f4>")      'call-last-kbd-macro)

(add-hook 'comint-mode-hook
          (lambda()
            (define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
            (define-key comint-mode-map (kbd "<down>") 'comint-next-input)
            (define-key comint-mode-map (kbd "C-<up>") 'windmove-up)
            (define-key comint-mode-map (kbd "C-<down>") 'windmove-down)))

(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)


;; Wrap selected text in quotes, or just insert empty pair
(global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)

(define-key global-map (kbd "C-c SPC") 'ace-jump-char-mode)
(define-key global-map (kbd "C-z SPC") 'ace-jump-word-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(define-key global-map (kbd "C-c f") 'iy-go-to-char)
(define-key global-map (kbd "C-c b") 'iy-go-to-char-backward)

(global-set-key (kbd "M-,") 'mark-previous-like-this)
(global-set-key (kbd "M-.") 'mark-next-like-this)
(global-set-key (kbd "M-*") 'mark-all-like-this)

(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

; @ec config quoted-insert to something
(define-key global-map (kbd "C-q") 'er/expand-region)

(define-key global-map (kbd "C-c R") 'restclient-mode)

(define-key global-map (kbd "C-c s") 'magit-status)

(define-key global-map (kbd "C-c c") 'compile-or-recompile)

(define-key global-map (kbd "C-c k") 'quick-copy-line)

(define-key global-map (kbd "C-c C-s") 'shell)

(define-key global-map (kbd "C-c C-t i") 'timeclock-in)
(define-key global-map (kbd "C-c C-t o") 'timeclock-out)

(global-set-key (kbd "C-x g") 'webjump)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

;; Hooks
(add-hook 'magit-mode-hook (lambda()
                             (require 'magit-svn)
                             (magit-key-mode-insert-action 'svn "x" "Fetch Externals" 'magit-svn-fetch-externals)
                             (if (magit-svn-get-ref-info)
                                 (magit-svn-mode))))

(add-hook 'magit-mode-hook 'hl-line-mode)

;; Keymaps
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(defvar magit-svn-externals-dir ".git_externals")

(defun magit-svn-fetch-externals()
  "Loops through all external repos found by `magit-svn-get-externals'
   and runs git svn fetch, and git svn rebase on each of them."
  (interactive)
  (let ((externals (magit-svn-get-externals)))
    (if (not externals)
        (message "No SVN Externals found. Check magit-svn-externals-dir.")
      (dolist (external externals)
        (let ((default-directory (file-name-directory external)))
          (magit-run-git "svn" "fetch")
          (magit-run-git "svn" "rebase")))
      (magit-refresh))))

(defun magit-svn-get-externals()
  (let* ((topdir (magit-get-top-dir "."))
         (default-directory (concat topdir magit-svn-externals-dir))
         (find (find-cmd '(and (name ".git")
                               (type "d")))))
    (when (file-directory-p default-directory)
      (remove "" (split-string (shell-command-to-string find) "\n")))))

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

(require 'winner)
(winner-mode t)

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat emacs-tmp-dir "/backups")))))

;; Make tramp autosaves save locally, saves time.
(setq tramp-auto-save-directory (concat emacs-tmp-dir "/backups"))

(defun kill-emacs-no-prompt()
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(global-set-key (kbd "C-x C-c") 'kill-emacs-no-prompt)

(defun make-files-directory-if-not-exists()
  "Makes the directory of the file referenced in `buffer-file-name',
   so we can 'open' files in non-existent directories, and this can
   create the directory. `before-save-hook' ftw."
  (interactive)
  (if (and (buffer-file-name)
           (not (file-exists-p (file-name-directory (buffer-file-name)))))
      (make-directory (file-name-directory buffer-file-name) t)))

(add-hook 'before-save-hook 'make-files-directory-if-not-exists)

(defadvice zap-to-char (after zap-until-char (arg char) activate)
  "Makes zap-to-char act like zap-until-char."
  (insert char)
  (backward-char 1))

;; Remove prompt of killing a buffer with a running process
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(defun swap-windows()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))


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
          (ignore-errors (make-directory new-name t))
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

(defun generate-rand-string(&optional char-set &optional len)
  "Generates a random string and inserts it at `point'. With no
   arguments, it conforms to an MD5 hashes pattern.

   CHAR-SET can be specified as a string with characters to be used,
   by default its set to 0-9a-z.

   LEN can be passed to specify how many characters it should insert,
   defaults at 32."
  (interactive)
  (let ((char-set (or char-set
                      "1234567890abcdefghijklmnopqrstyvwxyz")))
    (dotimes (i (or len 32))
      (insert (elt char-set (random (length char-set)))))))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun open-line-below ()
  (interactive)
  (if (eolp)
      (newline)
    (end-of-line)
    (newline))
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(fset 'yes-or-no-p 'y-or-n-p)

(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".saveplace" emacs-tmp-dir))

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
           ;; Major mode in brackets
           " [%m]"
           ;'(:eval (propertize (cdr (get-current-project buffer-file-name)) 'face 'bold))
           ;; Display time, followed by dashes till the end
            '(:eval (propertize (format-time-string "%l:%M%p")))
            " %-"))

(custom-set-variables
 '(php-manual-path (expand-file-name (format "%s/etc/php-manual" emacs-config-dir)))
 '(php-completion-file (expand-file-name (format "%s/etc/php-completion.txt" emacs-config-dir))))

(require 'php-eldoc)

(add-hook 'php-mode-hook '(lambda()
                            (require 'wordpress-mode)
                            (if (wp/exists)
                                (wordpress-mode))))

(add-hook 'php-mode-hook '(lambda()
                            (define-key php-mode-map (kbd "C-c C-f") 'php-search-local-documentation)
                            (define-key php-mode-map (kbd "<backtab>") 'php-complete-function)))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))

(autoload 'django-html-mumamo-mode "~/.emacs.d/lib/nxhtml/autostart.el")
(setq auto-mode-alist
      (append '(("\\.djhtml?$" . django-html-mumamo-mode)) auto-mode-alist))
(setq mumamo-background-colors nil)
(add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mumamo-mode))

(add-hook 'python-mode-hook '(lambda()
                               (elpy-mode)
                               (define-key elpy-mode-map (kbd "<M-down>") 'elpy-forward-definition)
                               (define-key elpy-mode-map (kbd "<M-up>") 'elpy-backward-definition)))

(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

(defun compile-or-recompile()
  (interactive)
  (if (get-buffer "*compilation*")
      (recompile)
    (compile compile-command)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(require 'package)

(add-to-list 'package-archives
;    '("marmalade" . "http://marmalade-repo.org/packages/")
    '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(defun run-mysql()
  (interactive)
  (let ((sql-scratch-buf (get-buffer-create "*sql-scratch*"))
        (sql-buf         (sql-mysql "*mysql*")))
    (with-current-buffer sql-scratch-buf
      (sql-mode)
      (sql-highlight-mysql-keywords))))

(define-key global-map (kbd "C-c m") 'run-mysql)

(require 'color-theme)
(require 'color-theme-almost-monokai)

(color-theme-almost-monokai)

(require 'uniquify)

(setq
 uniquify-buffer-name-style 'reverse
 uniquify-separator " - "
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*")

(require 'webjump)

(add-to-list 'webjump-sites
             '("Stack Overflow" .
               [simple-query "stackoverflow.com"
                             "http://stackoverflow.com/search?q="
                             ""]))

(require 'yasnippet)

(yas-global-mode 1)
(setq yas-trigger-key "TAB")

(setq yas-snippet-dirs
      '("~/.emacs.d/etc/snippets"))

(yas/reload-all)
