(defvar emacs-config-dir (expand-file-name "~/.emacs.d"))
(defvar emacs-tmp-dir    (expand-file-name (concat emacs-config-dir "/" "tmp")))

(let ((default-directory emacs-config-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

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

(eval-after-load "dired" '(require 'dired-config))
(eval-after-load "ido"   '(require 'ido-config))
(eval-after-load "magit" '(require 'magit-config))
(eval-after-load "magit-svn" '(require 'magit-svn-config))
(eval-after-load "package" '(require 'package-config))

(require 'coding-standards)
(require 'ido)
(require 'theme-config)
(require 'emacs-config)
(require 'keymap-config)
(require 'modeline-config)
(require 'modes-config)
(require 'uniquify-config)
(require 'yasnippet-config)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(asl/cache-enabled t)
 '(browse-url-browser-function (quote w3m-browse-url))
 '(erc-hide-list (quote ("JOIN" "QUIT")))
 '(indent-tabs-mode nil)
 '(php-completion-file (expand-file-name (format "%s/etc/php-completion.txt" emacs-config-dir)))
 '(php-manual-path (expand-file-name (format "%s/etc/php-manual" emacs-config-dir))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:background "brightblack"))))
 '(magit-item-highlight ((t (:inherit default)))))
(put 'upcase-region 'disabled nil)

(require 'shell)

(defun php-lint-this-file()
  (interactive)
  (shell-command (format "php -l %s" (buffer-file-name))))

(add-hook 'php-mode-hook '(lambda()
                            (define-key php-mode-map (kbd "C-c l") 'php-lint-this-file)))

(setq inferior-lisp-program "sbcl")
(require 'slime)
(require 'slime-autoloads)
(slime-setup '(slime-fancy))
