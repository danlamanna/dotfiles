;; PHP
(custom-set-variables
 '(php-manual-path (expand-file-name (format "%s/etc/php-manual" emacs-config-dir)))
 '(php-completion-file (expand-file-name (format "%s/etc/php-completion.txt" emacs-config-dir))))

(add-hook 'php-mode-hook '(lambda()
                            (require 'wordpress-mode)
                            (if (wp/exists)
                                (wordpress-mode))))

(add-hook 'php-mode-hook '(lambda()
                            (define-key php-mode-map (kbd "C-c C-f") 'php-search-local-documentation)
                            (define-key php-mode-map (kbd "<backtab>") 'php-complete-function)))

;; Python
(autoload 'django-html-mumamo-mode "~/.emacs.d/lib/nxhtml/autostart.el")
(setq auto-mode-alist
      (append '(("\\.djhtml?$" . django-html-mumamo-mode)) auto-mode-alist))
(setq mumamo-background-colors nil)
(add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mumamo-mode))

;; Ruby
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; C
(defun compile-or-recompile()
  (interactive)
  (if (get-buffer "*compilation*")
      (recompile)
    (compile compile-command)))

;; Common Lisp
(add-to-list 'auto-mode-alist '(".stumpwmrc$" . common-lisp-mode))

;; Emacs Lisp..
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'inferior-emacs-lisp-mode-hook 'eldoc-mode)

(provide 'modes-config)
