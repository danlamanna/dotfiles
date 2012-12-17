; PHP
(custom-set-variables
 '(php-manual-path (expand-file-name (format "%s/etc/php-manual" emacs-config-dir)))
 '(php-completion-file (expand-file-name (format "%s/etc/php-completion.txt" emacs-config-dir))))

(autoload 'php-mode "php-mode" "Major mode for PHP." t)
(add-to-list 'auto-mode-alist '("\\.\\(php\\|phtml\\)\\'" . php-mode))

(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

(require 'flymake)
(add-hook 'php-mode-hook 'flymake-mode-on)

(require 'align)
(add-to-list 'align-rules-list
             `(php-array-keys
               (regexp	. "\\(\\s-*\\)=")
               (justify	. nil)
               (repeat	. nil)
               (modes	. '(php-mode))
               (tab-stop)))

;(add-hook 'before-save-hook (lambda()
;                             (if (string-equal mode-name "PHP")
;                                 (align (point-min) (point-max)))))

(add-hook 'php-mode-hook '(lambda()
                            (if (wp/exists)
                                (wordpress-mode))))

(add-hook 'php-mode-hook '(lambda()
                            (define-key php-mode-map (kbd "C-c C-f") 'php-search-local-documentation)
                            (define-key php-mode-map (kbd "<backtab>") 'php-complete-function)))

(autoload 'django-html-mumamo-mode "~/.emacs.d/lib/nxhtml/autostart.el")
(setq auto-mode-alist
      (append '(("\\.djhtml?$" . django-html-mumamo-mode)) auto-mode-alist))
(setq mumamo-background-colors nil)
(add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mumamo-mode))

; C
(defun compile-or-recompile()
  (interactive)
  (if (get-buffer "*compilation*")
      (recompile)
    (compile compile-command)))

(provide 'modes-config)
