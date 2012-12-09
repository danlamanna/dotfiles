; PHP
(custom-set-variables
 '(php-manual-path (expand-file-name (format "%s/etc/php-manual" emacs-config-dir)))
 '(php-completion-file (expand-file-name (format "%s/etc/php-completion.txt" emacs-config-dir))))

(autoload 'php-mode "php-mode" "Major mode for PHP." t)
(add-to-list 'auto-mode-alist '("\\.\\(php\\|phtml\\)\\'" . php-mode))

(require 'flymake)
(add-hook 'php-mode-hook 'flymake-mode-on)

(require 'wordpress-mode)
(add-hook 'php-mode-hook '(lambda()
			    (if (wp/exists)
				(wordpress-mode))))

(add-hook 'php-mode-hook '(lambda()
			    (define-key php-mode-map (kbd "C-c C-f") 'php-search-local-documentation)
			    (define-key php-mode-map (kbd "<backtab>") 'php-complete-function)))

; C
(defun compile-or-recompile()
  (interactive)
  (if (get-buffer "*compilation*")
      (recompile)
    (compile compile-command)))


(provide 'modes-config)
