; PHP
(autoload 'php-mode "php-mode" "Major mode for PHP." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . php-mode))

; C
(add-hook 'c-mode-hook (lambda()
			 (set (make-local-variable 'compile-command)
			      (concat "gcc -o " (substring (format "%s" (buffer-name)) 0 (- (length (buffer-name)) 2)) " " (buffer-name))
			      (concat "gcc -Wall -g -o " (substring (format "%s" (buffer-name)) 0 (- (length (buffer-name)) 2)) " " (buffer-name)))))


(provide 'modes-config)
