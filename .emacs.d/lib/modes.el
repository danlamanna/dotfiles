;;; CSV Mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;;; PHP Mode
(add-to-list 'load-path (concat shared-lisp-dir "/modes") t)
(autoload 'php-mode "php-mode" "Major mode for PHP files." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;;; JS2 Mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Mode-specific Hooks
(require 'jquery-doc)
(add-hook 'js2-mode-hook 'jquery-doc-setup)

(add-hook 'php-mode-hook '(lambda() 
			    (load "modes/php/php-completion.el")))