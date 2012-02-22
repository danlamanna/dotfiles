;;; CSV Mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;;; PHP Mode
(add-to-list 'load-path (concat shared-lisp-dir "/php") t)
(autoload 'php-mode "php-mode" "Major mode for PHP files." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))

(setq php-completion-file (concat shared-lisp-dir "/php/php-builtins.txt"))
(add-hook 'php-mode-user-hook 'turn-on-font-lock)

;; Add line numbers to php mode
(add-hook 'abg-code-modes-hook
          (lambda () 
	    (flymake-mode 1)))

(add-hook 'php-mode-hook
          (lambda () (run-hooks 'abg-code-modes-hook)))

;;; YaSnippet (misplaced?)
;;(add-to-list 'load-path
  ;;            "~/.emacs.d/lib/yasnippet")
;;(require 'yasnippet) ;; not yasnippet-bundle
;;(yas/global-mode 1)