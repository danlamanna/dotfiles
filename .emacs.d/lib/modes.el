;;; CSV Mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;;; PHP Mode
(add-to-list 'load-path (concat shared-lisp-dir "/php") t)
(autoload 'php-mode "php-mode" "Major mode for PHP files." t)
;(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
;(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))

;;; YaSnippet (misplaced?)
(add-to-list 'load-path
              "~/.emacs.d/lib/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/global-mode 1)