(use-package lisp-mode
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)))

(provide 'dl-lisp)
