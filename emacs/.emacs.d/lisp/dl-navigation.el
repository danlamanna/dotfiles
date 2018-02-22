(use-package avy-menu
  :ensure t
  :bind (("M-g c" . avy-goto-char)
         ("M-g w" . avy-goto-word-or-subword-1)
         ("M-g SPC" . avy-pop-mark)))

(use-package avy-zap
  :ensure t
  :bind (("M-z" . avy-zap-up-to-char-dwim)
         ("M-Z" . avy-zap-to-char-dwim)))

(use-package avy-flycheck
  :ensure t
  :bind ("C-c '" . avy-flycheck-goto-error))

(provide 'dl-navigation)
