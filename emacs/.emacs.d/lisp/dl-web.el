(use-package rainbow-mode
  :ensure t
  :pin gnu)

(use-package stylus-mode
  :ensure t
  :init (add-hook 'stylus-mode-hook 'rainbow-mode))

(use-package css-mode
  :init (add-hook 'css-mode-hook 'rainbow-mode))

(use-package web-mode
  :ensure t
  :init (add-hook 'web-mode-hook 'rainbow-mode))

(use-package highlight-indentation
  :ensure t
  :config (progn
            (custom-set-variables
             '(highlight-indentation-offset 2))))

(use-package jade-mode
  :ensure t
  :config (progn
            (add-hook 'jade-mode-hook 'highlight-indentation-mode)))

(use-package php-mode
  :ensure t)

(provide 'dl-web)
