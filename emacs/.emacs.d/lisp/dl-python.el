(use-package anaconda-mode
  :ensure t
  :init (progn
          (add-hook 'python-mode-hook 'anaconda-mode)
          (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
  :bind (:map anaconda-mode-map
              ("M-." . anaconda-mode-find-definitions)
              ("M-," . anaconda-mode-go-back)
              ("M-*" . anaconda-mode-find-assignments)))

(use-package company-anaconda
  :ensure t
  :config (add-to-list 'company-backends 'company-anaconda))

(use-package pip-requirements
  :ensure t
  :after company)

(use-package sphinx-doc
  :ensure t
  :init (add-hook 'python-mode-hook 'sphinx-doc-mode))

(use-package pycoverage
  :ensure t
  :commands pycoverage-mode)

(provide 'dl-python)
