(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x l" . counsel-locate)
         ("C-x C-f" . counsel-find-file)
         ("C-h b" . counsel-descbinds)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)))

(use-package counsel-dash
  :ensure t)

(use-package counsel-projectile
  :ensure t
  :demand t
  :after (counsel projectile)
  :config (counsel-projectile-mode))

(use-package ivy
  :ensure t
  :bind ("C-c C-r" . ivy-resume)
  :config (progn
            (ivy-mode 1)

            (setq ivy-initial-inputs-alist nil
                  ivy-re-builders-alist '((t . ivy--regex-ignore-order))
                  ivy-use-virtual-buffers t
                  ivy-height 25
                  ivy-fixed-height-minibuffer t)))

(use-package ivy-rich
  :ensure t
  :after (ivy)
  :config (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

;; This is needed to order M-x by most recent commands
(use-package smex
  :ensure t)

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(provide 'dl-swiper)
