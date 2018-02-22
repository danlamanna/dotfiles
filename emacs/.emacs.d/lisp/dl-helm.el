(use-package helm
  :ensure t
  :demand t
  :bind (("C-c h" . helm-command-prefix)
         ("C-c l" . helm-locate)
         ("C-c f" . helm-find)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("M-s o" . helm-occur))
  :init (progn
          (require 'helm-config)

          (custom-set-variables
           '(helm-M-x-fuzzy-match t)
           '(helm-buffers-fuzzy-matching t)
           '(helm-recent-fuzzy-match t)
           '(helm-split-window-in-side-p t)
           '(helm-ff-file-name-history-use-recentf t))

          (helm-mode))
  :config (progn
            (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)))

(use-package helm-ag
  :ensure t)

(use-package helm-dash
  :ensure t)

(use-package helm-descbinds
  :ensure t
  :bind (("C-h b" . helm-descbinds)))

(use-package helm-projectile
  :ensure t
  :config (progn
            (helm-projectile-on)

            ;; Use ag instead of grep, see:
            ;; https://emacs.stackexchange.com/questions/21197/how-can-i-map-helm-projectile-grep-to-helm-projectile-ag
            (define-advice helm-projectile-grep (:override (&optional dir) ag)
              (helm-do-ag (or dir (projectile-project-root))))))

(provide 'dl-helm)
