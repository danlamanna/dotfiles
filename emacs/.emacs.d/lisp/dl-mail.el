(use-package helm-notmuch
  :ensure t
  :after (helm notmuch))

(use-package hydra
  :ensure t
  :demand t
  :after (helm notmuch)
  :config (progn
            (defhydra hydra-mail (global-map "C-c m")
              "mail"
              ("s" helm-notmuch "search" :exit t)
              ("r" (notmuch-jump-search) "read" :exit t)
              ("d" (notmuch-search "tag:draft") "drafts" :exit t)
              ("c" compose-mail "compose" :exit t)
              ("o" make-frame-command "new-frame" :color red))))

(use-package message
  :config (progn
            (add-hook 'message-mode-hook 'flyspell-mode)

            (setq message-citation-line-format "On %a, %b %e, %Y at %R %p, %f wrote:\n"
                  message-citation-line-function 'message-insert-formatted-citation-line)))

(use-package notmuch
  :ensure t
  :demand t
  :bind (:map notmuch-search-mode-map
              ("g" . notmuch-refresh-this-buffer)
              :map notmuch-tree-mode-map
              ("g" . notmuch-refresh-this-buffer))
  :config (progn
            (setq notmuch-saved-searches
                  '((:name "inbox"
                           :query "tag:inbox and not tag:flagged and date:7d.."
                           :key "i"
                           :sort-order 'newest-first
                           :search-type 'tree)
                    (:name "unread"
                           :query "tag:unread and not tag:flagged and not tag:draft"
                           :key "u"
                           :sort-order 'newest-first
                           :search-type 'tree)
                    (:name "girder"
                           :query "(tag:girder or tag:girder-users or tag:girder-devel) and not tag:flagged"
                           :key "g"
                           :sort-order 'newest-first
                           :search-type 'tree)))
            (setq notmuch-search-oldest-first nil)

            ;; Signal i3blocks when tags are changed within emacs
            (defadvice notmuch-update-tags(after signal-i3blocks activate)
              (start-process "update-i3blocks" nil "pkill" "-RTMIN+1" "i3blocks"))))

(use-package org-notmuch
  :load-path "lisp/"
  :after (org notmuch))

(use-package sendmail
  :config (progn
            (setq send-mail-function 'sendmail-send-it)))

(provide 'dl-mail)
