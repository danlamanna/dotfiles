(require 'yasnippet)

(yas-global-mode 1)
(setq yas-trigger-key "TAB")

(setq yas-snippet-dirs
      '("~/.emacs.d/etc/snippets"))

(yas/reload-all)

(provide 'yasnippet-config)
