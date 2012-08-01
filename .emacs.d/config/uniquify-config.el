;; Gets rid of Filename<2> and specifies the first different
;; folder up. 
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " - ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(provide 'uniquify-config)