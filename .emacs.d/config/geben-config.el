(custom-set-variables
 '(geben-dbgp-feature-list (quote ((:set max_data 32768) (:set max_depth 1) (:set max_children 1024) (:get breakpoint_types geben-dbgp-breakpoint-store-types))))
 '(geben-dbgp-redirect-buffer-init-hook nil)
 '(geben-temporary-file-directory "~/.emacs.d/tmp/geben"))

;; Fixing complications that occur with alive (or dead) dbgp listeners
;(defadvice geben-proxy-end(around quietly-kill-proxy activate)
 ; (ignore-errors
  ;  ad-do-it))

;(add-hook 'kill-emacs-hook (lambda()
 ;                            (call-interactively 'geben-proxy-end)))

(provide 'geben-config)
