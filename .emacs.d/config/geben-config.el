(require 'geben)

(custom-set-variables
 '(geben-dbgp-feature-list (quote ((:set max_data 32768) (:set max_depth 1) (:set max_children 1024) (:get breakpoint_types geben-dbgp-breakpoint-store-types))))
 '(geben-dbgp-redirect-buffer-init-hook nil)
 '(geben-temporary-file-directory "~/.emacs.d/tmp/geben"))


(defun kill-proxy()
  "Adds a hook to call geben-proxy-end before killing emacs when in geben mode."
  (add-hook 'kill-emacs-hook
	    (lambda()
	      (interactive "P")
	      (call-interactively 'geben-proxy-end))))

(add-hook 'geben-mode-hook 'kill-proxy)

(provide 'geben-config)
