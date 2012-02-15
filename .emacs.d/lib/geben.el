;; Enable geben
(autoload 'geben "geben" "PHP Debugger On Emacs" t)
(custom-set-variables
 '(geben-dbgp-feature-list (quote ((:set max_data 32768) (:set max_depth 1) (:set max_children 1024) (:get breakpoint_types geben-dbgp-breakpoint-store-types))))
 '(geben-dbgp-redirect-buffer-init-hook nil))

;; Exiting emacs without closing the proxy requires it to be restarted,
;; annoying, let's fix that.
(add-hook 'geben-mode-hook 'kill-proxy)
(defun kill-proxy()
    (add-hook 'kill-emacs-hook
        (lambda ()
                (interactive "P")               
                (call-interactively 'geben-proxy-end))))