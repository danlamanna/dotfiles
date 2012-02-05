;; Enable geben
(autoload 'geben "geben" "PHP Debugger On Emacs" t)
(custom-set-variables
 '(geben-dbgp-feature-list (quote ((:set max_data 32768) (:set max_depth 1) (:set max_children 1024) (:get breakpoint_types geben-dbgp-breakpoint-store-types))))
 '(geben-dbgp-redirect-buffer-init-hook nil))