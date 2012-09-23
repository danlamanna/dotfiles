(setq default-mode-line-format
	  (list
	   "-- "
	   ;; Displays buffer name bolded
	   '(:eval (propertize "%b" 'face 'bold 'help-echo (buffer-name)))
	   ;; Displays ** bolded if the file has been modified (and it's not a readonly buffer)
	   '(:eval (when (and (buffer-modified-p) (eq buffer-read-only nil) (not (eq (buffer-file-name) nil)))
		     (propertize "**" 'face 'bold)))
	   ;; Display percent from top, then line num, col num
	   '(:eval (if (not (eq (buffer-file-name) nil))
		       "  %p (L%l,C%c)"))
	   ;; Major mode in brackets
	   " [%m]"
	   ;'(:eval (propertize (cdr (get-current-project buffer-file-name)) 'face 'bold))
	   ;; Display time, followed by dashes till the end
	    '(:eval (propertize (format-time-string "%l:%M%p")))
	    " %-"))

(provide 'modeline-config)