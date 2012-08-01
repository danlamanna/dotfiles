(setq default-mode-line-format
          (list
	   "-- "
	   ;; Displays buffer name bolded
	   '(:eval (propertize "%b" 'face 'bold 'help-echo (buffer-name)))
	   ;; Displays ** bolded if the file has been modified
           '(:eval (when (buffer-modified-p)
		     (propertize "**" 'face 'bold)))
	   ;; Display percent from top, then line num, col num
           "  %p (L%l,C%c)"
	   ;; Major mode in brackets
	   " [%m]"
	   ;; Display time, followed by dashes till the end
	    '(:eval (propertize (format-time-string "%l:%M%p")))
	    " %-"))

(provide 'modeline-config)