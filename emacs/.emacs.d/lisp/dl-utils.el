;; Taken from http://www.draketo.de/proj/babcore/babcore.el
(defun x-urgency-hint (frame arg &optional source)
  "Set the x-urgency hint for the frame to arg:
- If arg is nil, unset the urgency.
- If arg is any other value, set the urgency.
If you unset the urgency, you still have to visit the frame to make the urgency setting disappear (at least in KDE)."
    (let* ((wm-hints (append (x-window-property
                "WM_HINTS" frame "WM_HINTS" source nil t) nil))
     (flags (car wm-hints)))
    (setcar wm-hints
        (if arg
        (logior flags #x100)
          (logand flags (lognot #x100))))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(provide 'dl-utils)
