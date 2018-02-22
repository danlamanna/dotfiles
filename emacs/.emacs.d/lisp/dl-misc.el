;; related to built-ins
(fset 'yes-or-no-p 'y-or-n-p)

;; aesthetics related
;; use of fbound for cross-platform compat (x and nox)
(-map (lambda(f)
        (if (fboundp f)
            (funcall f -1))) '(menu-bar-mode tool-bar-mode scroll-bar-mode))

;; misc
(defun insert-quotations (&optional arg)
  "Enclose following ARG sexps in quotation marks.
Leave point after open-paren."
  (interactive "*P")
  (insert-pair arg ?\' ?\'))

(defun insert-quotes (&optional arg)
  "Enclose following ARG sexps in quotes.
Leave point after open-quote."
  (interactive "*P")
  (insert-pair arg ?\" ?\"))

(global-set-key "\M-'" 'insert-quotations)
(global-set-key "\M-\"" 'insert-quotes)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x w") 'delete-frame)
(global-set-key (kbd "C-c b c") 'quick-calc)

;; backspace starts the isearch over
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(provide 'dl-misc)
