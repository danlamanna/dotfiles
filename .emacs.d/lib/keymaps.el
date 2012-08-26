(defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer) 1)))

(define-key global-map (kbd "C-A")      'back-to-indentation)
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))
(define-key global-map (kbd "M-g")         'goto-line)
(define-key global-map (kbd "M-'")         'expand-abbrev)
(define-key global-map (kbd "M-m")         'iy-go-to-char)
(define-key global-map (kbd "M-Q")         'unfill-paragraph)
(define-key global-map (kbd "C-c w")       'delete-trailing-whitespace)
(define-key global-map (kbd "C-c \\")      'toggle-truncate-lines)
(define-key global-map (kbd "C-c q")       'query-replace-string)
(define-key global-map (kbd "C-c M-q")     'query-replace-regexp)
(define-key global-map (kbd "C-c r")       'replace-string)
(define-key global-map (kbd "C-c M-r")     'replace-regexp)
(define-key global-map (kbd "C-c C-x C-f") 'find-file-at-point)
(define-key global-map (kbd "C-c l")       'linum-mode)
(define-key global-map (kbd "C-x g")       'uncomment-region)
(define-key global-map (kbd "C-c g")       'comment-region)

(define-key global-map (kbd "C-c C-p")      'switch-to-previous-buffer)
(define-key global-map (kbd "ESC <f1>")  'kill-buffer)
(define-key global-map (kbd "C-c C-b")      'buffer-menu)
(define-key global-map (kbd "<f3>")      'start-kbd-macro)
(define-key global-map (kbd "ESC <f3>")  'end-kbd-macro)
(define-key global-map (kbd "<f4>")      'call-last-kbd-macro)
(define-key global-map (kbd "<f5>")      'occur)
(define-key global-map (kbd "ESC <f5>")  'grep)
(define-key global-map (kbd "<f6>")      'string-rectangle)
(define-key global-map (kbd "<f7>")      'query-replace)
(define-key global-map (kbd "ESC <f7>")  'replace-string)

(define-key global-map (kbd "ESC <f8>")  'replace-regexp)
(global-set-key (kbd "<f8>") 'gist-region-or-buffer)

(global-set-key [(meta up)] 'windmove-up)
(global-set-key [(meta down)] 'windmove-down)
(global-set-key [(meta left)] 'windmove-left)
(global-set-key [(meta right)] 'windmove-right)

(global-set-key "\C-cs" 'shell)
(global-set-key "\C-cq" 'project-sql)

(define-key global-map (kbd "C-c C-s") 'ace-jump-mode)

(define-key global-map (kbd "C-q") 'er/expand-region)

;; Wrap selected text in quotes, or just insert empty pair
(global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)

(require 'iy-go-to-char)
(global-set-key (kbd "C-c C-f") 'iy-go-to-char)
(global-set-key (kbd "C-c C-b") 'iy-go-to-char-backward)

(global-set-key (kbd "C-c C-s") 'magit-status)

(provide 'keymaps)