(global-unset-key (kbd "C-z"))

(define-key global-map (kbd "C-c G") 'geben-single-or-proxy)

(define-key global-map (kbd "C-b") 'revert-buffer)
(define-key global-map (kbd "C-B") 'revert-all-buffers)

(define-key global-map (kbd "M-g")         'goto-line)

(define-key global-map (kbd "C-a") 'back-to-indentation-or-beginning)

(define-key global-map (kbd "C-c C-g") 'grep)

(define-key global-map (kbd "C-c r")       'replace-string)

(define-key global-map (kbd "<f3>")      'start-kbd-macro)
(define-key global-map (kbd "ESC <f3>")  'end-kbd-macro)
(define-key global-map (kbd "<f4>")      'call-last-kbd-macro)

(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)


;; Wrap selected text in quotes, or just insert empty pair
(global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)

(define-key global-map (kbd "C-c SPC") 'ace-jump-char-mode)
(define-key global-map (kbd "C-z SPC") 'ace-jump-word-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(define-key global-map (kbd "C-c f") 'iy-go-to-char)
(define-key global-map (kbd "C-c b") 'iy-go-to-char-backward)

(global-set-key (kbd "M-,") 'mark-previous-like-this)
(global-set-key (kbd "M-.") 'mark-next-like-this)
(global-set-key (kbd "M-*") 'mark-all-like-this)

(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

; @ec config quoted-insert to something
(define-key global-map (kbd "C-q") 'er/expand-region)

(define-key global-map (kbd "C-c R") 'restclient-mode)

(define-key global-map (kbd "C-c s") 'magit-status)

(define-key global-map (kbd "C-c c") 'compile-or-recompile)

(define-key global-map (kbd "C-c k") 'quick-copy-line)

(define-key global-map (kbd "C-c C-t i") 'timeclock-in)
(define-key global-map (kbd "C-c C-t o") 'timeclock-out)

(global-set-key (kbd "C-x g") 'webjump)

(provide 'keymap-config)
