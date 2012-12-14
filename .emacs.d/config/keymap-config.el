(global-unset-key (kbd "C-z"))

(define-key global-map (kbd "C-b") 'revert-buffer)

(define-key global-map (kbd "M-g")         'goto-line)

(define-key global-map (kbd "C-c r")       'replace-string)

(define-key global-map (kbd "<f3>")      'start-kbd-macro)
(define-key global-map (kbd "ESC <f3>")  'end-kbd-macro)
(define-key global-map (kbd "<f4>")      'call-last-kbd-macro)

(global-set-key [(meta up)] 'windmove-up)
(global-set-key [(meta down)] 'windmove-down)
(global-set-key [(meta left)] 'windmove-left)
(global-set-key [(meta right)] 'windmove-right)


;; Wrap selected text in quotes, or just insert empty pair
(global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)

(define-key global-map (kbd "C-c SPC") 'ace-jump-char-mode)
(define-key global-map (kbd "C-z SPC") 'ace-jump-word-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(define-key global-map (kbd "C-c f") 'iy-go-to-char)
(define-key global-map (kbd "C-c F") 'iy-go-to-char-backward)

(global-set-key (kbd "M-,") 'mark-previous-like-this)
(global-set-key (kbd "M-.") 'mark-next-like-this)
(global-set-key (kbd "M-*") 'mark-all-like-this)

(if (fboundp 'yas/expand)
    (global-set-key (kbd "<tab>") 'yas/expand))

(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

; @ec config quoted-insert to something
(define-key global-map (kbd "C-q") 'er/expand-region)

(define-key global-map (kbd "C-c s") 'magit-status)

(define-key global-map (kbd "C-c c") 'compile-or-recompile)

(define-key global-map (kbd "C-c k") 'quick-copy-line)

(define-key global-map (kbd "C-c C-t i") 'timeclock-in)
(define-key global-map (kbd "C-c C-t o") 'timeclock-out)

(provide 'keymap-config)
