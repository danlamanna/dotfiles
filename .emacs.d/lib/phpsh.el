(define-derived-mode phpsh-mode comint-mode "phpsh"
  "Major mode for interacting with phpsh with an inferior shell.")

(defun phpsh()
  (interactive)
  (require 'shell)
  (let ((explicit-shell-file-name "phpsh")
        (explicit-phpsh-args nil)
        (buf (get-buffer-create "*phpsh*")))
    (phpsh-mode)
    (shell buf)))
