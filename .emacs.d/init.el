(defvar shared-lisp-dir (expand-file-name "~/.emacs.d/lib")
"Directory containing Emacs libraries required for personal setup.")

; How else will I know the time
(display-time)

;; This really needs to happen now
(global-unset-key (kbd "C-x C-z"))

(load "~/.emacs.d/lib/keymaps.el")
(load "~/.emacs.d/lib/font-stuff.el")
(load "~/.emacs.d/lib/personal-terminal.el")
(load "~/.emacs.d/lib/gist.el")
(load "~/.emacs.d/lib/csv-mode.el")
;(load "~/.emacs.d/lib/burst.el")
(load "~/.emacs.d/lib/geben.el")
(load "~/.emacs.d/lib/gist-init.el")
(load "~/.emacs.d/lib/line-num.el")
(load "~/.emacs.d/lib/misc.el")
(load "~/.emacs.d/lib/modes.el")


(require 'tramp)
(setq tramp-default-method "scp")

(defun get-magento-file-from-class ()
  "Gets the proper Magento file from the class name."
  (interactive)
  (let (mageclass)
    (setq mageclass (thing-at-point 'symbol))
    (setq mageclass (replace-regexp-in-string "_" "/" mageclass))
    (message "%s.php" mageclass)
))