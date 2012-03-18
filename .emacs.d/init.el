;; Load work specific files, if applicable
(when (equal (system-name) "imladris.intellisites")
  (load "~/.emacs.d/lib/work-specific/custom-geben.el")
  (load "~/.emacs.d/lib/work-specific/burst.el"))

(defvar shared-lisp-dir (expand-file-name "~/.emacs.d/lib")
"Directory containing Emacs libraries required for personal setup.")
(add-to-list 'load-path shared-lisp-dir)

;; Load Aesthetics 
(add-to-list 'load-path "~/.emacs.d/lib/aesthetics/color-theme")
(load "color-theme.el")
(load "themes/color-theme-almost-monokai.el")
(color-theme-almost-monokai)

;;; VC - Gists on github, VC for SVN 1.7
(load "gist-init.el")
(load "gist.el")
(load "vc-svn17.el")
(require 'vc-svn17)

;; Keymappings, bindings, etc
(load "keymaps/key-chord.el")
(load "keymaps/keymaps.el")
(load "iy-go-to-char.el")
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c C-s") 'ace-jump-mode)

;; Mark multiple
(load "mark-multiple/mark-multiple.el")
(load "mark-multiple/inline-string-rectangle.el")
(load "mark-multiple/mark-more-like-this.el")

;; Jabber
(add-to-list 'load-path "~/.emacs.d/lib/jabber")
(load "jabber.el")
(load "gtalk.el")

;; Modes
;; Auto-Complete
(add-to-list 'load-path "~/.emacs.d/lib/auto-complete")
(load "auto-complete-config.el")
(load "auto-complete.el")
(add-to-list 'load-path "~/.emacs.d/lib/jquery-doc")
(load "modes.el")

;; RestClient
(add-to-list 'load-path "~/.emacs.d/lib/modes/restclient")
(require 'restclient)

;; Snippets
(add-to-list 'load-path "~/.emacs.d/lib/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)

;; Miscellaneous
(add-to-list 'load-path "~/.emacs.d/lib/expand-region")
(load "expand-region.el")
(load "uniquify.el")
(load "~/.emacs.d/lib/modes/php/magento/mage.el")
(load "misc.el")
(load "tramp.el")

;; Wrap selected text in quotes, or just insert empty pair
(global-set-key (kbd "M-'") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)

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