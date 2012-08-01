;; Load work specific files, if applicable
(when (equal (system-name) "imladris.intellisites")
  (load "~/.emacs.d/lib/work-specific/custom-geben.el")
  (load "~/.emacs.d/lib/work-specific/burst.el"))

(defvar shared-lisp-dir (expand-file-name "~/.emacs.d/lib")
"Directory containing Emacs libraries required for personal setup.")

(add-to-list 'load-path shared-lisp-dir)
(add-to-list 'load-path "~/.emacs.d/lib/yasnippet")
(add-to-list 'load-path "~/.emacs.d/lib/jabber")
(add-to-list 'load-path "~/.emacs.d/lib/aesthetics/color-theme")


;; Load Aesthetics
(require 'color-theme)
(load "~/.emacs.d/lib/aesthetics/color-theme/themes/color-theme-almost-monokai.el")
(color-theme-almost-monokai)

;;; VC - Gists on github, VC for SVN 1.7
(require 'gist)
(require 'vc-svn17)

;; Keymappings, bindings, etc
(require 'key-chord)
(require 'iy-go-to-char)
(require 'ace-jump-mode)
(require 'keymaps)

;; Mark multiple
(require 'mark-multiple)
(require 'inline-string-rectangle)
(require 'mark-more-like-this)

;; Jabber
(require 'jabber)

;; Modes
(require 'modes)
(require 'restclient)
(require 'json-reformat)

;; Snippets
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

;(load "browse-kill-ring.el")
;(when (require 'browse-kill-ring nil 'noerror)
  ;(browse-kill-ring-default-keybindings))

(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

(require 'ido) 
(ido-mode 'both) ; for buffers and files
(setq 
  ido-save-directory-list-file "~/.emacs.d/cache/ido.last"

  ido-ignore-buffers
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"

     "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
  ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
  ido-case-fold  t                 ; be case-insensitive

  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)
  ido-max-prospects 10              ; don't spam my minibuffer
  ido-confirm-unique-completion nil)

; when using ido, the confirmation is rather annoying...
 (setq confirm-nonexistent-file-or-buffer nil)