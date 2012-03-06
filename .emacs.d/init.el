;; Load work specific files, if applicable
(if (equal (system-name) "imladris.intellisites")
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

;; VC - Gists on github, VC for SVN 1.7
(load "gist-init.el")
(load "gist.el")
(load "vc-svn17.el")
(require 'vc-svn17)

;; Keymappings, bindings, etc
(load "keymaps/key-chord.el")
(load "keymaps/keymaps.el")
(load "iy-go-to-char.el")
(load "anything.el")

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

;; Snippets
(add-to-list 'load-path "~/.emacs.d/lib/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)

;; Miscellaneous
(add-to-list 'load-path "~/.emacs.d/lib/expand-region")
(load "expand-region.el")
(load "modes/php/magento.el")
(load "uniquify.el")
(load "misc.el")
(load "tramp.el")