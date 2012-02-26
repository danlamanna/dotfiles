(defvar shared-lisp-dir (expand-file-name "~/.emacs.d/lib")
 "Directory containing Emacs libraries required for personal setup.")
(add-to-list 'load-path shared-lisp-dir)

;; Load Aesthetics 
(load "aesthetics/font-stuff.el")
(load "aesthetics/personal-terminal.el")

;; VC - Gists on github, VC for SVN 1.7
(load "gist-init.el")
(load "gist.el")
(load "vc-svn17.el")
(require 'vc-svn17)

;; Keymappings, bindings, etc
(load "keymaps/key-chord.el")
(load "keymaps/keymaps.el")
(load "iy-go-to-char.el")

; Mark multiple
(load "mark-multiple/mark-multiple.el")
(load "mark-multiple/inline-string-rectangle.el")
(load "mark-multiple/mark-more-like-this.el")

;; Jabber
(add-to-list 'load-path "~/.emacs.d/lib/jabber")
(load "jabber.el")
(load "gtalk.el")

;; Modes
(load "modes.el")

;; Snippets
(add-to-list 'load-path "~/.emacs.d/lib/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)

;; Miscellaneous
(load "magento.el")
(load "uniquify.el")
(load "misc.el")
(load "tramp.el")

;; If on work server, load the work-specific files.
(if (string-equal system-name "imladris.intellisites")
    ((add-to-list 'load-path "~/.emacs.d/lib/work-specific")
    (load "burst.el")
    (load "geben.el")))