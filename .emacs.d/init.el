(defvar shared-lisp-dir (expand-file-name "~/.emacs.d/lib")
"Directory containing Emacs libraries required for personal setup.")

(add-to-list 'load-path shared-lisp-dir)
(add-to-list 'load-path "~/.emacs.d/lib/yasnippet")
(add-to-list 'load-path "~/.emacs.d/lib/expand-region")
(add-to-list 'load-path "~/.emacs.d/lib/jabber")
(add-to-list 'load-path "~/.emacs.d/lib/aesthetics/color-theme")

(setq tramp-default-method "scp")

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
(require 'expand-region)
(require 'uniquify)

(load "misc.el")


(require 'modeline-config)

(require 'browse-kill-ring)

(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

(require 'ido-config)