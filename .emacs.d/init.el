(defvar shared-lisp-dir (expand-file-name "~/.emacs.d/lib")
  "Directory containing Emacs libraries required for personal setup.")

(load "~/.emacs.d/lib/keymaps.el")
(load "~/.emacs.d/lib/font-stuff.el")
(load "~/.emacs.d/lib/personal-terminal.el")
(load "~/.emacs.d/lib/gist.el")
(load "~/.emacs.d/lib/csv-mode.el")
(load "~/.emacs.d/lib/burst.el")
(load "~/.emacs.d/lib/geben.el")
(load "~/.emacs.d/lib/gist-init.el")
(load "~/.emacs.d/lib/line-num.el")
(load "~/.emacs.d/lib/misc.el")
(load "~/.emacs.d/lib/modes.el")
(load "~/.emacs.d/lib/magento.el")
(load "~/.emacs.d/lib/tramp.el")
(load "~/.emacs.d/lib/uniquify.el")
(load "~/.emacs.d/lib/gtalk.el")

;; SVN 1.7 doesn't work in emacs 23 - Need to use lib
(load "~/.emacs.d/lib/vc-svn17.el")
(require 'vc-svn17)

(add-to-list 'load-path "~/.emacs.d/lib/jabber/")
(load "~/.emacs.d/lib/jabber/jabber.el")

(setq read-file-name-completion-ignore-case nil)