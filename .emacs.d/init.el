(defvar emacs-config-dir (expand-file-name "~/.emacs.d"))

(let ((default-directory emacs-config-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'ace-jump-mode)
(require 'color-theme)
(require 'expand-region)
(require 'geben)
(require 'inline-string-rectangle)
(require 'iy-go-to-char)
(require 'magit)
(require 'magit-svn)
(require 'mark-more-like-this)
(require 'restclient)
(require 'rinari)
(require 'theme-config)
(require 'uniquify)
(require 'w3m)
(require 'yasnippet)

(require 'emacs-config)
(require 'geben-config)
(require 'ido-config)
(require 'keymap-config)
(require 'modeline-config)
(require 'modes-config)
(require 'uniquify-config)
(require 'yasnippet-config)
