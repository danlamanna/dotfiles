(defvar emacs-config-dir (expand-file-name "~/.emacs.d"))
(defvar emacs-tmp-dir    (expand-file-name (concat emacs-config-dir "/" "tmp")))

(require 'org)
(require 'ob-tangle)

(org-babel-load-file (concat emacs-config-dir "/emacs.org"))
