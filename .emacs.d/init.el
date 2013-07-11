(defvar emacs-config-dir (expand-file-name "~/.emacs.d"))
(defvar emacs-tmp-dir    (expand-file-name (concat emacs-config-dir "/" "tmp")))

(add-to-list 'load-path (concat emacs-config-dir "/lib/org-8.0.5/lisp"))
(add-to-list 'load-path (concat emacs-config-dir "/lib/org-8.0.5/contrib/list"))

(require 'org)
(require 'ob-tangle)

(org-babel-load-file (concat emacs-config-dir "/emacs.org"))
