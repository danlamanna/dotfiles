(defvar shared-lisp-dir (expand-file-name "~/.emacs.d/lib"))

(add-to-list 'load-path shared-lisp-dir)
(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/lib/yasnippet")
(add-to-list 'load-path "~/.emacs.d/lib/expand-region")
(add-to-list 'load-path "~/.emacs.d/lib/multiple-cursors")
(add-to-list 'load-path "~/.emacs.d/lib/jabber")
(add-to-list 'load-path "~/.emacs.d/lib/zencoding")
(add-to-list 'load-path "~/.emacs.d/lib/magit")
(add-to-list 'load-path "~/.emacs.d/lib/modes/js2-mode")
(add-to-list 'load-path "~/.emacs.d/lib/modes/php-mode")
(add-to-list 'load-path "~/.emacs.d/lib/js2-refactor.el")
(add-to-list 'load-path "~/.emacs.d/lib/jquery-doc")
(load "~/.emacs.d/lib/nxhtml/autostart.el")

(add-to-list 'load-path "~/.emacs.d/lib/cedet-1.1.1/common")
(add-to-list 'load-path "~/.emacs.d/lib/auto-complete-1.3.1")


(require 'cedet)
(require 'gist)
(require 'magit)
(require 'magit-svn)
(require 'vc-svn17)
(require 'key-chord)
(require 'iy-go-to-char)
(require 'ace-jump-mode)
(require 'browse-kill-ring)
(require 'keymaps)
(require 'js2-refactor)
(require 'jquery-doc)
(require 'mark-multiple)
(require 'inline-string-rectangle)
(require 'mark-more-like-this)
(require 'jabber)
(require 'spotify)
(require 'twittering-mode)
(require 'modes)
(require 'restclient)
(require 'json-reformat)
(require 'expand-region)
(require 'multiple-cursors)
(require 'uniquify)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(require 'yasnippet)
(yas/global-mode 1)

(require 'aesthetics-config)
(require 'modeline-config)
(require 'saveplace-config)
(require 'geben-config)
(require 'uniquify-config)
(require 'ido-config)
(require 'misc-config)
(require 'twittering-mode-config)
(require 'workgroups)


;; takes a style tag, like "display:none;margin:5px;" and formats it into a css style


;; enable auto-revert mode for jabber emacs

(defadvice zap-to-char (after zap-until-char (arg char) activate)
  "Makes zap-to-char act like zap-until-char."
  (insert char)
  (backward-char 1))

(defun place-var-in-php-echo()
  (interactive)
  (replace-string (current-word) (format "<?php echo %s; ?>" (current-word)) t))
