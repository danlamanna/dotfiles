(defvar emacs-config-dir (expand-file-name "~/.emacs.d"))
(defvar emacs-tmp-dir    (expand-file-name (concat emacs-config-dir "/" "tmp")))

(let ((default-directory emacs-config-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(autoload 'assembla "assembla-mode" t)
(autoload 'ace-jump-word-mode "ace-jump-mode" t)
(autoload 'ace-jump-char-mode "ace-jump-mode" t)
(autoload 'er/expand-region "expand-region" t)
(autoload 'inline-string-rectangle "inline-string-rectangle" t)
(autoload 'iy-go-to-char "iy-go-to-char" t)
(autoload 'iy-go-to-char-backward "iy-go-to-char" t)
(autoload 'magit-status "magit" t)
(autoload 'magit-svn-mode "magit-svn" t)
(autoload 'mark-more-like-this "mark-more-like-this" t)
(autoload 'mark-next-like-this "mark-more-like-this" t)
(autoload 'restclient-mode "restclient" t)
(autoload 'rinari-minor-mode "rinari" t)
(autoload 'wordpress-mode "wordpress-mode" t)

(require 'assembla-mode)
(require 'theme-config)
(require 'emacs-config)
(require 'geben-config)
(require 'ido-config)
(require 'keymap-config)
(require 'modeline-config)
(require 'modes-config)
(require 'uniquify-config)
(require 'yasnippet-config)

(custom-set-variables
 '(asl/cache-enabled t)
 '(browse-url-browser-function (quote w3m-browse-url))
 '(geben-dbgp-feature-list (quote ((:set max_data 32768) (:set max_depth 1) (:set max_children 1024) (:get breakpoint_types geben-dbgp-breakpoint-store-types))))
 '(geben-dbgp-redirect-buffer-init-hook nil)
 '(geben-temporary-file-directory "~/.emacs.d/tmp/geben")
 '(indent-tabs-mode nil)
 '(php-completion-file (expand-file-name (format "%s/etc/php-completion.txt" emacs-config-dir)))
 '(php-manual-path (expand-file-name (format "%s/etc/php-manual" emacs-config-dir))))

(custom-set-faces
 '(flymake-errline ((t (:background "brightblack"))))
 '(magit-item-highlight ((t (:inherit default)))))
