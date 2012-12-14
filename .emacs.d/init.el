(defvar emacs-config-dir (expand-file-name "~/.emacs.d"))
(defvar emacs-tmp-dir    (expand-file-name (concat emacs-config-dir "/" "tmp")))

(let ((default-directory emacs-config-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'ace-jump-mode)
(require 'assembla-mode)
(require 'color-theme)
(require 'expand-region)
;(require 'geben)
(require 'inline-string-rectangle)
(require 'iy-go-to-char)
(require 'magit)
(require 'magit-svn)
(require 'mark-more-like-this)
(require 'restclient)
(require 'rinari)
(require 'theme-config)
(require 'uniquify)
;(require 'w3m)
(require 'wordpress-mode)
(require 'yasnippet)

(require 'emacs-config)
;(require 'geben-config)
(require 'ido-config)
(require 'keymap-config)
(require 'modeline-config)
(require 'modes-config)
(require 'uniquify-config)
(require 'yasnippet-config)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(asl/cache-enabled t)
 '(browse-url-browser-function (quote w3m-browse-url))
 '(geben-dbgp-feature-list (quote ((:set max_data 32768) (:set max_depth 1) (:set max_children 1024) (:get breakpoint_types geben-dbgp-breakpoint-store-types))))
 '(geben-dbgp-redirect-buffer-init-hook nil)
 '(geben-temporary-file-directory "~/.emacs.d/tmp/geben")
 '(indent-tabs-mode nil)
 '(php-completion-file (expand-file-name (format "%s/etc/php-completion.txt" emacs-config-dir)))
 '(php-manual-path (expand-file-name (format "%s/etc/php-manual" emacs-config-dir))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:background "brightblack"))))
 '(magit-item-highlight ((t (:inherit default)))))
