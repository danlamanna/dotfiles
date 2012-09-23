(defvar shared-lisp-dir (expand-file-name "~/.emacs.d/lib"))

(add-to-list 'load-path shared-lisp-dir)
(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/lib/yasnippet")
(add-to-list 'load-path "~/.emacs.d/lib/expand-region")
(add-to-list 'load-path "~/.emacs.d/lib/jabber")
(add-to-list 'load-path "~/.emacs.d/lib/zencoding")
(add-to-list 'load-path "~/.emacs.d/lib/magit")
(add-to-list 'load-path "~/.emacs.d/lib/modes/js2-mode")
(add-to-list 'load-path "~/.emacs.d/lib/js2-refactor.el")
(add-to-list 'load-path "~/.emacs.d/lib/cedet-1.1.1/common")


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
(require 'uniquify)
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

(add-hook 'c-mode-hook (lambda()
			 (set (make-local-variable 'compile-command)
			      (concat "gcc -o " (substring (format "%s" (buffer-name)) 0 (- (length (buffer-name)) 2)) " " (buffer-name)))))

(global-set-key "\M-/" 'hippie-expand)

(add-hook 'before-save-hook 'whitespace-cleanup)

(defun add-paren-before-semicolon()
  "Adds a closing paren to the end of the current line, but before any semicolons, if present.
   Note: Also saves excursion, so the point doesn't change visibly at all during this."
  (interactive)
  (save-excursion
    (setq now-until-eol (substring (buffer-string) (point) (line-end-position)))
    (whitespace-cleanup)
    (move-end-of-line nil)
    (insert ")")))

(global-set-key "\C-\\" 'add-paren-before-semicolon)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(zencoding-preview-default nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


;http://thoughtbus.com/customer_support/mozilla-thunderbird-setup
;; only display line/col/distance in file buffers
;; set modeline to ensure hte buffer is a FILE buffer
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
