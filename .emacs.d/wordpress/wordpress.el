;;; wordpress.el --- Interacting with WordPress through emacs.

;;; Commentary:
;;

(require 'ido)
(require 'dash)
(require 's)
(require 'etags)
(require 'cc-mode)

(require 'wordpress-cli)
(require 'wordpress-ctags)
(require 'wordpress-eldoc)

;; spacebat

;;; Code:

(add-hook 'php-mode-hook 'wp-open-file)
                                        ;(add-hook 'php-mode-hook 'wp-eldoc-enable)
(add-hook 'after-save-hook 'wp-save-file)

;;;###autoload
(define-minor-mode wordpress-mode
  "WordPress Mode"
  :group 'wordpress
  :init-value nil
  :lighter " WordPress"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x w s") 'wp-search-codex)
            (define-key map (kbd "C-x w c") 'wp-cli-ido)
            (define-key map (kbd "C-x w d") 'wp-goto-declaration)
            (define-key map (kbd "C-x w u") 'wp-find-usages)
            map)
  (when wp-use-coding-style
    (setq indent-tabs-mode t
          fill-column 78
          tab-width 4
          c-indent-comments-syntactically-p t)
    (c-set-style "wordpress")))

;;;###autoload
(defun enable-wordpress-mode()
  (if (wp-exists)
      (wordpress-mode)))

(defcustom wp-use-coding-style nil
  "Whether or not to use WordPress' coding style.")

(defcustom wp-php-executable "/usr/bin/php"
  "Path to PHP for calling WordPress functions.")

(defconst wp-config-file "wp-config.php")
(defconst wp-codex-search-url "wordpress.org/search")

(defun wp-save-file()
  "Append any new tags from this file to the existing tags files."
  (when (wp-ctags)
    (wp--run-ctags (wp-ctags-destination "WPTAGS") (buffer-file-name) "generic" t)))

(defun wp-open-file()
  "Creates any tags file that doesn't exist on open of a php file within
the project.

Note that due to the 'when (wp-ctags)' stipulation, it will only run on PHP
files within a wordpress directory."
  (when (wp-ctags)
    (if (not (file-exists-p (wp-ctags-destination "WPTAGS")))
        (wp--run-ctags (wp-ctags-destination "WPTAGS") (wp-exists) "generic"))
    (visit-tags-table (wp-ctags-destination "WPTAGS"))))


(defun wp-exists()
  "Returns the absolute path for the WordPress installation, or `nil'."
  (let ((wp-dir (locate-dominating-file default-directory wp-config-file)))
    (if wp-dir
        (expand-file-name wp-dir))))

(defun wp-search-codex(beg end)
  "Searches the wp-codex using `wp-codex-search-url' along with the
selected region, or the symbol at point. "
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (null beg)
      (browse-url (format "%s/%s" wp-codex-search-url (symbol-name (symbol-at-point))))
    (browse-url (format "%s/%s" wp-codex-search-url (buffer-substring-no-properties beg end)))))

;; styling
(unless (-contains? (mapcar 'car c-style-alist) "wordpress")
  ;; the following code taken from ejmr's php-mode https://github.com/ejmr/php-mode
  ;; you should really be using it if you aren't :)
  (c-add-style
   "wordpress"
   '((c-basic-offset . 4)
     (c-offsets-alist . ((arglist-cont . 0)
                         (arglist-intro . php-lineup-arglist-intro)
                         (arglist-close . php-lineup-arglist-close)
                         (topmost-intro-cont . (first c-lineup-cascaded-calls
                                                      php-lineup-arglist-intro))
                         (brace-list-intro . +)
                         (brace-list-entry . c-lineup-cascaded-calls)
                         (case-label . 2)
                         (arglist-close . 0)
                         (defun-close . 0)
                         (defun-block-intro . +)
                         (knr-argdecl . [0])
                         (arglist-cont-nonempty . c-lineup-cascaded-calls)
                         (statement-cont . (first c-lineup-cascaded-calls +)))))))

(provide 'wordpress)

;;; wordpress.el ends here
