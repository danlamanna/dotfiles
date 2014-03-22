(require 'ido)
(require 'dash)
(require 's)
(require 'mag-menu)
(require 'etags)
(require 'etags-table)

(defcustom wp-php-executable "/usr/bin/php"
  "Path to PHP for calling WordPress functions.")

(defcustom wp-ctags-executable "/usr/local/bin/ctags"
  "Path to PHP for calling WordPress functions.")

(defcustom wp-cli-executable "/usr/bin/wp"
  "Path to PHP for calling WordPress functions.")

;; this actually needs to change per project
(defcustom wp-ctags-outfile "/home/dan/.emacs.d/wp_tags"
  "Path to ctags outfile.")

;; disabling these commands until we can find a decent way of integrating them
(setq wp-disabled-cli-commands '("cli" "help" "shell"))

(defconst wp-config-file "wp-config.php")
(defconst wp-codex-search-url "wordpress.org/search")

;(add-hook 'php-mode-hook 'wp-open-file)
(add-hook 'after-save-hook 'wp-save-file)

(defun wp-save-file()
  (when (and (wp-exists)
             wp-ctags-executable
             (file-exists-p wp-ctags-outfile))
    (wp-append-to-tags-table)))

(defun wp-open-file()
  (when (and (wp-exists)
             wp-ctags-executable
             (file-exists-p wp-ctags-outfile))
    (visit-tags-table wp-ctags-outfile)))

(defun wp-exists()
  "Returns the absolute path for the WordPress installation, or `nil'."
  (let ((wp-dir (locate-dominating-file default-directory wp-config-file)))
    (if wp-dir
        (expand-file-name wp-dir))))

(defun wp-build-tags-table()
  (when (and (wp-exists)
             wp-ctags-executable)
    (shell-command
     (format "%s --languages=PHP --PHP-kinds=-v -Ren -o %s %s"
             wp-ctags-executable
             wp-ctags-outfile
             (wp-exists)))
    (visit-tags-table wp-ctags-outfile t)))

(defun wp-append-to-tags-table()
  (when (and (wp-exists)
             wp-ctags-executable)
    (if (not (file-exists-p wp-ctags-outfile))
        (wp-build-tags-table)
      (shell-command
       (format "%s --languages=PHP --PHP-kinds=-v -Ren -a -o %s %s"
               wp-ctags-executable
               wp-ctags-outfile
               (buffer-file-name)))
      (visit-tags-table wp-ctags-outfile))))

(defun wp-search-codex(beg end)
  "Searches the wp-codex using `wp-codex-search-url' along with the
selected region, or the symbol at point. "
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (null beg)
      (browse-url (format "%s/%s" wp-codex-search-url (symbol-name (symbol-at-point))))
    (browse-url (format "%s/%s" wp-codex-search-url (buffer-substring-no-properties beg end)))))

(defun wp-cli-commands()
  (when (and (wp-exists)
             wp-cli-executable)
    (-filter (lambda(cmd-parts)
               (not (-contains? wp-disabled-cli-commands (car cmd-parts))))
             (mapcar (lambda(arg)
                       (let ((cmd-line (s-split " " arg t)))
                         `(,(first cmd-line) . ,(-remove-at 0 cmd-line))))
                     (s-split "\n" (shell-command-to-string (format "%s cli completions" wp-cli-executable)) t)))))

(defun wp-cli-subcommands(command)
  (when (and (wp-exists)
             wp-cli-executable)
    (-drop 1 (car (-filter (lambda(cmd-parts)
                             (string-equal (car cmd-parts) command)) (wp-cli-commands))))))


(defun wp-cli-ido()
  (interactive)
  (let ((command (ido-completing-read "WP CLI: " (mapcar (lambda(cmd-parts)
                                                           (car cmd-parts)) (wp-cli-commands)))))
    (if (not (wp-cli-subcommands command))
        (message (shell-command-to-string (format "%s %s %s" wp-cli-executable command (read-string command))))
      (let ((subcommand (ido-completing-read (format "%s " command) (wp-cli-subcommands command))))
        (message (shell-command-to-string (format "%s %s %s %s" wp-cli-executable command subcommand (read-string (format "%s %s " command subcommand)))))))))

(defun wp-goto-declaration()
  (interactive)
  (find-tag (symbol-name (symbol-at-point))))

(defun wp-find-usages()
  (interactive)
  (tags-search (symbol-name (symbol-at-point))))

(defvar wp-cli-cache-add-menu-group
  `(wp-cli-executable
    (man-page ,(if (null wp-cli-executable)
                   nil
                 (file-name-nondirectory (file-truename wp-cli-executable))))
    (actions
     ("a" "add" wp-cli-menu-cache-add-action))
    (switches)
    (arguments
     ("-k" "key" "" read-from-minibuffer)
     ("-v" "value" "" read-from-minibuffer)
     ("-g" "group" "" read-from-minibuffer)
     ("-e" "expiration" "" read-from-minibuffer))))


















  (provide 'wordpress)


                                        ;--regex-PHP=/do\_action\(\ '([a-zA-Z0-9\_]+)'/\1/a,actions/
                                        ;--regex-PHP=/apply\_filters\(\ '([a-zA-Z0-9\_]+)'/\1/h,hooks/
