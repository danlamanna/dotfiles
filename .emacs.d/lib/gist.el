;;; gist.el --- Emacs integration for gist.github.com

;; Author: Christian Neukirchen <purl.org/net/chneukirchen>
;; Maintainer: Chris Wanstrath <chris@ozmm.org>
;; Contributors:
;; Will Farrington <wcfarrington@gmail.com>
;; Michael Ivey
;; Phil Hagelberg
;; Dan McKinley
;; Version: 0.6.3
;; Created: 21 Jul 2008
;; Keywords: gist git github paste pastie pastebin

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Uses your local GitHub config if it can find it.
;; See http://github.com/blog/180-local-github-config

;; if you are using Emacs 22 or earlier, download the json.el from following url
;; http://bzr.savannah.gnu.org/lh/emacs/emacs-23/annotate/head:/lisp/json.el

;;; TODO;
;; * defgroup

;;; Code:

(eval-when-compile (require 'cl))
(require 'json)
(require 'url)

(defvar github-user nil
  "If non-nil, will be used as your GitHub username without checking
git-config(1).")
(defvar github-token nil
  "If non-nil, will be used as your GitHub token without checking
git-config(1).")

(defcustom gist-user-password nil
  "If non-nil, will be used as your GitHub password without reading."
  :type 'string)

(defcustom gist-view-gist nil
  "If non-nil, automatically use `browse-url' to view gists after they're
posted."
  :type 'boolean)

(defcustom gist-display-date-format "%Y-%m-%d %H:%M"
  "Date format displaying in `gist-list' buffer."
  :type 'string)

(defcustom gist-authenticate-function 'gist-basic-authentication
  "Authentication function symbol."
  :type 'function)

;; TODO http://developer.github.com/v3/oauth/
;; "Desktop Application Flow" says that using the basic authentication...
(defun gist-basic-authentication ()
  (destructuring-bind (user . pass) (github-auth-info-basic)
    (format "Basic %s"
            (base64-encode-string (format "%s:%s" user pass)))))

(defun gist-oauth2-authentication ()
  (let ((token (github-auth-info-oauth2)))
    (format "Bearer %s" token)))

(defun gist-request (method url callback &optional json)
  (let ((url-request-data (and json (concat (json-encode json) "\n")))
        (url-request-extra-headers
         `(("Authorization" . ,(funcall gist-authenticate-function))))
        (url-request-method method)
        (url-max-redirection -1))
    (url-retrieve url callback)))

;;;###autoload
(defun gist-region (begin end &optional private)
  "Post the current region as a new paste at gist.github.com
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "r\nP")
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (name (file-name-nondirectory file))
         (description (read-from-minibuffer "Description: ")))
    (gist-request
     "POST"
     "https://api.github.com/gists"
     'gist-created-callback
     `(("description" . ,description)
       ("public" . ,(if private :json-false 't))
       ("files" .
        ((,name .
                (("content" . ,(buffer-substring begin end))))))))))

(defun gist-created-callback (status)
  (let ((location (save-excursion
                    (goto-char (point-min))
                    (and (re-search-forward "^Location: \\(.*\\)" nil t)
                         (match-string 1))))
        (http-url))
    (cond
     ;; check redirected location indicate public/private gist url
     ((and (stringp location)
           (string-match "\\([0-9]+\\|[0-9a-zA-Z]\\{20\\}\\)$" location))
      (let ((id (match-string 1 location)))
        (setq http-url (format "https://gist.github.com/%s" id))
        (message "Paste created: %s" http-url)
        (when gist-view-gist
          (browse-url http-url))))
     (t
      (message "Paste is failed")))
    (when http-url
      (kill-new http-url))
    (url-mark-buffer-as-dead (current-buffer))))

(defun gist-make-query-string (params)
  "Returns a query string constructed from PARAMS, which should be
a list with elements of the form (KEY . VALUE). KEY and VALUE
should both be strings."
  (mapconcat
   (lambda (param)
     (concat (url-hexify-string (car param)) "="
             (url-hexify-string (cdr param))))
   params "&"))

;;;###autoload
(defun gist-region-private (begin end)
  "Post the current region as a new private paste at gist.github.com
Copies the URL into the kill ring."
  (interactive "r")
  (gist-region begin end t))

(defun github-config (key)
  "Returns a GitHub specific value from the global Git config."
  (let ((strip (lambda (string)
                 (if (> (length string) 0)
                     ;;strip newline
                     (substring string 0 -1))))
        (git (executable-find "git")))
  (funcall strip (shell-command-to-string
                  (format "%s config --global github.%s" git key)))))

(defun github-set-config (key value)
  "Sets a GitHub specific value to the global Git config."
  (let ((git (executable-find "git")))
    (shell-command-to-string
     (format "%s config --global github.%s %s" git key value))))

;; FIXME obsoleted auth function?
(defun github-auth-info ()
  "Returns the user's GitHub authorization information.
Searches for a GitHub username and token in the global git config,
and returns (USERNAME . TOKEN). If nothing is found, prompts
for the info then sets it to the git config."
  (interactive)

  ;; If we've been called within a scope that already has this
  ;; defined, don't take the time to get it again.
  (if (boundp '*github-auth-info*)
      *github-auth-info*

    (let* ((user (or github-user (github-config "user")))
           (token (or github-token (github-config "token"))))

      (when (not user)
        (setq user (read-string "GitHub username: "))
        (github-set-config "user" user))

      (when (not token)
        (setq token (read-string "GitHub API token: "))
        (github-set-config "token" token))

      (cons user token))))

;; 1. Register a oauth application
;;   https://github.com/account/applications/
;; 2. Open like following url by web-browser replace URL with registered callback url
;;    and client-id with CLIENT-ID
;;  https://github.com/login/oauth/authorize?redirect_uri=**URL**&client_id=**CLIENT-ID**
;; 3. Copy the code in the redirected url query string.
;;    ex: http://www.example.com/?code=SOME-CODE
;; 4. Open like following url by web-browser with replacing query-string like process 2.
;; https://github.com/login/oauth/access_token?code=**CODE**&redirect_uri=**URL**&client_id=**CLIENT-ID**&client_secret=**CLIENT-SECRET**

(defun github-auth-info-oauth2 ()
  (let* ((token (or github-token (github-config "oauth-token"))))

    (when (not token)
      (setq token (read-string "GitHub OAuth token: "))
      (github-set-config "oauth-token" token))

    token))

(defun github-auth-info-basic ()
  (let* ((user (or github-user (github-config "user")))
         pass)

    (when (not user)
      (setq user (read-string "GitHub username: "))
      (github-set-config "user" user))

    (setq pass (gist-get-user-password))

    (cons user pass)))

(defun gist-get-user-password ()
  (or gist-user-password
      (read-passwd "Password: ")))

;;;###autoload
(defun gist-buffer (&optional private)
  "Post the current buffer as a new paste at gist.github.com.
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "P")
  (gist-region (point-min) (point-max) private))

;;;###autoload
(defun gist-buffer-private ()
  "Post the current buffer as a new private paste at gist.github.com.
Copies the URL into the kill ring."
  (interactive)
  (gist-region-private (point-min) (point-max)))

;;;###autoload
(defun gist-region-or-buffer (&optional private)
  "Post either the current region, or if mark is not set, the current buffer as a new paste at gist.github.com
Copies the URL into the kill ring.

With a prefix argument, makes a private paste."
  (interactive "P")
  (if (gist-region-active-p)
      (gist-region (region-beginning) (region-end) private)
    (gist-buffer private)))

;;;###autoload
(defun gist-region-or-buffer-private ()
  "Post either the current region, or if mark is not set, the current buffer as a new private paste at gist.github.com
Copies the URL into the kill ring."
  (interactive)
  (if (gist-region-active-p)
      (gist-region-private (region-beginning) (region-end))
    (gist-buffer-private)))

;;;###autoload
(defun gist-list ()
  "Displays a list of all of the current user's gists in a new buffer."
  (interactive)
  (message "Retrieving list of your gists...")
  (gist-request
   "GET"
   "https://api.github.com/gists"
   'gist-lists-retrieved-callback))

(defun gist-region-active-p ()
  (if (functionp 'region-active-p)
      ;; trick for suppressing elint warning
      (funcall 'region-active-p)
    (and transient-mark-mode mark-active)))

(defun gist-lists-retrieved-callback (status)
  "Called when the list of gists has been retrieved. Parses the result
and displays the list."
  (goto-char (point-min))
  (when (re-search-forward "^\r?$" nil t)
    (let ((json (json-read)))
      (url-mark-buffer-as-dead (current-buffer))
      (with-current-buffer (get-buffer-create "*gists*")
        (toggle-read-only -1)
        (setq truncate-lines t)
        (goto-char (point-min))
        (save-excursion
          (kill-region (point-min) (point-max))
          (gist-insert-list-header)
          (mapc 'gist-insert-gist-link json)

          ;; remove the extra newline at the end
          (delete-backward-char 1))

        ;; skip header
        (forward-line)
        (toggle-read-only t)
        (set-window-buffer nil (current-buffer))))))

(defun gist-insert-list-header ()
  "Creates the header line in the gist list buffer."
  (save-excursion
    (insert "  ID           Updated                "
            "  Visibility  Description             "
            (gist-fill-string "" (frame-width))
            "\n"))
  (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
    (overlay-put ov 'face 'header-line))
  (forward-line))

(defun gist-insert-gist-link (gist)
  "Inserts a button that will open the given gist when pressed."
  (let* ((data (gist-parse-gist gist))
         (repo (car data)))
    (mapc '(lambda (x) (insert (format "  %s   " x))) (cdr data))
    (make-text-button (line-beginning-position) (line-end-position)
                      'repo repo
                      'action 'gist-describe-button
                      'face 'default
                      'gist-json gist))
  (insert "\n"))

(defun gist-describe-button (button)
  (let ((json (button-get button 'gist-json)))
    (with-help-window (help-buffer)
      (with-current-buffer standard-output
        (gist-describe-gist-1 json)))))

(defun gist-describe-insert-button (text action json)
  (let ((button-text (if (display-graphic-p) text (concat "[" text "]")))
        (button-face (if (display-graphic-p)
                         '(:box (:line-width 2 :color "dark grey")
                                :background "light grey"
                                :foreground "black")
                       'link))
        (id (cdr (assq 'id json))))
    (insert-text-button button-text
                        'face button-face
                        'follow-link t
                        'action action
                        'repo id
                        'gist-json json)
    (insert " ")))

(defun gist-describe-gist-1 (gist)
  (require 'lisp-mnt)
  (let ((id (cdr (assq 'id gist)))
        (description (cdr (assq 'description gist)))
        (url (cdr (assq 'html_url gist)))
        (updated (cdr (assq 'updated_at gist)))
        (publicp (eq (cdr (assq 'public gist)) t)))

    (insert
     (if publicp
         (propertize "Public Gist"
                     'font-lock-face `(bold underline ,font-lock-warning-face))
       (propertize "Private Gist"
                   'font-lock-face '(bold underline)))
     "\n")
    (insert "  " (propertize "Description: " 'font-lock-face 'bold) (or description "") "\n")
    (insert "          " (propertize "URL: " 'font-lock-face 'bold) url "\n")
    (insert "      " (propertize "Updated: " 'font-lock-face 'bold)
            (format-time-string
             gist-display-date-format
             (gist-parse-time-string updated)) "\n")

    (insert "\n\n")

    (gist-describe-insert-button "Fetch Repository" 'gist-fetch-button gist)
    (gist-describe-insert-button "Browse" 'gist-open-web-button gist)

    (insert "\n\n")

    (gist-describe-insert-button "Edit Description" 'gist-update-button gist)
    (gist-describe-insert-button "Delete Gist" 'gist-delete-button gist)))

(defun gist-fetch-button (button)
  "Called when a gist [Fetch] button has been pressed.
Fetche gist repository and open the directory.

See `gist-working-directory-alist' document to fetch repository
into the user selected directory."
  (gist-fetch (button-get button 'repo)))

(defun gist-delete-button (button)
  "Called when a gist [Delete] button has been pressed.
Confirm and delete the gist."
  (when (y-or-n-p "Really delete this gist? ")
    (gist-delete (button-get button 'repo))))

(defun gist-update-button (button)
  "Called when a gist [Edit] button has been pressed.
Edit the gist description."
  (let* ((json (button-get button 'gist-json))
         (desc (read-from-minibuffer
                "Description: "
                (cdr (assq 'description json)))))
    (gist-update (button-get button 'repo) desc)))

(defun gist-open-web-button (button)
  "Called when a gist [Browse] button has been pressed."
  (let* ((json (button-get button 'gist-json))
         (url (cdr (assq 'html_url json))))
    (browse-url url)))

(defun gist-parse-gist (gist)
  "Returns a list of the gist's attributes for display, given the xml list
for the gist."
  (let ((repo (cdr (assq 'id gist)))
        (updated-at (cdr (assq 'updated_at gist)))
        (description (cdr (assq 'description gist)))
        (visibility (if (eq (cdr (assq 'public gist)) 't)
                    "public"
                  "private")))
    (list repo
          (gist-fill-string repo 8)
          (gist-fill-string
           (format-time-string
            gist-display-date-format (gist-parse-time-string updated-at))
           20)
          (gist-fill-string visibility 7)
          (or description ""))))

(defun gist-parse-time-string (string)
  (let* ((times (split-string string "[-T:Z]" t))
         (getter (lambda (x) (string-to-number (nth x times))))
         (year (funcall getter 0))
         (month (funcall getter 1))
         (day (funcall getter 2))
         (hour (funcall getter 3))
         (min (funcall getter 4))
         (sec (funcall getter 5)))
    (encode-time sec min hour day month year 0)))

(defun gist-fill-string (string width)
  (truncate-string-to-width string width nil ?\s))

(defcustom gist-working-directory "~/.gist"
  "*Working directory where to go gist repository is."
  :type 'directory)

(defcustom gist-working-directory-alist nil
  "*Alist of gist id as key, value is directory path.

Example:
\(setq gist-working-directory-alist
      `((\"1080701\" . \"~/mygist/Emacs-nativechecker\")))
"
  :type '(alist :key-type string
                :value-type directory))

(defconst gist-repository-url-format "git@gist.github.com:%s.git")

(defun gist-fetch (id)
  (let* ((url (format gist-repository-url-format id))
         (working-copy (gist-working-copy-directory id)))
    (cond
     ((not (file-directory-p (expand-file-name ".git" working-copy)))
      (message "Cloning %s into working copy..." url)
      (gist-start-git `("clone" ,url ".") working-copy))
     (t
      (message "Fetching %s into working copy... " url)
      (gist-start-git `("pull" ,url) working-copy)))
    (dired working-copy)))

(defun gist-delete (id)
  (gist-request
   "DELETE"
   (format "https://api.github.com/gists/%s" id)
   (gist-simple-receiver "Delete")))

(defun gist-update (id description)
  (gist-request
   "PATCH"
   (format "https://api.github.com/gists/%s" id)
   (gist-simple-receiver "Update")
   `(,@(and description
            `(("description" . ,description))))))

(defun gist-working-copy-directory (id)
  (let* ((pair (assoc id gist-working-directory-alist))
         (dir (cond
               (pair
                (cdr pair))
               (t
                (expand-file-name id gist-working-directory)))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun gist-start-git (args &optional directory)
  (let* ((buffer (generate-new-buffer " *gist git* "))
         (default-directory
           (or (and directory (file-name-as-directory directory))
               default-directory))
         (proc (apply 'start-process "Gist" buffer "git" args)))
    (set-process-sentinel
     proc (lambda (p e)
            (when (memq (process-status p) '(exit signal))
              (let ((code (process-exit-status p)))
                (cond
                 ((eq code 0)
                  (message "Done fetching gist repository."))
                 (t
                  (message "Gist git process finished with %d" code))))
              (kill-buffer (process-buffer p)))))
    proc))

(defun gist-simple-receiver (message)
  ;; Create a receiver of `gist-request'
  `(lambda (status)
     (goto-char (point-min))
     (when (re-search-forward "^Status: \\([0-9]+\\)" nil t)
       (let ((code (string-to-number (match-string 1))))
         (if (and (<= 200 code) (< code 300))
             (message "%s succeeded" ,message)
           (message "%s failed" ,message))))))

(provide 'gist)

;;; gist.el ends here.