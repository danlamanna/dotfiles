;; Misc settings
(menu-bar-mode -1)
(show-paren-mode t)
(setq show-paren-style 'mixed)

(setq inhibit-splash-screen t)

;; Autosave/Backup Stuff
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat emacs-tmp-dir "/backups")))))

(setq vc-make-backup-files t)

;; X Emacs stuff..
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(defun toggle-fullscreen()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(global-set-key (kbd "<f11>") 'toggle-fullscreen)

(toggle-fullscreen)

;; Dired
(add-hook 'dired-mode-hook (lambda()
                             (hl-line-mode)))

;; Winner mode
(require 'winner)
(winner-mode t)

;; Remove prompts from close
(defun kill-emacs-no-prompt()
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(global-set-key (kbd "C-x C-c") 'kill-emacs-no-prompt)

(defun make-files-directory-if-not-exists()
  "Makes the directory of the file referenced in `buffer-file-name',
   so we can 'open' files in non-existent directories, and this can
   create the directory. `before-save-hook' ftw."
  (interactive)
  (if (and (buffer-file-name)
           (not (file-exists-p (file-name-directory (buffer-file-name)))))
      (make-directory (file-name-directory buffer-file-name) t)))

(add-hook 'before-save-hook 'make-files-directory-if-not-exists)

(if (fboundp 'w3m-browse-url)
    (custom-set-variables
     '(browse-url-browser-function 'w3m-browse-url))
  (if (fboundp 'browse-url-chromium)
      (custom-set-variables
       '(browse-url-browser-function 'browse-url-chromium))))

;; Copying current line (start-finish, regardless of point)
;; http://justin.jetfive.com/emacs-copy-line-to-kill-ring-fast
(defun quick-copy-line ()
  "Copy the whole line that point is on and move to the beginning of the next line.
    Consecutive calls to this command append each line to the
    kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
        (end (line-beginning-position 2)))
    (if (eq last-command 'quick-copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end))))
  (beginning-of-line 2))

;; Advice
(defadvice zap-to-char (after zap-until-char (arg char) activate)
  "Makes zap-to-char act like zap-until-char."
  (insert char)
  (backward-char 1))

;; Remove prompt of killing a buffer with a running process
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; Misc functions
(defun swap-windows()
 "If you have 2 windows, it swaps them."
 (interactive)
 (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
       (t
        (let* ((w1 (first (window-list)))
               (w2 (second (window-list)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)))))


(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(defun revert-all-buffers()
   "Refreshes all open buffers from their respective files."
   (interactive)
   (let* ((list (buffer-list))
          (buffer (car list)))
     (while buffer
       (when (buffer-file-name buffer)
         (set-buffer buffer)
         (revert-buffer t t t))
       (setq list (cdr list))
       (setq buffer (car list))))
   (message "done."))

(defun generate-rand-string(&optional char-set &optional len)
  "Generates a random string and inserts it at `point'. With no
   arguments, it conforms to an MD5 hashes pattern.

   CHAR-SET can be specified as a string with characters to be used,
   by default its set to 0-9a-z.

   LEN can be passed to specify how many characters it should insert,
   defaults at 32."
  (interactive)
  (let ((char-set (or char-set
                      "1234567890abcdefghijklmnopqrstyvwxyz")))
    (dotimes (i (or len 32))
      (insert (elt char-set (random (length char-set)))))))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; saveplace
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".saveplace" emacs-tmp-dir))


(defun open-line-below ()
  (interactive)
  (if (eolp)
      (newline)
    (end-of-line)
    (newline))
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

(fset 'yes-or-no-p 'y-or-n-p)

(provide 'emacs-config)
