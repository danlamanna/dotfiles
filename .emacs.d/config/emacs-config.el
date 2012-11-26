;; Misc settings
(menu-bar-mode -1)
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; Winner mode
(require 'winner)
(winner-mode t)

;; Remove prompts from close
(defun kill-emacs-no-prompt()
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(global-set-key (kbd "C-x C-c") 'kill-emacs-no-prompt)

;; Before save
(add-hook 'before-save-hook '(lambda()
			       (if (buffer-file-name)
				   (unless (string-equal (file-name-extension (buffer-file-name)) "md")
				     (whitespace-cleanup)))))
(add-hook 'before-save-hook
	  '(lambda ()
	     "If the file doesn't exist, we attempt to make the directory,
	      this way, we can 'open' files in non-existent directories, and when
	      saving them, it creates the directory for us."
	     (or (file-exists-p (file-name-directory buffer-file-name))
		 (make-directory (file-name-directory buffer-file-name) t))))

;; Advice
(defadvice zap-to-char (after zap-until-char (arg char) activate)
  "Makes zap-to-char act like zap-until-char."
  (insert char)
  (backward-char 1))

;; Disable backup/autosave files
(setq auto-save-interval 0)
(setq backup-inhibited t)
(setq auto-save-default nil)

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

(defun rename-file-and-buffer(new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME."
 (interactive "sNew name: ")
 (let ((name (buffer-name))
       (filename (buffer-file-name)))
   (if (not filename)
       (message "Buffer '%s' is not visiting a file!" name)
     (if (get-buffer new-name)
	 (message "A buffer named '%s' already exists!" new-name)
       (progn  (rename-file name new-name 1)  (rename-buffer new-name)  (set-visited-file-name new-name)  (set-buffer-modified-p nil))))))

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

(provide 'emacs-config)
