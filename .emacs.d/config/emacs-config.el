(require 'winner)
(winner-mode t)


(defun kill-emacs-no-prompt()
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(global-set-key (kbd "C-x C-c") 'kill-emacs-no-prompt)

(add-hook 'before-save-hook 'whitespace-cleanup)

(menu-bar-mode -1)

(show-paren-mode t)
(setq show-paren-style 'mixed)

(defadvice zap-to-char (after zap-until-char (arg char) activate)
  "Makes zap-to-char act like zap-until-char."
  (insert char)
  (backward-char 1))


;; None of that M-x make-directory nonsense
;; @ec get rid of warning
(add-hook 'before-save-hook
	  '(lambda ()
	     (or (file-exists-p (file-name-directory buffer-file-name))
		 (make-directory (file-name-directory buffer-file-name) t))))

;; Disable backup/autosave files
(setq auto-save-interval 0)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Remove prompt of killing a buffer with a running process
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
	 kill-buffer-query-functions))




;; Swaps 2 windows with each other
(defun swap-windows ()
 "If you have 2 windows, it swaps them." (interactive) (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
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

;; Renames the file and buffer :
(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
       (filename (buffer-file-name)))
 (if (not filename)
     (message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
      (message "A buffer named '%s' already exists!" new-name)
   (progn  (rename-file name new-name 1)  (rename-buffer new-name)  (set-visited-file-name new-name)  (set-buffer-modified-p nil))))))

;; Refreshes all buffers from their open files
(defun revert-all-buffers ()
   "Refreshes all open buffers from their respective files"
   (interactive)
   (let* ((list (buffer-list))
	  (buffer (car list)))
     (while buffer
       (when (buffer-file-name buffer)
	 (set-buffer buffer)
	 (revert-buffer t t t))
       (setq list (cdr list))
       (setq buffer (car list))))
  (message "Refreshing open files"))

(defun generate-alphanumeric-string()
  (interactive)
  (let ((mycharset "1234567890abcdefghijklmnopqrstyvwxyz"))
    (dotimes (i 32)
      (insert (elt mycharset (random (length mycharset)))))))


(defun django-manage-command()
  (interactive)
  (when (stringp (buffer-file-name))
    (let ((inner-command (read-from-minibuffer "manage.py: "))
	  (abs-manage-py (locate-dominating-file (buffer-file-name) "manage.py")))
      (if (eq abs-manage-py nil)
	  (message "No manage.py found in project.")
	(shell-command (format "python %smanage.py %s --settings settings.dev" abs-manage-py inner-command))))))


(provide 'emacs-config)
