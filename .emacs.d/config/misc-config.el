(setq read-file-name-completion-ignore-case nil)

(require 'winner)
(winner-mode t)

(defvar hipchat-jabber-id "170_26771@chat.hipchat.com")

(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(require 'mark-more-like-this)
(global-set-key (kbd "M-,") 'mark-previous-like-this)
(global-set-key (kbd "M-.") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this)

;; Making buffers open side by side rather than horizontal terribleness.
(setq split-height-threshold nil)
(setq split-width-threshold 500)

;; Get rid of prompting you to kill
(defun my-kill-emacs ()
  "save some buffers, then exit unconditionally"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))
(global-set-key (kbd "C-x C-c") 'my-kill-emacs)

;; Turn off terrible menu bar
(menu-bar-mode -1)

;; Turn on paren matching
(show-paren-mode t)
(setq show-paren-style 'mixed)

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



;; None of that M-x make-directory nonsense
(add-hook 'before-save-hook
	  '(lambda ()
	     (or (file-exists-p (file-name-directory buffer-file-name))
		 (make-directory (file-name-directory buffer-file-name) t))))

;; Disable backup/autosave files
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Remove prompt of killing a buffer with a running process
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
	 kill-buffer-query-functions))

(provide 'misc-config)
