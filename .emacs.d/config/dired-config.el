(defun truncate-file-contents(filename)
  (with-temp-buffer
    (write-file filename)))

(defun dired-do-truncate()
  (interactive)
  (mapc 'truncate-file-contents (dired-get-marked-files)))

(provide 'dired-config)
