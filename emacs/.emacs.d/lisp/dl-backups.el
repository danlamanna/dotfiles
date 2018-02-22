(custom-set-variables
 '(auto-save-interval 30)
 '(auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
 '(vc-make-backup-files t) ;; backup version controlled files, too
 '(backup-by-copying t) ;; no symlinks
 '(delete-old-versions t) ;; no confirm
 '(kept-new-versions 20)
 '(kept-old-versions 20)
 '(version-control t) ;; number backups
 '(backup-directory-alist
   `(("." . ,(no-littering-expand-var-file-name "backup/")))))

;; backup every save, instead of just the first time in the buffer
(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))
(add-hook 'before-save-hook 'force-backup-of-buffer)

(provide 'dl-backups)
