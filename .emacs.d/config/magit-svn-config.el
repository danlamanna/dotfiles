(defvar magit-svn-externals-dir ".git_externals")

(defun magit-svn-fetch-externals()
  "Loops through all external repos found by `magit-svn-get-externals'
   and runs git svn fetch, and git svn rebase on each of them."
  (interactive)
  (let ((externals (magit-svn-get-externals)))
    (if (not externals)
        (message "No SVN Externals found. Check magit-svn-externals-dir.")
      (dolist (external externals)
        (let ((default-directory (file-name-directory external)))
          (magit-run-git "svn" "fetch")
          (magit-run-git "svn" "rebase")))
      (magit-refresh))))

(defun magit-svn-get-externals()
  (let* ((topdir (magit-get-top-dir "."))
         (default-directory (concat topdir magit-svn-externals-dir))
         (find (find-cmd '(and (name ".git")
                               (type "d")))))
    (when (file-directory-p default-directory)
      (remove "" (split-string (shell-command-to-string find) "\n")))))

(magit-key-mode-insert-action 'svn "x" "Fetch Externals" 'magit-svn-fetch-externals)

(provide 'magit-svn-config)
