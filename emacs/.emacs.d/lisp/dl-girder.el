(defun girder-dir?(&optional directory)
  "Determine if the DIRECTORY is within a Girder directory."
  (locate-dominating-file (or directory default-directory)
                          (lambda (dir)
                            (and (f-exists? (f-join dir "girder"))
                                 (f-exists? (f-join dir "setup.py"))))))

(defun girder-setup-python-buffer()
  (when-let* ((girder-dir (girder-dir?)))
    (if (f-exists? (f-join girder-dir "tests/flake8.cfg"))
        (set (make-local-variable 'flycheck-flake8rc) (f-join girder-dir "tests/flake8.cfg")))))

(add-hook 'python-mode-hook 'girder-setup-python-buffer)

(defun girder-setup-js-buffer()
    (nvm-use-for)
  (when-let* ((girder-dir (girder-dir?)))
    (set (make-local-variable 'flycheck-javascript-eslint-executable)
         (f-join girder-dir "node_modules/.bin/eslint"))
    (set (make-local-variable 'flycheck-eslintrc)
         (f-join girder-dir ".eslintrc"))))

(add-hook 'js2-mode-hook 'girder-setup-js-buffer)

(defun girder-combine-coverage(&rest args)
  "Combine coverage in a Girder project.
   If this is called from within a Girder project, a coverage executable
   exists, and uncombined coverage files exist (.coverage.*). This will
   combine the coverage files into a .coverage file."
  (-when-let* ((default-directory (find-girder-dir))
               (coverage-executable (executable-find "coverage"))
               (coverage-files (f-glob ".coverage.*")))
    (shell-command (format "%s combine" coverage-executable))))

(advice-add 'pycoverage-mode :before 'girder-combine-coverage)

(provide 'dl-girder)
