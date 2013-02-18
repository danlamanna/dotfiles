(require 's)

(defgroup cower-mode nil
  "Emacs interface to cower."
  :prefix "cower-mode-"
  :group 'tools)

(defcustom cm/default-target-dir "."
  "Default target directory for AUR packages."
  :type 'string
  :group 'cower-mode)

(defcustom cm/search-command "cower --search"
  "Default cower command to use to search AUR packages."
  :type 'string
  :group 'cower-mode)

(define-derived-mode cower-mode tabulated-list-mode "Cower (AUR)"
  "Major mode for browsing packages from the Arch User Repository."
  (setq tabulated-list-format [("Package" 22 nil)
                               ("Version" 12 nil)
                               ("Status"  10 nil)
                               ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun cm/installed()
  (executable-find "cower"))

(defun cm/--package-is-installed(package)
  (string-equal package (s-trim (shell-command-to-string (format "pacman -Qq %s" package)))))

(defun cm/--parse-description(arg)
  (s-trim (replace-regexp-in-string "([0-9]+)" "" arg)))

(defun cm/--parse-package-listing(package-listing &optional tabulated-list-entry)
  (let* ((split (split-string package-listing))
         (listing `((package  . ,(car split))
                    (version . ,(car (cdr split)))
                    (votes   . ,(replace-regexp-in-string "(\\|)" "" (car (cdr (cdr split)))))
                    (description . ,(cm/--parse-description (mapconcat 'identity (cdr (cdr (cdr split))) " "))))))
    (if tabulated-list-entry
        (list (cdr (assoc 'package listing))
              `[,(list (cdr (assoc 'package listing))
                       'action 'foo
                       'package-symbol (cdr (assoc 'package listing)))
                ,(cdr (assoc 'version listing))
                ,(if (cm/--package-is-installed (cdr (assoc 'package listing))) "Installed" "Available")
                ,(cdr (assoc 'description listing))])
      listing)))

(defun cm/describe-package(package)
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (prin1 package)
      (princ " is ")
      (if (cm/--package-is-installed package)
          (insert "an installed package.\n\n")
        (progn
          (insert "an uninstalled package. ")
          (insert "INSTALL.\n\n")))
      (insert (shell-command-to-string (format "cower -ii %s" package))))))

(defun foo(&optional button)
  (cm/describe-package (tabulated-list-get-id)))

(defun cm/list-packages(search-terms)
  (interactive "sPackages Like: ")
  (with-current-buffer (get-buffer-create "*cower*")
    (cower-mode)
    (goto-char (point-min))
    (erase-buffer)
    (let* ((cower-package-listing (cm/--package-results search-terms))
           (cower-package-listings (split-string cower-package-listing "aur/" t)))
      (dolist (package-listing cower-package-listings)
        (push (cm/--parse-package-listing package-listing t) tabulated-list-entries))
      (tabulated-list-print)
      (pop-to-buffer "*cower*"))))

(defun cm/--package-results(terms)
  (let ((results (shell-command-to-string (format "%s %s" cm/search-command terms))))
    (replace-regexp-in-string "\\[installed\\]" "" results)))


(provide 'cower-mode)
