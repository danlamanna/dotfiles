;;; pyvirtualenv.el --- Python Pyvirtualenv support

;; Copyright (C) 2012  Jorgen Schaefer <forcer@forcix.cx>

;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; URL: https://github.com/jorgenschaefer/pyvirtualenv
;; Version: 1.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The pyvirtualenv-mode provides some simple support for Python's
;; pyvirtualenv. A single keystroke can activate, deactivate, or switch
;; between pyvirtualenvs.

;;; Code:

(defvar pyvirtualenv-current nil
  "The current virtual env, or nil if none.")

(defvar pyvirtualenv-mode-line nil
  "The mode line entry for pyvirtualenv support.")

(defvar pyvirtualenv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") 'pyvirtualenv)
    map)
  "Key map to use in `pyvirtualenv-mode'.")

;;;###autoload
(define-minor-mode pyvirtualenv-mode
  "Minor mode providing easy interface to Python's pyvirtualenvs.

\\{pyvirtualenv-mode-map}"
  :global t
  :lighter ""
  (if (not pyvirtualenv-mode)
      (when (memq 'pyvirtualenv-mode-line mode-line-format)
        (setq mode-line-format (delq 'pyvirtualenv-mode-line
                                     mode-line-format)))
    (let ((rest mode-line-format))
      (catch 'break
        (while rest
          (when (eq 'vc-mode (car-safe (cadr rest)))
            (setcdr rest (cons 'pyvirtualenv-mode-line
                               (cdr rest)))
            (throw 'break nil))
          (setq rest (cdr rest)))))))

;;;###autoload
(defun pyvirtualenv (pyvirtualenv)
  "Switch to pyvirtualenv PYVIRTUALENV.

If PYVIRTUALENV is an existing directory, it is assumed to be the
location of an existing virtual environment. If it does not
exist, it's created as a new virtual environment, and activated.

If the argument is nil, or when a prefix argument is given, all
changes to the environment are removed.

NOTE: Both Pymacs and any inferior Python shell will be
unaffected by this until you restart them. Doing so automatically
might lose data, so we avoid that."
  (interactive (list (if current-prefix-arg
                         nil
                       (read-directory-name "Virtual Environment: "))))
  (when pyvirtualenv-current
    (pyvirtualenv-deactivate))
  (when pyvirtualenv
    (setq pyvirtualenv (expand-file-name pyvirtualenv))
    (when (not (file-directory-p pyvirtualenv))
      (when (not (y-or-n-p (concat "Directory does not exist, "
                                   "create new pyvirtualenv? ")))
        (error "Pyvirtualenv not created."))
      (pyvirtualenv-create pyvirtualenv))
    (pyvirtualenv-activate pyvirtualenv))
  (if (not pyvirtualenv-current)
      (message "Deactivated pyvirtualenv")
    (pyvirtualenv-check-processes)
    (message "Switched to pyvirtualenv %s" (pyvirtualenv-name))))

(defun pyvirtualenv-name (&optional dir)
  "Return the base name of directory DIR.

Defaults to `pyvirtualenv-current'."
  (when (not dir)
    (setq dir pyvirtualenv-current))
  (if (equal "/" (substring dir -1))
      (file-name-nondirectory (substring dir 0 -1))
    (file-name-nondirectory dir)))

(defun pyvirtualenv-activate (dir)
  "Activate the virtual environment in DIR.

This adjusts `exec-path' and $PATH, as well as making the
pyvirtualenv show up in the mode line."
  (when (not (pyvirtualenv-is-pyvirtualenv dir))
    (error (format "Directory %s is not a virtual environment"
                   dir)))
  (when pyvirtualenv-current
    (pyvirtualenv-deactivate))
  (setq exec-path (cons dir exec-path))
  (setenv "PATH" (concat dir ":" (getenv "PATH")))
  (setq pyvirtualenv-current dir
        pyvirtualenv-mode-line (format "VEnv:%s"
                                          (pyvirtualenv-name)))
  (force-mode-line-update))

(defun pyvirtualenv-deactivate ()
  "Deactivate the current environment."
  (when pyvirtualenv-current
    (setq exec-path (delete pyvirtualenv-current exec-path))
    (setenv "PATH"
            (let ((dir pyvirtualenv-current)
                  (path (getenv "PATH")))
              (with-temp-buffer
                (insert path)
                (goto-char (point-min))
                (while (search-forward (concat dir ":") nil t)
                  (replace-match ""))
                (buffer-string))))
    (setq pyvirtualenv-current nil
          pyvirtualenv-mode-line nil)
    (force-mode-line-update)))

(defun pyvirtualenv-create (dir)
  "Create a new pyvirtualenv in DIR."
  (shell-command (format "pyvirtualenv %s"
                         (shell-quote-argument dir))))

(defun pyvirtualenv-is-pyvirtualenv (dir)
  "Return a true value iff DIR is an actual pyvirtualenv."
  (and (file-directory-p dir)
       (file-exists-p (format "%s/bin/activate_this.py" dir))
       (file-exists-p (format "%s/bin/python" dir))))

(defun pyvirtualenv-check-processes ()
  "Check for any running subprocesses being Python interpreters."
  (let ((python-list nil))
    (dolist (proc (process-list))
      (when (and (process-buffer proc)
                 (process-command proc)
                 (string-match "python"
                               (car (process-command proc))))
        (setq python-list (cons (process-buffer proc)
                                python-list))))
    (when python-list
      (with-help-window (get-buffer-create "*Pyvirtualenv Warning*")
        (with-current-buffer (get-buffer-create "*Pyvirtualenv Warning*")
          (insert
           "You have changed your pyvirtualenv, but some Python processes\n"
           "are running that might still be using the old environment.\n"
           "Please recheck these buffers and restart them as needed.\n"
           "\n")
          (dolist (buf python-list)
            (insert " ")
            (insert-button (buffer-name buf)
                           'action 'pyvirtualenv-button-activate
                           'pyvirtualenv-buffer buf)
            (insert "\n")))))))

(defun pyvirtualenv-button-activate (button)
  "Activate BUTTON."
  (switch-to-buffer-other-window (button-get button 'pyvirtualenv-buffer)))

(provide 'pyvirtualenv)
;;; pyvirtualenv.el ends here
