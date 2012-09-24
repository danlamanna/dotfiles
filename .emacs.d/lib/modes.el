;;; CSV Mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;;; PHP Mode
(add-to-list 'load-path (concat shared-lisp-dir "/modes") t)
(autoload 'php-mode "php-mode" "Major mode for PHP files." t)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-hook 'php-mode-hook (lambda()
			   (flymake-mode)))

(require 'zencoding-mode)
(add-hook 'php-mode-hook 'zencoding-mode)


(defun php-closing-paren()
  "Adds a closing paren to the end of the current line, but before any semicolons, if present."
  (interactive)
  (save-excursion
    (setq now-until-eol (substring (buffer-string) (point) (line-end-position)))
    (whitespace-cleanup)
    (move-end-of-line nil)
    (while (eq (char-before (point)) 59)
      (backward-char 1))
    (insert ")")))

(defun php-word-can-be-var(word)
  "Returns t or nil on whether or not the word can be a variable name.
  [a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*")

(defun php-make-var(&optional default-value)
  (interactive)
  (save-excursion
    (backward-word)
    (insert "$")
    (move-end-of-line nil)
    (insert " = ")
    (if default-value
	(insert default-value)))
  (move-end-of-line nil))

(defun php-make-var-array()
  (interactive)
  (php-make-var "array("))

(setq php-func-declarations '(("protected static function")
			      ("public static function")
			      ("private static function")
			      ("static public function")
			      ("static protected function")
			      ("static private function")
			      ("public function")
			      ("protected function")
			      ("private function")
			      ("function")
			      ("class")
			      ("abstract class")
			      ("interface")))

(defun php-nav-next-structure()
  "Navigates to next function def or class def."
  (interactive)
  (setq next-structure 0)
  (setq first-structure nil)
  (save-excursion
    (dolist (el php-func-declarations)
      (let ((fsearch (re-search-forward (car el) nil t)))
	(when (not (eq fsearch nil)) ; when it has a result
	  (message (format "Has a result - %s" (car el)))
	  (if (eq first-structure t) ; its already found a result
	      (message (format "already found a result. %s" fsearch))
	      (if (> next-structure fsearch) ; this result happens first
		  (setq next-structure fsearch))
					; hasn't found a result before, but it has one now
	    (setq first-structure t)
	    (setq next-structure fsearch))))))
  (if (eq first-structure t)
      (goto-char next-structure)
    (message ("No next PHP Structure Found."))))





;;; C Mode
;(add-hook 'c-mode-hook (lambda()
;			 (auto-complete-mode)))

(add-hook 'c-mode-hook (lambda()
			 (set (make-local-variable 'compile-command)
			      (concat "gcc -o " (substring (format "%s" (buffer-name)) 0 (- (length (buffer-name)) 2)) " " (buffer-name)))))



;;; Django!
(autoload 'django-html-mumamo-mode "~/.emacs.d/nxhtml/autostart.el")
(setq auto-mode-alist
      (append '(("\\.djhtml?$" . django-html-mumamo-mode)) auto-mode-alist))
(setq mumamo-background-colors nil)
(add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mumamo-mode))

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(provide 'modes)