(require 'cl)
(require 'thingatpt)

(defun wp-eldoc-setup()
  (setq wp-eldoc-functions-hash
        (let ((hash (make-hash-table :size 2500 :test 'equal)))
          (mapc (lambda (func)
                  (puthash (car func) (rest func) hash))
                (-filter (lambda(func-line)
                           (> (length func-line) 1)) (wp-eldoc-slow-function-defs)))
          hash)))

(defun wp-eldoc-slow-function-defs()
  (let ((function-lines '()))
    (dolist (tag (wp-get-tag-names "generic"))
      (with-current-buffer (find-tag-noselect tag)
        (let ((line (thing-at-point 'line t)))
          (when (string-match "function [A-Za-z0-9\_]+\(\\(.*\\)\)" line)
            (push (-concat `(,tag) (mapcar 's-trim (s-split "," (match-string 1 line) t)))  function-lines)))))
    function-lines))

(defun wp-function-and-argument ()
  (condition-case error
      (let ((start-pos (point))
            argument
            (function-name-chars-regex "[-A-Za-z[:digit:]_]"))
        (or (save-excursion
              (when (save-excursion
                      (while (or (plusp (skip-syntax-forward "_w\\s-"))
                                 (plusp (skip-chars-forward "\n"))))
                      (equal (char-after) ?\())
                (list (thing-at-point 'symbol)
                      nil)))
            (save-excursion
              (while (in-string-p)
                (backward-char))
              (let* (( closing-paren
                       (save-excursion
                         (when (search-backward ")" nil t)
                           (point))))
                     ( boundary
                       (save-excursion (search-backward "(")
                                       (when (and closing-paren
                                                  (> closing-paren (point)))
                                         (error "not inside argument list"))
                                       (point)))
                     ( argument-number
                       (let ((counter 0))
                         (while (search-backward "," boundary t)
                           (incf counter))
                         counter))
                     ( function-name
                       (progn (goto-char boundary)
                              (re-search-backward function-name-chars-regex)
                              (forward-char)
                              (setq boundary (point))
                              (ignore-errors
                                (while
                                    (progn (backward-char)
                                           (when (looking-at
                                                  function-name-chars-regex)
                                             (if (equal (point) (point-min))
                                                 (error "beginning of buffer")
                                               t))))
                                (forward-char))
                              (buffer-substring (point) boundary))))
                (list function-name argument-number)))))
    (error nil)))

(defun wp-eldoc-function ()
  (let* ((func (wp-function-and-argument))
         (hash-result (when func
                        (gethash (car func) wp-eldoc-functions-hash)))
         (arguments "")
         (counter 0))
    (when hash-result
      (cl-dolist (arg hash-result)
        (setq arguments
              (concat arguments
                      (if (equal counter (second func))
                          (propertize arg 'face '(:weight bold))
                        arg)
                      ", "))
        (incf counter)))
    (when (>= (length arguments) 2)
      (setq arguments (substring arguments 0 (- (length arguments) 2))))
    (when hash-result
      (concat (propertize (first func) 'face 'font-lock-function-name-face)
              "( " arguments " )"))))

(defun wp-eldoc-ac-candidates ()
  (let (result)
    (maphash (lambda (key value)
               (push key result))
             wp-eldoc-functions-hash)
    result))

(eval-after-load 'auto-complete
  '(ac-define-source wp-eldoc
     '((candidates . wp-eldoc-ac-candidates)
       (cache)
       (symbol . "f"))))

(defun wp-eldoc-enable ()
  (interactive)
  (when (and (fboundp 'auto-complete-mode)
             auto-complete-mode)
    (pushnew 'ac-source-wp-eldoc ac-sources))
  (setq-local eldoc-documentation-function 'wp-eldoc-function)
  (eldoc-mode 1))

(provide 'wordpress-eldoc)
