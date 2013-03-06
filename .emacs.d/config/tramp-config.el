(require 'tramp)

(setq tramp-default-method "ssh")

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(defun sudo-tramp-current-file()
  (interactive)
  (when buffer-file-name
    (let ((pos (point)))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
      (goto-char pos))))

(provide 'tramp-config)
