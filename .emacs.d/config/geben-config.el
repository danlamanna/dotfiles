(custom-set-variables
 '(geben-dbgp-default-proxy '("127.0.0.1" 9001 "dan" nil t))
 '(geben-dbgp-feature-list (quote ((:set max_data 32768) (:set max_depth 1) (:set max_children 1024) (:get breakpoint_types geben-dbgp-breakpoint-store-types))))
 '(geben-dbgp-redirect-buffer-init-hook nil)
 '(geben-temporary-file-directory "~/.emacs.d/tmp/geben"))

(defun geben-safely-end-proxy()
  "Tries to call `dbgp-proxy-unregister', but silently
   returns `nil' if it throws an error."
  (interactive)
  (condition-case nil
      (dbgp-proxy-unregister "dan")
    (error nil)))

(defun geben-single-or-proxy()
  "Tries calling geben, if it throws an error because it needs to use
   `geben-proxy', it tries that.
   TODO: make it toggle.."
  (interactive)
  (condition-case nil
      (geben)
    (error (geben-proxy "127.0.0.1" 9001 "dan"))))

(defadvice geben-display-context(before clear-windows-for-vars activate)
  (delete-other-windows))

(add-hook 'kill-emacs-hook 'geben-safely-end-proxy)

(provide 'geben-config)


(setq geben-disable-ajax-requests t)

(add-hook 'geben-session-enter-hook 'geben-ignore-ajax-requests)

(defun geben-ignore-ajax-requests(session)
  (when geben-disable-ajax-requests
    (let* ((php-is-ajax "(int)(!empty($_SERVER['HTTP_X_REQUESTED_WITH']) && strtolower($_SERVER['HTTP_X_REQUESTED_WITH']) == 'xmlhttprequest')")
           (is-ajax-result (format "%s" (geben-eval-expression php-is-ajax))))
      (when (string-equal "result: 1" is-ajax-result) ;; is an aja request
        (geben-quit-window)))))
