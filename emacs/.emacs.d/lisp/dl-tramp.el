;; vagrant tramp
(use-package vagrant-tramp
  :ensure t
  :config (progn
            ;; Exact override of the original except it changes the -drop 8 to -drop 7.
            ;; See: https://github.com/dougm/vagrant-tramp/issues/33
            (defun vagrant-tramp--all-boxes ()
              (let* ((status-cmd "vagrant global-status --machine-readable")
                     (status-raw (shell-command-to-string status-cmd))
                     (status-lines (-drop 7 (split-string status-raw "\n")))
                     (status-data-raw (--map (mapconcat 'identity
                                                        (-drop 4 (split-string it ",")) ",")
                                             status-lines))
                     (status-data (--map (replace-regexp-in-string " " "" it) status-data-raw))
                     (status-groups (-butlast (-split-on "" status-data)))
                     (vm-attrs '(id name provider state dir)))
                (--map (-zip vm-attrs it) status-groups)))))


;; docker tramp
(use-package docker-tramp
  :ensure t
  :config (progn
            (custom-set-variables
             '(docker-tramp-use-names t))))

(use-package helm-tramp
  :ensure t
  :after (docker-tramp vagrant-tramp)
  :bind ("C-c s" . helm-tramp))

(provide 'dl-tramp)

;; Taken from
;; http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
