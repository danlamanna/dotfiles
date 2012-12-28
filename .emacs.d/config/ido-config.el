(ido-mode 'both)

(setq
 ido-save-directory-list-file (format "%s/ido.last" emacs-tmp-dir)
 ido-ignore-buffers '(".*Completion"
                      "\\*")
 ido-work-directory-list '("~/" "~/projects")
 ido-enable-flex-matching t
 ido-case-fold t
 ido-enable-last-directory-history t
 ido-max-work-directory-list 10
 ido-max-work-file-list 20
 ido-use-filename-at-point nil
 ido-use-url-at-point nil
 ido-max-prospects 7
 ido-create-new-buffer 'always
 ido-confirm-unique-completion nil)

(setq confirm-nonexistent-file-or-buffer nil)

(provide 'ido-config)
