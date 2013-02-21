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

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read geben-find-file 'geben)
(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

(provide 'ido-config)
