(require 'ido) 
(ido-mode 'both) ; for buffers and files
(setq 
  ido-save-directory-list-file "~/.emacs.d/cache/ido.last"

  ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace" "^\*compilation" "^\*GTAGS" "^session\.*")
  ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
  ido-case-fold  t                 ; be case-insensitive

  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)
  ido-max-prospects 10              ; don't spam my minibuffer
  ido-create-new-buffer 'always ;don't prompt if a buff er doesn't exist, just create
  ido-confirm-unique-completion nil)

; when using ido, the confirmation is rather annoying...
 (setq confirm-nonexistent-file-or-buffer nil)

(provide 'ido-config)
