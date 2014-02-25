;;; vlf-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "vlf" "vlf.el" (21258 37905 540841 875000))
;;; Generated autoloads from vlf.el

(autoload 'vlf "vlf" "\
View Large FILE in batches.
You can customize number of bytes displayed by customizing
`vlf-batch-size'.
Return newly created buffer.

\(fn FILE)" t nil)

;;;***

;;;### (autoloads nil "vlf-ediff" "vlf-ediff.el" (21258 37905 536841
;;;;;;  852000))
;;; Generated autoloads from vlf-ediff.el

(autoload 'vlf-ediff-buffers "vlf-ediff" "\
Run batch by batch ediff over VLF buffers BUFFER-A and BUFFER-B.
Batch size is determined by the size in BUFFER-A.
Requesting next or previous difference at the end or beginning
respectively of difference list, runs ediff over the adjacent chunks.

\(fn BUFFER-A BUFFER-B)" t nil)

(autoload 'vlf-ediff-files "vlf-ediff" "\
Run batch by batch ediff over FILE-A and FILE-B.
Files are processed with VLF with BATCH-SIZE chunks.
Requesting next or previous difference at the end or beginning
respectively of difference list, runs ediff over the adjacent chunks.

\(fn FILE-A FILE-B BATCH-SIZE)" t nil)

;;;***

;;;### (autoloads nil nil ("vlf-base.el" "vlf-follow.el" "vlf-integrate.el"
;;;;;;  "vlf-occur.el" "vlf-pkg.el" "vlf-search.el" "vlf-write.el")
;;;;;;  (21258 37905 554353 335000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; vlf-autoloads.el ends here
