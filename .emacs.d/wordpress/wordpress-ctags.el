(defcustom wp-ctags-executable "/usr/local/bin/ctags"
  "Path to PHP for calling WordPress functions.")

(defcustom wp-ctags-directory "/home/dan/.emacs.d/tmp"
  "Path to directory to store ctags.")

;; ctags command line configurations
(defconst wp-ctags-language "--languages=PHP")

;; default kinds to tag, classes, interfaces, functions
(defconst wp-ctags-generic-kinds "--PHP-kinds=+cif-dvj")

;; capture do_action declarations
(defconst wp-ctags-actions-regex "'--regex-PHP=/do_action\\(\\s?([\"]{1}|['\\'']{1})([a-zA-Z0-9\_]+)\\1/\\2/a,actions/'")
(defconst wp-ctags-actions-kinds "--PHP-kinds=+a-cidvfj")

;; capture apply_filters declarations
(defconst wp-ctags-filters-regex "'--regex-PHP=/apply_filters\\(\\s?([\"]{1}|['\\'']{1})([a-zA-Z0-9\_]+)\\1/\\2/h,filters/'")
(defconst wp-ctags-filters-kinds "--PHP-kinds=+h-cidvfj")

;; flags to be run, these are only modified when appending
(defvar wp-ctags-flags "-Ren")


;; ctags integration
(defun wp-ctags()
  "Returns t if it's possible to run ctags."
  (and (wp-exists)
       (not (string= "" wp-ctags-executable))
       (file-executable-p wp-ctags-executable)))

(defun wp-ctags-destination(&optional ctags-file-name)
  "Returns the destination for the projects tags files, optionally
concatenates it with `ctags-file-name'."
  (if (wp-ctags)
      (concat wp-ctags-directory "/" (s-replace "/" "_" (wp-exists)) ctags-file-name)))

(defun wp--run-ctags(outfile path type &optional append)
  "Assembles the ctags command to be run with `shell-command'.

This append business is foolish, assuming the user never re-builds the tags
tables, after many saves they will grow to unusable sizes. (albeit, MANY saves)

The ideal for performance is what we do, just call --append with ctags, on only
the file being saved. The issue is --append will continue to append new tags, infinitely
many times, so do_action('foo') would appear n times, n being 1(initial build) + number of times files saved with that do_action.

Perhaps force a re-build over time? Inconvenient, but I'm sure IDEs rebuild all the time. Perhaps re-build over time async.

`type' is anything, and the ctags command is built based on `wp-ctags-TYPE-regex' and/or `wp-ctags-TYPE-kinds' being defined."
  (when (wp-ctags)
    (shell-command (format "%s %s %s %s -o %s %s"
                           wp-ctags-executable ;; ctags
                           wp-ctags-language ;; --languages=PHP
                           (concat
                            (if (boundp (intern (format "wp-ctags-%s-regex" type)))
                                (eval (intern (format "wp-ctags-%s-regex" type)))) ;; --regex-PHP=...
                            " "
                            (if (boundp (intern (format "wp-ctags-%s-kinds" type)))
                                (eval (intern (format "wp-ctags-%s-kinds" type))))) ;; --PHP-kinds=...
                           (concat
                            wp-ctags-flags
                            " "
                            (if append
                                " --append")) ;; -Ren (--append)
                           outfile ;; WPTAGS or something similar
                           path)))) ;; project_dir or file.php

(defun wp-ctags-build-tables()
  "Builds all of the ctags tables, regardless of whether or not
they exist."
  (interactive)
  (when (wp-ctags)
    (wp--run-ctags (wp-ctags-destination "WPTAGS") (wp-exists) "generic")))

(defun wp-get-tag-names(type)
  "Gets the names of all tags within the tag file."
  (when (string= type "all")
    (visit-tags-table (wp-ctags-destination "WPTAGS"))
    (visit-tags-table (wp-ctags-destination "WPACTIONTAGS"))
    (visit-tags-table (wp-ctags-destination "WPFILTERSTAGS")))
  (when (string= type "generic")
    (visit-tags-table (wp-ctags-destination "WPTAGS")))
  (when (string= type "actions")
    (visit-tags-table (wp-ctags-destination "WPACTIONTAGS")))
  (when (string= type "filters")
    (visit-tags-table (wp-ctags-destination "WPFILTERSTAGS")))

  (let ((tag-names '()))
    (mapatoms (lambda(ob)
                (push (symbol-name ob) tag-names)) (tags-completion-table))
    tag-names))

(defun wp-goto-declaration()
  (interactive)
  (find-tag (symbol-name (symbol-at-point))))

(defun wp-find-usages()
  (interactive)
  (tags-search (symbol-name (symbol-at-point))))

(provide 'wordpress-ctags)
