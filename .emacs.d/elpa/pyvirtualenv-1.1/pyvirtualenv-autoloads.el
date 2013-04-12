;;; pyvirtualenv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (pyvirtualenv pyvirtualenv-mode) "pyvirtualenv"
;;;;;;  "pyvirtualenv.el" (20834 18224 13125 611000))
;;; Generated autoloads from pyvirtualenv.el

(defvar pyvirtualenv-mode nil "\
Non-nil if Pyvirtualenv mode is enabled.
See the command `pyvirtualenv-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pyvirtualenv-mode'.")

(custom-autoload 'pyvirtualenv-mode "pyvirtualenv" nil)

(autoload 'pyvirtualenv-mode "pyvirtualenv" "\
Minor mode providing easy interface to Python's pyvirtualenvs.

\\{pyvirtualenv-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'pyvirtualenv "pyvirtualenv" "\
Switch to pyvirtualenv PYVIRTUALENV.

If PYVIRTUALENV is an existing directory, it is assumed to be the
location of an existing virtual environment. If it does not
exist, it's created as a new virtual environment, and activated.

If the argument is nil, or when a prefix argument is given, all
changes to the environment are removed.

NOTE: Both Pymacs and any inferior Python shell will be
unaffected by this until you restart them. Doing so automatically
might lose data, so we avoid that.

\(fn PYVIRTUALENV)" t nil)

;;;***

;;;### (autoloads nil nil ("pyvirtualenv-pkg.el") (20834 18224 21892
;;;;;;  710000))

;;;***

(provide 'pyvirtualenv-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pyvirtualenv-autoloads.el ends here
