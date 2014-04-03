(defcustom wp-cli-executable "/usr/bin/wp"
  "Path to PHP for calling WordPress functions.")

(defcustom wp-cli-buf-name "*wp-cli-output*"
  "Buffer to use to pass WP CLI commands through.")

;; disabling these commands until we can find a decent way of integrating them
(setq wp-disabled-cli-commands '("cli" "help" "shell" "export"))

(defun wp-cli()
  "Returns t if WP CLI commands are capable of being run."
  (and (wp-exists)
       (not (string= "" wp-cli-executable))
       (file-executable-p wp-cli-executable)
       wp-cli-buf-name))

(defun wp-cli-shell-command-to-str(command)
  "Returns the output of running a shell command while logging the input
and output to `wp-cli-buf-name'."
  (when (wp-cli)
    (let ((output (shell-command-to-string command)))
      (with-current-buffer (get-buffer-create wp-cli-buf-name)
        (insert (format "\n>%s\n" command))
        (insert output))
      output)))

(defun wp-cli-commands()
  "Returns all commands available in the wp cli completions command
that are not present in `wp-disabled-cli-commands'.

Returns as a list of lists, such as:
( (\"command\" \"subcommand1\" \"subcommand2\")  ..)"
  (when (wp-cli)
    (-filter (lambda(cmd-parts)
               (not (-contains? wp-disabled-cli-commands (car cmd-parts))))
             (mapcar (lambda(arg)
                       (let ((cmd-line (s-split " " arg t)))
                         `(,(first cmd-line) . ,(-remove-at 0 cmd-line))))
                     (s-split "\n" (shell-command-to-string (format "%s cli completions" wp-cli-executable)) t)))))

(defun wp-cli-subcommands(command)
  "Takes a string such as 'cache' and returns all subcommands from `wp-cli-commands'. "
  (when (wp-cli)
    (-drop 1 (car (-filter (lambda(cmd-parts)
                             (string-equal (car cmd-parts) command)) (wp-cli-commands))))))


(defun wp-cli-ido()
  "Prompts the user using ido for all top level WP CLI commands, upon completion it uses
`wp-cli-subcommands' to give another ido prompt for that commands subcommands.

It then lets the user input the rest of the arguments needed (if any) and shows the output
using `wp-cli-shell-command-to-str'."
  (interactive)
  (if (wp-cli)
      (let ((command (ido-completing-read "WP CLI: " (mapcar (lambda(cmd-parts)
                                                               (car cmd-parts)) (wp-cli-commands)))))
        (if (not (wp-cli-subcommands command))
            (message (wp-cli-shell-command-to-str (format "%s %s %s" wp-cli-executable command (read-string command))))
          (let ((subcommand (ido-completing-read (format "%s " command) (wp-cli-subcommands command))))
            (with-wp-cli-mode-line-usage `(,command ,subcommand)
                                         (wp-cli-raw-actions command
                                                             subcommand
                                                             (read-string (concat (s-join " " `("wp" ,command ,subcommand)))))))))
    (message "WP CLI not enabled.")))

(defun wp-cli-raw-actions(&rest args)
  "Takes n args such as 'cache' 'add' 'foo' 'bar' and runs them with `wp-cli-shell-command-to-str'
as 'wp cache add foo bar'."
  (message (wp-cli-shell-command-to-str (format "%s %s" wp-cli-executable (s-join " " args)))))

;; hash table of '("user" "add-cap") => "<user> <cap>"
(setq wp-cli-usage-hash (make-hash-table :test 'equal
                                         :size 200))

(defun wp-cli-build-usage-hash()
  (dolist (cmd (wp-cli-top-level-commands))
    (let ((usage-str (shell-command-to-string (format "%s %s" wp-cli-executable cmd))))
      (dolist (usage-line (s-split "\n" usage-str))
        (when (string-match (format "%s \\([a-zA-Z\_\-]+\\) \\(.*\\)" cmd) usage-line)
          (puthash `(,cmd ,(match-string 1 usage-line)) (match-string 2 usage-line) wp-cli-usage-hash))))))

(defun wp-cli-top-level-commands()
  (mapcar (lambda(arg)
            (car arg)) (wp-cli-commands)))

(defun wp-cli-usage(commands)
  (gethash commands wp-cli-usage-hash))

(defmacro with-wp-cli-mode-line-usage(commands &rest body)
  "Just executes body with the WP CLI Usage line as the mode-line
while in the minibuffer.

This is an awful hack, and really needs to be replaced with either an extension of
eldoc (eldoc-eval maybe) or totally different interaction, such as mag-menu."
  (declare (indent 4) (debug t))
  `(let ((orig-mode-line mode-line-format))
     (setq mode-line-format (wp-cli-usage ,commands))
     ,@body
     (setq mode-line-format orig-mode-line)))

(provide 'wordpress-cli)
