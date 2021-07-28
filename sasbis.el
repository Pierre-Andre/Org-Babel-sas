;;; sasbis.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Pierre-André Cornillon
;;
;; Author: Pierre-André Cornillon <https://github.com/pac>
;; Maintainer: Pierre-André Cornillon <pierre-andre.cornillon@univ-rennes2.fr>
;; Created: avril 28, 2021
;; Modified: avril 28, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'comint)
(require 'tramp-sh)

(defcustom sasbis-shell-interpreter "sas"
  "Default Sas interpreter for shell."
  :type 'string
  :group 'sasbis)
(defcustom sasbis-log-separated 't
  "If nil SAS buffer will contains LOG and Output."
  :type 'string
  :group 'sasbis)
(defcustom sasbis-shell-interpreter-args "-nodms -nonews -stdio -nofullstimer -nodate -nocenter -terminal -pagesize max -nosyntaxcheck"
  "Default arguments for the Sas interpreter."
  :type 'string
  :group 'sasbis)

(defcustom sasbis-shell-buffer-name "Sas"
  "Default buffer name for Sas interpreter."
  :type 'string
  :group 'sasbis
  :safe 'stringp)

;; The next two are ``the inside of [...] in a regexp'' to be used in
;; (skip-chars-(for|back)ward SAS-..-chars)
(defcustom sas-white-chars " \t\n\f"
  "This does NOT escape blanks (RMH, 2000/03/20)."
  :group 'sasbis
  :type  'string)

(defcustom sas-comment-chars (concat sas-white-chars ";")
  "Doc?"
  :group 'sasbis
  :type  'string)

(defun make-comint-in-buffer-std (name buffer program &optional startcommand stderr &rest switches)
"Make a Comint process NAME in BUFFER, running PROGRAM.
If BUFFER is nil, it defaults to NAME surrounded by `*'s.
If there is a running process in BUFFER, it is not restarted.

PROGRAM should be one of the following:
- a string, denoting an executable program to create via
  `start-file-process'
- a cons pair of the form (HOST . SERVICE), denoting a TCP
  connection to be opened via `open-network-stream'
- nil, denoting a newly-allocated pty.

Optional fourth arg STARTCOMMAND is string whose
contents are sent to the process as its initial input.
Optional fifth arg STDERR is a buffer for standard error.
SWITCHES are PROGRAM switches.

If PROGRAM is a string, any more args are arguments to PROGRAM.

Return the (possibly newly created) process buffer."
  (or (fboundp 'start-file-process)
      (error "Multi-processing is not supported for this system"))
  (setq buffer (get-buffer-create (or buffer (concat "*" name "*"))))
  ;; If no process, or nuked process, crank up a new one and put buffer in
  ;; comint mode.  Otherwise, leave buffer and existing process alone.
  (unless (comint-check-proc buffer)
    (with-current-buffer buffer
      (unless (derived-mode-p 'comint-mode)
        (comint-mode))) ; Install local vars, mode, keymap, ...
    (comint-exec-std buffer name program startcommand stderr switches))
  buffer)

(defun comint-exec-std (buffer name command startcommand stderr switches)
"Start up a process named NAME in buffer BUFFER for Comint modes.
Run the given COMMAND with SWITCHES, initial input
from STARTCOMMAND and standard error from STDERR.

COMMAND should be one of the following:
- a string, denoting an executable program to create via
  `start-file-process'
- a cons pair of the form (HOST . SERVICE), denoting a TCP
  connection to be opened via `open-network-stream'
- nil, denoting a newly-allocated pty.

This function blasts any old process running in the buffer, and
does not set the buffer mode.  You can use this to cheaply run a
series of processes in the same Comint buffer.  The hook
`comint-exec-hook' is run after each exec."
  (with-current-buffer buffer
    (let ((proc (get-buffer-process buffer)))	; Blast any old process.
      (if proc (delete-process proc)))
    ;; Crank up a new process
    (let ((proc
           (if (consp command)
               (open-network-stream name buffer (car command) (cdr command))
             (comint-exec-1-std name buffer command stderr switches))))
      (set-process-filter proc 'comint-output-filter)
      (setq-local comint-ptyp process-connection-type) ; t if pty, nil if pipe.
      ;; Jump to the end, and set the process mark.
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      (cond (startcommand
        (sleep-for 1)
	     (goto-char (point-max))
          (comint-send-string proc startcommand)))
      (run-hooks 'comint-exec-hook)
      buffer)))

(defun comint-exec-1-std (name buffer command stderr switches)
"Same function as `comint-exec-1' but with STDERR argument.
  STDERR is a buffer that will be used as standard error of process (see `make-process')"
  (let ((process-environment
         (nconc
          (comint-term-environment)
          (list (format "INSIDE_EMACS=%s,comint" emacs-version))
          process-environment))
        (default-directory
          (if (file-accessible-directory-p default-directory)
              default-directory
            "/"))
        proc decoding encoding changed)
    (let ((exec-path (if (and command (file-name-directory command))
                         ;; If the command has slashes, make sure we
                         ;; first look relative to the current directory.
                         (cons default-directory exec-path) exec-path)))
      (setq proc (apply 'start-file-process-std name buffer command stderr switches)))
    ;; Some file name handler cannot start a process, fe ange-ftp.
    (unless (processp proc) (error "No process started"))
    (let ((coding-systems (process-coding-system proc)))
      (setq decoding (car coding-systems)
            encoding (cdr coding-systems)))
    ;; Even if start-file-process left the coding system for encoding data
    ;; sent from the process undecided, we had better use the same one
    ;; as what we use for decoding.  But, we should suppress EOL
    ;; conversion.
    (if (and decoding (not encoding))
        (setq encoding (coding-system-change-eol-conversion decoding 'unix)
              changed t))
    (if changed
        (set-process-coding-system proc decoding encoding))
    proc))

(defun start-file-process-std (name buffer program stderr &rest program-args)
"Start a program in a subprocess.  Return the process object for it.

Similar to `start-process', but may invoke a file name handler based on
`default-directory'.  See Info node `(elisp)Magic File Names'.

This handler ought to run PROGRAM, perhaps on the local host,
perhaps on a remote host that corresponds to `default-directory'.
In the latter case, the local part of `default-directory', the one
produced from it by `file-local-name', becomes the working directory
of the process on the remote host.

PROGRAM and PROGRAM-ARGS might be file names.  They are not
objects of file name handler invocation, so they need to be obtained
by calling `file-local-name', in case they are remote file names.

STDERR is a buffer which will be used as standard error of process (see `make-process')

File name handlers might not support pty association, if PROGRAM is nil."
  (let ((fh (find-file-name-handler default-directory 'start-file-process-std)))
    (if fh (apply fh 'start-file-process-std name buffer program stderr program-args)
      (apply 'start-process-std name buffer program stderr program-args))))

(defun start-process-std (name buffer program stderr &rest program-args)
"Start a program in a subprocess.  Return the process object for it.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer name) to associate with the process.

Process output (both standard output and standard error streams)
goes at end of BUFFER, unless you specify a filter function to
handle the output.  BUFFER may also be nil, meaning that this
process is not associated with any buffer.

PROGRAM is the program file name.  It is searched for in `exec-path'
\(which see).  If nil, just associate a pty with the buffer.  Remaining
arguments PROGRAM-ARGS are either strings to give program as arguments or
a plist (:stderr \"*buffer name of stderr*\" :switches (\"-l\" \"-a\"))

STDERR is a buffer for separate standard error from standard output:
if nil standard error is in BUFFER ; if it is a buffer this will receive standard error

The process runs in `default-directory' if that is local (as
determined by `unhandled-file-name-directory'), or \"~\"
otherwise.  If you want to run a process in a remote directory
use `start-file-process'."
  (unless (fboundp 'make-process)
    (error "Emacs was compiled without subprocess support"))
  (apply #'make-process
         (append (list :name name :buffer buffer)
                 (if program
                     (if stderr
                         (list :command (cons program program-args)
                               :stderr stderr)
                       (list :command (cons program program-args)))
                   )))  )

(defun run-sasbis (&optional cmd dedicated show)
"Run an inferior Sas process.

Argument CMD defaults to `sasbis-shell-calculate-command' return
value.  When called interactively with `prefix-arg', it allows
the user to edit such value and choose whether the interpreter
should be DEDICATED for the current buffer.  When numeric prefix
arg is other than 0 or 4 do not SHOW.

For a given buffer and same values of DEDICATED, if a process is
already running for it, it will do nothing.  This means that if
the current buffer is using a global process, the user is still
able to switch it to use a dedicated one.

Runs the hook `inferior-sasbis-mode-hook' after
`comint-mode-hook' is run.  (Type \\[describe-mode] in the
process buffer for a list of commands.)"
  (interactive
   (if current-prefix-arg
       (list
        (read-shell-command "Run Sas: " (sasbis-shell-calculate-command))
        (y-or-n-p "Make dedicated process? ")
        (= (prefix-numeric-value current-prefix-arg) 4))
     (list (sasbis-shell-calculate-command) nil t)))
  (let ((buffer
         (sasbis-shell-make-comint
          (or cmd (sasbis-shell-calculate-command))
          (sasbis-shell-get-process-name dedicated) dedicated show)))
    (pop-to-buffer buffer)
    (get-buffer-process buffer)))

(defun sasbis-shell-calculate-command ()
"Calculate the string used to execute the inferior Sas process."
  (format "%s %s"
          ;; `sasbis-shell-make-comint' expects to be able to
          ;; `split-string-and-unquote' the result of this function.
          (combine-and-quote-strings (list sasbis-shell-interpreter))
          sasbis-shell-interpreter-args))

(defmacro sasbis-shell-with-environment (&rest body)
"Modify shell environment during execution of BODY.
Temporarily sets `process-environment' and `exec-path' during
execution of body.  If `default-directory' points to a remote
machine then modifies `tramp-remote-process-environment' and
`sasbis-shell-remote-exec-path' instead."
  (declare (indent 0) (debug (body)))
  (let ((vec (make-symbol "vec")))
    `(progn
       (let* ((,vec
               (when (file-remote-p default-directory)
                 (ignore-errors
                   (tramp-dissect-file-name default-directory 'noexpand))))
              (process-environment
               (if ,vec
                   process-environment
                 (sasbis-shell-calculate-process-environment)))
              (exec-path
               (if ,vec
                   exec-path
                 (sasbis-shell-calculate-exec-path)))
              (tramp-remote-process-environment
               (if ,vec
                   (sasbis-shell-calculate-process-environment)
                 tramp-remote-process-environment)))
         (when (tramp-get-connection-process ,vec)
           ;; For already existing connections, the new exec path must
           ;; be re-set, otherwise it won't take effect.  One example
           ;; of such case is when remote dir-locals are read and
           ;; *then* subprocesses are triggered within the same
           ;; connection.
           (sasbis-shell-tramp-refresh-remote-path
            ,vec (sasbis-shell-calculate-exec-path))
           ;; The `tramp-remote-process-environment' variable is only
           ;; effective when the started process is an interactive
           ;; shell, otherwise (like in the case of processes started
           ;; with `process-file') the environment is not changed.
           ;; This makes environment modifications effective
           ;; unconditionally.
           (sasbis-shell-tramp-refresh-process-environment
            ,vec tramp-remote-process-environment))
         ,(macroexp-progn body)))))
(defmacro sasbis-shell--add-to-path-with-priority (pathvar paths)
"Modify PATHVAR and ensure PATHS are added only once at beginning."
  `(dolist (path (reverse ,paths))
     (cl-delete path ,pathvar :test #'string=)
     (cl-pushnew path ,pathvar :test #'string=)))

(defun sasbis-shell-get-process-name (dedicated)
"Calculate the appropriate process name for inferior Sas process.
If DEDICATED is t returns a string with the form
`sasbis-shell-buffer-name'[`buffer-name'] else returns the value
of `sasbis-shell-buffer-name'."
  (if dedicated
      (format "%s[%s]" sasbis-shell-buffer-name (buffer-name))
    sasbis-shell-buffer-name))
(defun sasbis-shell-get-errorbuffer-name (dedicated)
"Calculate the appropriate  name for error bufffer .
If DEDICATED is t returns a string with the form
Log`sasbis-shell-buffer-name'[`buffer-name'] else returns the value
of `sasbis-shell-buffer-name'."
  (if dedicated
      (format "Log-%s[%s]" sasbis-shell-buffer-name (buffer-name))
   (format "Log-%s"  sasbis-shell-buffer-name)))

(defun sasbis-shell-make-comint (cmd proc-name &optional dedicated  show internal)
"Create a Sas shell comint buffer.
CMD is the Sas command to be executed and PROC-NAME is the
process name the comint buffer will get.  After the comint buffer
is created the `inferior-sasbis-mode' is activated. When
optional argument DEDICATED is non-nil it controls if the
 stderr buffer is dedicated. When
optional argument SHOW is non-nil the buffer is shown.  When
optional argument INTERNAL is non-nil this process is run on a
buffer with a name that starts with a space, following the Emacs
convention for temporary/internal buffers, and also makes sure
the user is not queried for confirmation when the process is
killed."
  (save-excursion
    (sasbis-shell-with-environment
     (let* ((proc-buffer-name
             (format (if (not internal) "*%s*" " *%s*") proc-name)))
       (when (not (comint-check-proc proc-buffer-name))
         (let* ((cmdlist (split-string-and-unquote cmd))
                (interpreter (car cmdlist))
                (args (cdr cmdlist))
                (bufstderr (if sasbis-log-separated
                             (get-buffer-create (sasbis-shell-get-errorbuffer-name dedicated))))
                (buffer (apply #'make-comint-in-buffer-std proc-name proc-buffer-name
                               interpreter nil bufstderr args))
                (sasbis-shell--parent-buffer (current-buffer))
                (process (get-buffer-process buffer))
                ;; Users can override the interpreter and args
                ;; interactively when calling `run-sasbis', let-binding
                ;; these allows having the new right values in all
                ;; setup code that is done in `inferior-sasbis-mode',
                ;; which is important, especially for prompt detection.
                (sasbis-shell--interpreter interpreter)
                (sasbis-shell--interpreter-args
                 (mapconcat #'identity args " ")))
           (if sasbis-log-separated (with-current-buffer bufstderr
             (inferior-sasbis-mode)))
           (with-current-buffer buffer
             (inferior-sasbis-mode))
            (when show (display-buffer buffer))
           (and internal (set-process-query-on-exit-flag process nil))))
       proc-buffer-name))))

(defun sasbis-shell-calculate-process-environment ()
"Calculate `process-environment' or `tramp-remote-process-environment'.
  If `default-directory' points to a remote host, the returned value is intended for `tramp-remote-process-environment'."
  (let* ((remote-p (file-remote-p default-directory))
         (process-environment (if remote-p
                                  tramp-remote-process-environment
                                process-environment)))
    process-environment))

(defun sasbis-shell-calculate-exec-path ()
"Calculate `exec-path'.
Prepends `sasbis-shell-exec-path'.  If `default-directory' points
to a remote host, the returned value appends
`sasbis-shell-remote-exec-path' instead of `exec-path'."
  (let ((new-path (copy-sequence
                   (if (file-remote-p default-directory)
                       sasbis-shell-remote-exec-path
                     exec-path))))
    (sasbis-shell--add-to-path-with-priority
     new-path sasbis-shell-exec-path)
    new-path))

(defcustom sasbis-shell-remote-exec-path nil
"List of paths to be ensured remotely for searching executables.
When this variable is non-nil, values are exported into remote
hosts PATH before starting processes.  Values defined in
`sasbis-shell-exec-path' will take precedence to paths defined
here.  Normally you wont use this variable directly unless you
plan to ensure a particular set of paths to all Sas shell
executed through tramp connections."
  :version "25.1"
  :type '(repeat string)
  :group 'sasbis)
(defcustom sasbis-shell-exec-path nil
"List of paths for searching executables.
When this variable is non-nil, values added at the beginning of
the PATH before starting processes.  Any values present here that
already exists in PATH are moved to the beginning of the list so
that they are prioritized when looking for executables."
  :type '(repeat string)
  :group 'sasbis)

;; (defvar sas-cli-file-path "/usr/local/bin/sas_u8"
;;   "Path to the program used by `run-sas'")
;; (defvar sas-cli-arguments '("-nodms" "-nonews" "-stdio"
;;                             "-nofullstimer" "-nodate" "-nocenter"
;;                             "-terminal" "-pagesize" "max"
;;                             "-nosyntaxcheck")
;;   "Commandline arguments to pass to `sas-cli'.")
;; to print sas options list add "-oplist" to sas-cli-arguments
(defvar sasbis-prompt-regexp "^"
"Prompt for `run-sasbis'.")
(defun sasbis--initialize ()
  "Helper function to initialize Sas"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode inferior-sasbis-mode comint-mode "Inferior sas"
 "Major mode for sas inferior process`run-sasbis'."
  nil "sasbis"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp sasbis-prompt-regexp)
  (setq font-lock-defaults
        ;; KEYWORDS KEYWORDS-ONLY CASE-FOLD .....
        '(sasbis-mode-font-lock-defaults nil t)))
;;  (set-syntax-table sasbis-mode-syntax-table))
;; this makes it read only; a contentious subject as some prefer the
;; buffer to be overwritable.
;; (setq comint-prompt-read-only t)
;; (setq comint-process-echoes t)
;; this makes it so commands like M-{ and M-} work.
;; (set (make-local-variable 'paragraph-separate) "\\'")
;; (set (make-local-variable 'font-lock-defaults) '(sasbis-font-lock-keywords t))
;; (set (make-local-variable 'paragraph-start) sasbis-prompt-regexp))

;; this has to be done in a hook. grumble grumble.
(add-hook 'inferior-sasbis-mode-hook 'sasbis--initialize)

(defvar sasbis-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r"   #'sasbis-shell-send-region)
    (define-key map "\C-c\C-b"   #'sasbis-shell-send-buffer)
    (define-key map "\C-c\C-j"   #'sasbis-shell-send-line)
    (define-key map [(control return)] #'sasbis-shell-send-dwim)
    (define-key map "\C-c\C-q"   #'sasbis-exit)
   map)
  "Keymap for `sasbis-mode'.")

(defun sasbis-shell-send-string (string &optional process msg)
"Send STRING to inferior Sas PROCESS.
When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive
   (list (read-string "Sas command: ") nil t))
  (let ((process (or process (sasbis-shell-get-process-or-error msg))))
      (comint-send-string process string)
      (when (not (string-match ".*\n[:blank:]*" string))
        (comint-send-string process "\n"))))

(defun sasbis-shell-send-region (start end &optional  msg)
"Send the region delimited by START and END to inferior Sas process.
When optional argument MSG is
non-nil, forces display of a user-friendly message if there's no
process running; defaults to t when called interactively."
  (interactive
   (list (region-beginning) (region-end) t))
  (let* ((string (buffer-substring-no-properties start end))
         (process (sasbis-shell-get-process-or-error msg))
         (_ (string-match "\\`\n*\\(.*\\)" string)))
    (message "Sent: %s..." (match-string 1 string))
    ;; Recalculate positions to avoid landing on the wrong line if
    ;; lines have been removed/added.
    ;; (with-current-buffer (process-buffer process)
    ;;  (compilation-forget-errors))
    (sasbis-shell-send-string string process)
    (deactivate-mark)))

(defun sasbis-shell-send-line (&optional  msg)
"Send the current line to the inferior ESS process.
to inferior Sas
process. When optional argument MSG is
non-nil, forces display of a user-friendly message if there's no
process running; defaults to t when called interactively."
 (interactive (list t))
  (let* ((start (point-at-bol))
         (end (point-at-eol))
         (string (buffer-substring-no-properties start end))
         (process (sasbis-shell-get-process-or-error msg))
         (_ (string-match "\\`\n*\\(.*\\)" string)))
    (message "Sent: %s..." (match-string 1 string))
    ;; Recalculate positions to avoid landing on the wrong line if
    ;; lines have been removed/added.
    ;; (with-current-buffer (process-buffer process)
    ;;  (compilation-forget-errors))
    (sasbis-shell-send-string string process)
    (deactivate-mark)))

(defun sasbis-shell-send-buffer (&optional msg)
"Send the entire buffer to inferior Sas process.
When optional argument MSG is
non-nil, forces display of a user-friendly message if there's no
process running; defaults to t when called interactively."
  (interactive (list t))
  (save-restriction
    (widen)
    (sasbis-shell-send-region (point-min) (point-max)  msg)))

(defun sasbis-shell-send-file (file-name &optional process msg)
"Send FILE-NAME to inferior Sas PROCESS.
When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running;
defaults to t when called interactively."
  (interactive
   (list
    (read-file-name "File to send: ")   ; file-name
    nil                                 ; process
    t))                                 ; msg
  (let* ((process (or process (sasbis-shell-get-process-or-error msg)))
         (file-name (file-local-name (expand-file-name file-name)))
         (string (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-string))))
    (sasbis-shell-send-string string process t)))

(defun sasbis-shell-send-exit (&optional process)
"Send \"endsas;\" to the Sas PROCESS."
  (interactive (list nil))
   (let* ((process (or process (sasbis-shell-get-process-or-error))))
    (sasbis-shell-send-string "endsas;\n" process)))

(defun sasbis-exit ()
"Send exit to Sas PROCESS, and close buffer."
  (interactive)
  (let* ((process (sasbis-shell-get-process-or-error))
         (name-buffer-sas (buffer-name (process-buffer process)))
         (name-buffer-saslog (concat "Log-" (substring name-buffer-sas 1 -1))))
    (sasbis-shell-send-exit process)
    ;; sits for a clean exit of Sas process
    (sleep-for 0 500)
    ;; kill buffer
    (if sasbis-log-separated
        (kill-buffer name-buffer-saslog))
    (kill-buffer name-buffer-sas)))

(defun sasbis-shell-send-dwim ()
"Send the region if selected if not try to send the block
proc/run or data/run."
  (interactive)
  (if (use-region-p)
      (sasbis-shell-send-region (region-beginning) (region-end) t)
    (let (begpos endpos nameproc)
      (save-excursion
        (setq nameproc (sasbis-beginning-of-sas-proc))
        (setq begpos (point))
        (message "begpos %s" begpos))
      (if (and nameproc (string-equal (downcase nameproc) "iml"))
          (sasbis-shell-send-line t)
          (progn
            (save-excursion
              (sasbis-end-of-sas-proc t nil)
              (setq endpos (point))
              (message "endpos %s" endpos))
            (sasbis-shell-send-region begpos endpos t))))))

(defun sasbis-shell-get-process-or-error (&optional interactivep)
"Return inferior Sas process for current buffer or signal error.
When argument INTERACTIVEP is non-nil, use `user-error' instead
of `error' with a user-friendly message."
  (or (sasbis-shell-get-process)
      (if interactivep
          (user-error
           "Start a Sas process first with `M-x run-sasbis' or `%s'."
           ;; Get the binding.
           (key-description
            (where-is-internal
             #'run-sasbis overriding-local-map t)))
        (error
         "No inferior Sas process running."))))
(defun sasbis-shell-get-process ()
 "Return inferior Sas process for current buffer."
  (get-buffer-process (sasbis-shell-get-buffer)))

(defun sasbis-shell-get-buffer ()
"Return inferior Sas buffer for current buffer.
If current buffer is in `inferior-sasbis-mode', return it."
  (if (derived-mode-p 'inferior-sasbis-mode)
      (current-buffer)
    (let* ((dedicated-proc-name (sasbis-shell-get-process-name t))
           (dedicated-proc-buffer-name (format "*%s*" dedicated-proc-name))
           (global-proc-name  (sasbis-shell-get-process-name nil))
           (global-proc-buffer-name (format "*%s*" global-proc-name))
           (dedicated-running (comint-check-proc dedicated-proc-buffer-name))
           (global-running (comint-check-proc global-proc-buffer-name)))
      ;; Always prefer dedicated
      (or (and dedicated-running dedicated-proc-buffer-name)
          (and global-running global-proc-buffer-name)))))

(eval-and-compile
  (defun sasbis-syntax--context-compiler-macro (form type &optional syntax-ppss)
    (pcase type
      (''comment
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 4 ppss) (nth 8 ppss))))
      (''string
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 3 ppss) (nth 8 ppss))))
      (''paren
       `(nth 1 (or ,syntax-ppss (syntax-ppss))))
      (_ form))))
(defun sasbis-syntax-context (type &optional syntax-ppss)
"Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be `comment', `string' or `paren'.  It returns the start
character address of the specified TYPE."
  (declare (compiler-macro sasbis-syntax--context-compiler-macro))
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (pcase type
      ('comment (and (nth 4 ppss) (nth 8 ppss)))
      ('string (and (nth 3 ppss) (nth 8 ppss)))
      ('paren (nth 1 ppss))
      (_ nil))))

(defun sasbis-beginning-of-sas-statement ()
"Move point to beginning of current sas statement."
  (interactive)
  (if (re-search-backward ";[ \n\t]*" (point-min) t)
      (if (sasbis-syntax-context 'comment)
          (sasbis-beginning-of-sas-statement)
        (progn
          (if (looking-at ";\n")
              (forward-char 2)
            (forward-char 1))
          (skip-chars-forward sas-white-chars)))
    (goto-char (point-min))))

(defun sasbis-end-of-sas-statement ()
"Move point to beginning of current sas statement."
  (interactive)
  (if (search-forward ";" nil t)
      (if (sasbis-syntax-context 'comment)
          (sasbis-end-of-sas-statement))
    (goto-char (point-max))))

(defun sasbis-beginning-of-sas-proc (&optional redo)
"Move point to the beginning of sas proc, macro or data step.
Optional argument REDO (when non-nil) allows
to skip the first displacement to the end of statement."
  (interactive)
  (if (not redo)
      (sasbis-end-of-sas-statement))
  (let (nameproc (case-fold-search t))
(if (re-search-backward "[ \t\n]+proc[ \t\n]\\|[ \t\n]+data[ \t\n]+\\|[ \t\n]+%macro[ \t\n]*" (point-min) t)
    (if (sasbis-syntax-context 'comment)
        (sasbis-beginning-of-sas-proc t))
  (goto-char (point-min)))
(if (looking-at "[ \t\n]+proc[ \t\n]+\\([A-Za-z]+\\)")
        (setq nameproc (match-string 1)))
      (skip-chars-forward sas-white-chars)
    (concat nameproc "")))

(defun sasbis-end-of-sas-proc (&optional plusone redo)
"Move point to end of sas proc, macro or data step.
If PLUSONE is non-nil point is moved forward of one char.
Optional argument REDO (when non-nil) allows
to skip the first displacement to the end of statement."
  (interactive (list t nil))
  (if (not redo)
      (progn
        (sasbis-beginning-of-sas-statement)
        (forward-char -1)))
  (let ((case-fold-search t))
    (if (re-search-forward "[ \t\n]+run[ \t\n]*;\\|%mend[ \t\n]+[a-z_0-9]+[ \t\n]*;\\|%mend[ \t\n]*;" (point-max) t)
        (if (sasbis-syntax-context 'comment)
            (sasbis-end-of-sas-proc nil t)
          (if plusone
              (forward-char 1)))
      (goto-char (point-max)))))

(defun sasbis-next-sas-proc (arg)
"Move point to beginning of next sas proc, macro or data step.
The optional argument ARG is a number that indicates the
  search direction and the number of occurrences to search for.  If it
  is positive, search forward for COUNT successive occurrences; if it
  is negative, search backward, instead of forward, for -COUNT
  occurrences.  A value of nil means the same as 1."
  (interactive "P")
  (let ((case-fold-search t))
    (forward-char 1)
    (if (re-search-forward
         "^[ \t]*\\(data[ ;]\\|proc[ ;]\\|endsas[ ;]\\|g?options[ ;]\\|%macro[ ;]\\)"
         nil t arg)
      (if (sasbis-syntax-context 'comment)  (sasbis-next-sas-proc))
        (sasbis-beginning-of-sas-statement 1)
      (forward-char -1))))

(defcustom ess-sasbis-tab-stop-list
  '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)
  "List of tab stop positions used by `tab-to-tab-stop' in sasbis-mode."
  :type '(repeat integer)
  :group 'sasbis-mode)

(defvar sasbis-mode-syntax-table
  (let ((tab (make-syntax-table)))
    (modify-syntax-entry ?\\ "."  tab)  ;; backslash is punctuation
    (modify-syntax-entry ?+  "."  tab)
    (modify-syntax-entry ?-  "."  tab)
    (modify-syntax-entry ?=  "."  tab)
    (modify-syntax-entry ?%  "w"  tab)
    (modify-syntax-entry ?<  "."  tab)
    (modify-syntax-entry ?>  "."  tab)
    (modify-syntax-entry ?&  "w"  tab)
    (modify-syntax-entry ?|  "."  tab)
    (modify-syntax-entry ?\' "\"" tab)
    (modify-syntax-entry ?*  ". 23"  tab) ; comment character
    (modify-syntax-entry ?\; "."  tab)
    (modify-syntax-entry ?_  "w"  tab)
    (modify-syntax-entry ?<  "."  tab)
    (modify-syntax-entry ?>  "."  tab)
    (modify-syntax-entry ?/  ". 14"  tab) ; comment character
    (modify-syntax-entry ?.  "w"  tab)
    tab)
  "Syntax table for `sasbis-mode'.")

(defvar sasbis-mode-font-lock-comment01
  (list
  ;; .log NOTE: messages
       (cons "^NOTE [0-9]+-[0-9]+: Line generated by the invoked macro"
             font-lock-comment-face)
       (cons "^NOTE: .*$"                          font-lock-comment-face)
       (cons "^      [^ @].*[.]$"                   font-lock-comment-face)
       (cons "^      [a-z].*[a-z][ ]?$"            font-lock-comment-face)
       (cons "^      Engine:[ ]+V.+$"              font-lock-comment-face)
       (cons "^      Physical Name:[ ]+.+$"        font-lock-comment-face)
       (cons "^      \\(cpu\\|real\\) time[ ]+[0-9].*$"
             font-lock-comment-face)
       (cons "^      decimal may be shifted by the"
             font-lock-comment-face)
       (cons "^NOTE: The infile "                  font-lock-comment-face)
       (cons "^NOTE: 1 record was read from the infile "
             font-lock-comment-face)
       (cons "^NOTE: [1-9][0-9]* records were read from the infile "
             font-lock-comment-face)
       (cons "^      Filename=.*,$"                font-lock-comment-face)
       (cons "^      File Name=.*,$"               font-lock-comment-face)
       (cons "^      File $"                       font-lock-comment-face)
       (cons "^      Name=.*,$"                    font-lock-comment-face)
       (cons "^      File List=("                  font-lock-comment-face)
       (cons "^      List=("                       font-lock-comment-face)
       (cons "^      Owner Name=.*,$"              font-lock-comment-face)
       (cons "^      Access Permission=.*,$"       font-lock-comment-face)
       (cons "^      Last Modified=.*,?$"          font-lock-comment-face)
       (cons "^      File Size (bytes)=[0-9]+$"    font-lock-comment-face)
       (cons "^      Pipe command="                font-lock-comment-face)
       (cons "^NOTE: The file "                    font-lock-comment-face)
       (cons "^NOTE: 1 record was written to the file "
             font-lock-comment-face)
       (cons "^NOTE: [1-9][0-9]* records were written to the file "
             font-lock-comment-face)
       (cons "^NOTE: PROC LOGISTIC is modeling the probability that"
             font-lock-comment-face)
       (cons "^NOTE: PROC GENMOD is modeling the probability that"
             font-lock-comment-face)
       ;; Sas system message
       (cons "^1[ ]+The SAS System.*$"             font-lock-comment-face)
       (cons "^1[ ]+Le Système SAS.*$"             font-lock-comment-face)
       (cons "^[ ]+SAS/ETS[ ]+[0-9]+\\.[0-9]+[ ]*$" font-lock-comment-face)
       ;; Sas module
       (cons "^[ ]+SAS/IML[ ]+[0-9]+\\.[0-9]+[ ]*$" font-lock-comment-face)
       (cons "^[ ]+SAS/OR[ ]+[0-9]+\\.[0-9]+[ ]*$" font-lock-comment-face)
       (cons "^[ ]+SAS/QC[ ]+[0-9]+\\.[0-9]+[ ]*$" font-lock-comment-face)
       (cons "^[ ]+SAS/STAT[ ]+[0-9]+\\.[0-9]+[ ]*$" font-lock-comment-face)
       ;; uname
        (cons "^[ ]+Linux LIN X64.*$" font-lock-comment-face)
        (cons "^\014.*$"                            font-lock-comment-face)
       (cons "[*][*][*] ANNOTATE macros are now available [*][*][*]"
             font-lock-comment-face)
       (cons "For further information on ANNOTATE macros, enter,"
             font-lock-comment-face)
       (cons "\\(or \\)?%HELPANO.*$"
             font-lock-comment-face)
       (cons "^Local Variables:$"                  font-lock-comment-face)
       (cons "^End:$"                              font-lock-comment-face)
       (cons "^MPRINT([_A-Z0-9]+)"                 font-lock-comment-face)
       ))

(defvar sasbis-mode-font-lock-errors02
  (list
       ;; .log ERROR: messages
                                        ;     (cons "^ERROR\\( [0-9]+-[1-9][0-9][0-9]\\)?: .*$"
       (cons "^ERROR\\( [0-9]+-[0-9]+\\)?: .*$"
             font-lock-keyword-face)
                                        ;       ERROR:
       (cons "^       [^ @].*\\([.][ ]?[ ]?\\|[,a-z][ ]\\)$"
             font-lock-keyword-face)
                                        ;       ERROR #-###:
       (cons "^             [^ @].*\\([.][ ]?[ ]?\\|[,a-z][ ]\\)$"
             font-lock-keyword-face)
                                        ;       ERROR ##-###:
       (cons "^              [^ @].*\\([.][ ]?[ ]?\\|[,a-z][ ]\\)$"
             font-lock-keyword-face)
                                        ;       ERROR ###-###:
       (cons "^               [^ @].*\\([.][ ]?[ ]?\\|[,a-z][ ]\\)$"
             font-lock-keyword-face)
       (cons "^              a format name."       font-lock-keyword-face)
       (cons "^       where a numeric operand is required. The condition was: "
             font-lock-keyword-face)
       (cons "[ ][_]+$"                            font-lock-keyword-face)))

(defvar sasbis-mode-font-lock-warnings03
  (list
   ;; .log WARNING: messages
                                        ;(cons "^WARNING\\( [0-9]+-[1-9][0-9][0-9]\\)?: .*$"
       (cons "^WARNING\\( [0-9]+-[0-9]+\\)?: .*$"
             font-lock-function-name-face)
                                        ;       WARNING:
       (cons "^         [^ @].*\\([.][ ]?[ ]?\\|[,a-z][ ]\\)$"
             font-lock-function-name-face)
                                        ;       WARNING #-###:
       (cons "^               [^ @].*\\([.][ ]?[ ]?\\|[,a-z][ ]\\)$"
             font-lock-function-name-face)
                                        ;       WARNING ##-###:
       (cons "^                [^ @].*\\([.][ ]?[ ]?\\|[,a-z][ ]\\)$"
             font-lock-function-name-face)
                                        ;       WARNING ###-###:
       (cons "^                 [^ @].*\\([.][ ]?[ ]?\\|[,a-z][ ]\\)$"
             font-lock-function-name-face)

       ;; SAS comments
       ;; /* */ style handled by grammar above
       (cons "\\(^[0-9]*\\|[:;!]\\)[ \t]*%?\\*[^;/][^;]*;"
             font-lock-comment-face)))

(defvar sasbis-mode-font-lock-override04
  (list
                                            ; these over-rides need to come before the more general declarations
       (cons "\\<and("      font-lock-function-name-face)
       (cons "\\<data="     font-lock-keyword-face)
       (cons "\\<in:("      font-lock-function-name-face)
       (cons "\\<index("    font-lock-function-name-face)
       (cons "\\<input("    font-lock-function-name-face)
       (cons "\\<libname("  font-lock-function-name-face)
       (cons "\\<not("      font-lock-function-name-face)
       (cons "\\<or("       font-lock-function-name-face)
       (cons "\\<put("      font-lock-function-name-face)
       (cons "\\<sum("      font-lock-function-name-face)

                                        ; other idiosyncratic keywords
                                        ;(cons "key="      font-lock-keyword-face)
                                        ;(cons "/unique"   font-lock-keyword-face)
))

(defvar sasbis-mode-font-lock-execblocks05
  (list
  ;; SAS execution blocks: DATA, %MACRO/%MEND, %DO/%END, etc.
       (cons (regexp-opt '(
                           "data" "start" "return" ;"proc"
                           "%macro" "%mend"
                           "%do" "%to" "%by" "%end"
                           "%goto" "%go to"
                           "%if" "%then" "%else"
                           "%global" "%inc" "%include" "%input" "%local" "%let" "%put" "%sysexec"
                           ) 'words) font-lock-constant-face)
 ;; SAS execution blocks that must be followed by a semi-colon
       (cons (concat "\\<"
                     (regexp-opt
                      '(
                        "run;" "quit;" "endsas;" "finish;"
                        "cards;" "cards4;" "datalines;" "datalines4;" "lines;" "lines4;"
                        )))
             font-lock-constant-face)))

(defvar sasbis-mode-font-lock-statements06
  (list
       ;; SAS statements that must be followed by a semi-colon
       (cons (concat "\\<"
                     (regexp-opt
                      '(
                        "end;" "list;" "lostcard;" "page;" "stop;" ;"return;"
                        )))
             font-lock-keyword-face)

       ;; SAS statements that must be followed by an equal sign
       (cons (concat "\\<"
                     (regexp-opt
                      '(
                        "compress=" "in=" "out=" "sortedby="
                        )))
             font-lock-keyword-face)
   ))

(defvar sasbis-mode-font-lock-procname07
  (list
    ;; SASBIS procedure names
       (cons (concat "\\<proc[ ]+"
                     (regexp-opt '(
                                   ;; SAS base and SAS/Graph
                                   "append"
                                   "calendar" "catalog" "chart" "cimport" "cport" "compare" "contents" "copy" "corr"
                                   "datasets" "dbcstab" "display"
                                   "explode" "export"
                                   "fcmp" "format" "forms" "freq" "fsbrowse" "fsedit" "fsletter" "fslist" "fsview"
                                   "ganno" "gchart" "gcontour" "gdevice" "geocode" "gfont" "gimport" "ginside"
                                   "gkeymap" "gmap" "goptions" "gplot" "gprint" "gproject" "greduce" "gremove"
                                   "greplay" "gslide" "gtestit" "g3d" "g3grid"
                                   "iml" "import" "insight"
                                   "mapimport" "means"
                                   "options"
                                   "plot" "pmenu" "print" "printto"
                                   "rank" "registry" "report"
                                   "setinit" "sgdesign" "sgmap"
                                   "sgpanel" "sgplot" "sgrender" "sgscatter" "sort" "sql" "standard" "summary"
                                   "tabulate" "template" "timeplot" "transpose" "trantab"
                                   "univariate"

                                   ;;SAS/Stat and SAS/ETS
                                   "aceclus" "anova" "arima" "autoreg"
                                   "bgenmod" "blifereg" "boxplot" "bphreg"
                                   "calis" "cancorr" "candisc" "catmod" "citibase" "cluster" "computab" "corresp" "countreg"
                                   "discrim" "distance"
                                   "entropy" "expand"
                                   "factor" "fastclus" "forecast"
                                   "gam" "gee" "genmod" "glimmix" "glm" "glmmod" "glmpower" "glmselect"
                                   "hpmixed"
                                   "inbreed"
                                   "kde" "krige2d"
                                   "lattice" "lifereg" "lifetest" "loess" "logistic"
                                   "mcmc" "mdc" "mds" "mi" "mianalyze" "mixed" "modeclus" "model" "mortgage" "multtest"
                                   "nested" "nlin" "nlmixed" "npar1way"
                                   "orthoreg"
                                   "panel" "pdlreg" "phreg" "plan" "plm" "pls" "power" "princomp" "prinqual" "probit"
                                   "qlim" "quantreg"
                                   "reg" "risk" "robustreg" "rsreg"
                                   "score" "seqdesign" "seqtest" "severity" "sim2d" "similarity" "simlin" "simnormal"
                                   "spectra" "statespace" "stdize" "stepdisc"
                                   "surveyfreq" "surveylogistic" "surveymeans" "surveyphreg" "surveyreg" "surveyselect" "syslin"
                                   "tcalis" "timeid" "timeseries" "tphreg" "tpspline" "transreg" "tree" "ttest"
                                   "ucm"
                                   "varclus" "varcomp" "variogram" "varmax"
                                   "x11" "x12"
                                   ) 'words)) font-lock-constant-face)

                                        ;       (cons (concat
                                        ;             "\\<"
                                        ;             "do[ \t]*" (regexp-opt '("over" "until" "while") t) "?"
                                        ;             "\\>")
                                        ;            font-lock-keyword-face)
                                        ;
   ))

(defvar sasbis-mode-font-lock-basegraphstatements08
  (list
       ;; SAS base and SAS/Graph statements
       (cons (concat ;"\\<"
              (regexp-opt
               '(
                 "do" "to" "by" "goto" ; "go"
                 "abort" "and" "array" "assess" "attrib"
                 "baseline" "bayes" "between" "bivar" "block" "bubble" "bubble2"
                 "change" "choro" "class" "contains" "contrast"
                 "delete" "display" "dm" "donut" "drop"
                 "else" "error" "exchange" "exclude"
                 "fcs" "file" "filename" "format" "freq"
                 "footnote" "footnote1" "footnote2" "footnote3" "footnote4" "footnote5"
                 "footnote6" "footnote7" "footnote8" "footnote9" "footnote10"
                 "goptions" "grid" ; "ge" "gt"
                 "hazardratio" "hbar" "hbar3d"
                 "id" "if" "index" "infile" "informat" "input" ; "is" rarely used, but common false pos.
                 "keep"
                 "label" "length" "libname" "like" "link" "lsmeans" ; "le" "lt"
                 "manova" "means" "merge" "missing" "model" "modify"
                 "not" "null" ; "ne" "note"
                 "ods" "options" "output" "otherwise" ; "or"
                 "pageby" "parms" "pie" "pie3d" "plot" "plot2" "prism" "put"
                 "random" "rename" "repeated" "retain"
                 "same" "save" "scatter" "select" "set" "skip" "star" "strata" "sum" "sumby" "surface"
                 "table" "tables" "test" "then" "time"
                 "title" "title1" "title2" "title3" "title4" "title5"
                 "title6" "title7" "title8" "title9" "title10"
                 "univar" "update"
                 "value" "var" "vbar" "vbar3d"
                 "weight" "where" "window" "with"
                                        ; "x"
                 ) 'words)) ;"\\>")
             font-lock-keyword-face)

       ;; SAS/GRAPH statements not handled above
       (cons (concat "\\<"
                     (regexp-opt
                      '("axis" "legend" "pattern" "symbol")) "\\([1-9][0-9]?\\)?"
                      "\\>")
             font-lock-keyword-face)
   ))

(defvar sasbis-mode-font-lock-macrosfunctions09
  (list
       ;; SAS functions and SAS macro functions
       (cons "%[a-z_][a-z_0-9]*[(;]"                  font-lock-function-name-face)
                                        ;(cons "\\<call[ \t]+[a-z]+("                   font-lock-function-name-face)
   ))

(defvar sasbis-mode-font-lock-functions10
  (list
       (cons (concat ;"\\<"
              (regexp-opt
               '(
                 "abs" "arcos" "arsin" "atan"
                 "betainv" "byte"
                 "call execute" "call label" "call module" "call modulei"
                 "call poke" "call ranbin" "call rancau" "call ranexp"
                 "call rangam" "call rannor" "call ranpoi" "call rantbl"
                 "call rantri" "call ranuni" "call rxchange" "call rxfree"
                 "call rxsubstr" "call set" "call streaminit" "call symput" "call system"
                 "cdf" "ceil" "cinv" "collate" "compress" "convx" "convxp" "cos" "cosh" "css" "cv"
                 "daccdb" "daccdbsl" "daccsl" "daccsyd" "dacctab"
                 "depdb" "depdbsl" "depsl" "depsyd" "deptab"
                 "date" "datejul" "datepart" "datetime" "day" "dhms" "dif" "digamma" "dim"
                 "erf" "erfc" "exp"
                 "finv" "fipname" "fipnamel" "fipstate" "floor" "fuzz"
                 "gaminv" "gamma"
                 "hbound" "hms" "hour"
                 "in" "index" "indexc" "input" "int" "intck" "intnx" "intrr" "irr"
                 "juldate"
                 "kurtosis"
                 "lag" "lbound" "left" "length" "lgamma" "log" "log10" "log2"
                 "logcdf" "logpdf" "logsdf"
                 "max" "mdy" "mean" "min" "minute" "mod" "month" "mort"
                 "n" "netpv" "nmiss" "normal" "npv"
                 "ordinal"
                 "pdf"
                 "probbeta" "probbnml" "probchi" "probf" "probgam" "probhypr" "probit" "probnegb" "probnorm" "probt"
                 "poisson" "put"
                 "qtr" "quantile"
                 "rand" "range" "rank" "repeat" "reverse" "right" "round" "rxmatch" "rxparse"
                 "ranbin" "rancau" "ranexp" "rangam" "rannor" "ranpoi" "rantbl" "rantri" "ranuni"
                 "saving" "scan" "sdf" "second" "sign" "sin" "sinh" "sqrt" "squantile"
                 "std" "stderr" "stfips" "stname" "stnamel" "substr" "sum" "symget"
                 "tan" "tanh" "time" "timepart" "tinv" "today" "translate" "trigamma" "trim" "trunc"
                 "uniform" "until" "upcase" "uss"
                 "var" "verify"
                 "weekday" "when" "while"
                 "year" "yyq"
                 "zipfips" "zipname" "zipnamel" "zipstate"
;;;    ;; SAS/IML functions
                 "all" "allcomb" "allperm" "any" "apply" "armasim"
                 "bin" "blankstr" "block" "branks" "bspline" "btran" "byte"
                 "char" "choose" "col" "colvec" "concat" "contents" "convexit" "corr" "corr2cov"
                 "countmiss" "countn" "countunique" "cov" "cov2corr" "covlag" "cshape" "cusum"
                 "cuprod" "cv" "cvexhull"
                 "datasets" "design" "designf" "det" "diag" "dimension" "distance" "do" "duration"
                 "echelon" "eigval" "eigvec" "expmatrix" "expandgrid"
                 "fft" "fftc" "forward" "froot" "full"
                 "gasetup" "geomean" "ginv"
                 "hadamard" "half" "hankel" "harmean" "hdir" "hermite" "homogen"
                 "i" "ifft" "ifftc" "importtablefromr" "insert" "inv" "invupdt" "isempty" "isskipped"
                 "j" "jroot"
                 "kurtosis"
                 "lambertw" "listgetallnames" "listgetitem" "listgetname" "listgetsubitem" "listindex"
                 "listlen" "loc" "logabsdet"
                 "mad" "magic" "mahalanobis" "moduleic" "modulein"
                 "name" "ncol" "nrow" "ndx2sub" "nleng" "norm" "num"
                 "opscal" "orpol"
                 "parentname" "palette" "polyroot" "prod" "product" "pv"
                 "quartile"
                 "rancomb" "randdirichlet" "randfun" "randmultinomial" "randmvt" "randnormal" "randwishart"
                 "ranperk" "ranperm" "ranktie" "rates" "ratio" "remove" "repeat" "root" "row"
                 "rowcat" "rowcatc" "rowvec" "rsubstr"
                 "sample" "setdif" "shape" "shapecol" "skewness" "solve" "sparse" "splinev" "spot"
                 "sqrsym" "sqrvech" "ssq" "standard" "storage" "sub2ndx" "sweep" "symsqr"
                 "t" "tablecreate" "tablecreatefromdataset" "tablegetvardata" "tablegetvarformat"
                 "tablegetvarindex" "tablegetvarinformat" "tablegetvarlabel" "tablegetvarname"
                 "tablegetvartype" "tableisexistingvar" "tableisvarnumeric" "tfhilbert" "tfpwv"
                 "tfstft" "tfwindow" "toeplitz" "trace" "trisolv" "type"
                 "union" "unique" "uniqueby"
                 "value" "vecdiag" "vech"
                 "xmult" "xsect"
                 "yield"

;;;    ;; SAS functions introduced in Technical Report P-222
                 "airy"
                 "band" "blshift" "brshift" "bnot" "bor" "bxor"
                 "cnonct" "compbl"
                 "dairy" "dequote"
                 "fnonct"
                 "ibessel" "indexw" "inputc" "inputn"
                 "jbessel"
                 "lowcase"
                 "putc" "putn"
                 "quote"
                 "resolve"
                 "soundex" "sysprod"
                 "tnonct" "tranwrd" "trimn"

;;;    ;; SCL functions that are known to work with SAS macro function %sysfunc
                 "attrc" "attrn"
                 "cexist" "close"
                 "dclose" "dnum" "dopen" "dread"
                 "exist"
                 "fclose" "fetchobs" "fileexist" "finfo" "fopen" "fput" "fwrite"
                 "getoption" "getvarc" "getvarn"
                 "libname" "libref"
                 "open" "optgetn" "optsetn"
                 "pathname"
                 "sysmsg"
                 "varfmt" "varlabel" "varnum" "vartype"
                 ) 'words) ;"\\>"
              "("); "[ \t]*(")
             font-lock-function-name-face)
   ))

(defvar sasbis-mode-font-lock-defaults
  (append sasbis-mode-font-lock-comment01
sasbis-mode-font-lock-errors02
sasbis-mode-font-lock-warnings03
sasbis-mode-font-lock-override04
sasbis-mode-font-lock-execblocks05
sasbis-mode-font-lock-statements06
sasbis-mode-font-lock-procname07
sasbis-mode-font-lock-basegraphstatements08
sasbis-mode-font-lock-macrosfunctions09
sasbis-mode-font-lock-functions10))

(define-derived-mode sasbis-mode prog-mode "sas"
  "Major mode for editing SAS source. "
  :group 'sasbis-mode
  ;; (ess-setq-vars-local SAS-customize-alist)
  ;; (setq ess-local-customize-alist SAS-customize-alist)
  (setq-local sentence-end ";[\t\n */]*")
  (setq-local paragraph-start "^[ \t]*$")
  (setq-local paragraph-separate "^[ \t]*$")
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local adaptive-fill-mode nil)
  (setq-local indent-line-function #'sasbis-indent-line)
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/[*]")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[*]/")
  (setq-local comment-column 40)
  ;;  (setq-local ess-local-process-name nil)
  (setq-local tab-stop-list ess-sasbis-tab-stop-list)
  (setq font-lock-defaults
        ;; KEYWORDS KEYWORDS-ONLY CASE-FOLD .....
        '(sasbis-mode-font-lock-defaults nil t))
  (set-syntax-table sasbis-mode-syntax-table))

  ;; thing for either batch or interactive sessions
  ;; however, neither of these solutions are planned
  ;; therefore, no key definitions can be shared between
  ;; batch and interactive at this time, hence the lines that
  ;; are commented below:  uncomment at your own risk
  ;;  (define-key sas-mode-local-map "\C-c\C-p" 'ess-sas-file-path)
  ;;  (define-key sas-mode-local-map "\C-c\C-b" 'ess-sas-submit)
  ;;  (define-key sas-mode-local-map "\C-c\C-r" 'ess-sas-submit-region)
  ;;  (define-key sas-mode-local-map "\C-c\C-x" 'ess-sas-goto-log)
  ;;  (define-key sas-mode-local-map "\C-c\C-y" 'ess-sas-goto-lst)

(add-to-list 'auto-mode-alist '("\\.[Ss][Aa][Ss]\\'" . sasbis-mode))

(provide 'sasbis)
;;; sasbis.el ends here
