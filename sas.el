;;; tutu.el --- Description -*- lexical-binding: t; -*-
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

Optional fourth arg STARCOMMAND is string whose
contents are sent to the process as its initial input.
Optional fifth arg STDERR is a buffer for standard error.

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
Runs the given COMMAND with SWITCHES, initial input from STARTCOMMAND and standard error from STDERR.

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
  "Same function as `comint-exec-1' but with STDERR argument: a buffer to which will be used as standard error of process (see `make-process')"
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

(defun run-sas (&optional cmd dedicated show)
  "Run an inferior Sas process.

Argument CMD defaults to `sas-shell-calculate-command' return
value.  When called interactively with `prefix-arg', it allows
the user to edit such value and choose whether the interpreter
should be DEDICATED for the current buffer.  When numeric prefix
arg is other than 0 or 4 do not SHOW.

For a given buffer and same values of DEDICATED, if a process is
already running for it, it will do nothing.  This means that if
the current buffer is using a global process, the user is still
able to switch it to use a dedicated one.

Runs the hook `inferior-sas-mode-hook' after
`comint-mode-hook' is run.  (Type \\[describe-mode] in the
process buffer for a list of commands.)"
  (interactive
   (if current-prefix-arg
       (list
        (read-shell-command "Run Sas: " (sas-shell-calculate-command))
        (y-or-n-p "Make dedicated process? ")
        (= (prefix-numeric-value current-prefix-arg) 4))
     (list (sas-shell-calculate-command) nil t)))
  (let ((buffer
         (sas-shell-make-comint
          (or cmd (sas-shell-calculate-command))
          (sas-shell-get-process-name dedicated) show)))
    (pop-to-buffer buffer)
    (get-buffer-process buffer)))

(defun sas-shell-calculate-command ()
  "Calculate the string used to execute the inferior Sas process."
  (format "%s %s"
          ;; `sas-shell-make-comint' expects to be able to
          ;; `split-string-and-unquote' the result of this function.
          (combine-and-quote-strings (list sas-shell-interpreter))
          sas-shell-interpreter-args))
(defcustom sas-shell-interpreter "sas"
  "Default Sas interpreter for shell."
  :type 'string
  :group 'sas)
(defcustom sas-shell-interpreter-args '("-nodms" "-nonews" "-stdio"
                            "-nofullstimer" "-nodate" "-nocenter"
                            "-terminal" "-pagesize" "max"
                            "-nosyntaxcheck")
  "Default arguments for the Sas interpreter."
  :type 'string
  :group 'sas)

(defun sas-shell-make-comint (cmd proc-name &optional show internal)
  "Create a Sas shell comint buffer.
CMD is the Sas command to be executed and PROC-NAME is the
process name the comint buffer will get.  After the comint buffer
is created the `inferior-sas-mode' is activated.  When
optional argument SHOW is non-nil the buffer is shown.  When
optional argument INTERNAL is non-nil this process is run on a
buffer with a name that starts with a space, following the Emacs
convention for temporary/internal buffers, and also makes sure
the user is not queried for confirmation when the process is
killed."
  (save-excursion
    (sas-shell-with-environment
      (let* ((proc-buffer-name
              (format (if (not internal) "*%s*" " *%s*") proc-name)))
        (when (not (comint-check-proc proc-buffer-name))
          (let* ((cmdlist (split-string-and-unquote cmd))
                 (interpreter (car cmdlist))
                 (args (cdr cmdlist))
                 (buffer (apply #'make-comint-in-buffer-std proc-name proc-buffer-name
                                interpreter nil nil args))
                 (sas-shell--parent-buffer (current-buffer))
                 (process (get-buffer-process buffer))
                 ;; Users can override the interpreter and args
                 ;; interactively when calling `run-sas', let-binding
                 ;; these allows having the new right values in all
                 ;; setup code that is done in `inferior-sas-mode',
                 ;; which is important, especially for prompt detection.
                 (sas-shell--interpreter interpreter)
                 (sas-shell--interpreter-args
                  (mapconcat #'identity args " ")))
            (with-current-buffer buffer
              (inferior-sas-mode))
            (when show (display-buffer buffer))
            (and internal (set-process-query-on-exit-flag process nil))))
        proc-buffer-name))))

;; (defvar sas-cli-file-path "/usr/local/bin/sas_u8"
;;   "Path to the program used by `run-sas'")
;; (defvar sas-cli-arguments '("-nodms" "-nonews" "-stdio"
;;                             "-nofullstimer" "-nodate" "-nocenter"
;;                             "-terminal" "-pagesize" "max"
;;                             "-nosyntaxcheck")
;;   "Commandline arguments to pass to `sas-cli'.")
;; to print sas options list add "-oplist" to sas-cli-arguments
(defvar sas-prompt-regexp "^"
  "Prompt for `run-sas'.")
(defun sas--initialize ()
  "Helper function to initialize Sas"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode inferior-sas-mode comint-mode "Inferior sas"
  "Major mode for sas inferior process`run-sas'."
  nil "sas"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp sas-prompt-regexp))
;; this makes it read only; a contentious subject as some prefer the
;; buffer to be overwritable.
;; (setq comint-prompt-read-only t)
;; (setq comint-process-echoes t)
;; this makes it so commands like M-{ and M-} work.
;; (set (make-local-variable 'paragraph-separate) "\\'")
;; (set (make-local-variable 'font-lock-defaults) '(sas-font-lock-keywords t))
;; (set (make-local-variable 'paragraph-start) sas-prompt-regexp))

;; this has to be done in a hook. grumble grumble.
(add-hook 'inferior-sas-mode-hook 'sas--initialize)

(defvar sasbis-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r"   #'ess-eval-region)
;   (define-key map "\C-c\M-r"   #'ess-eval-region-and-go)
    (define-key map "\C-c\C-b"   #'ess-eval-buffer)
;   (define-key map "\C-c\M-b"   #'ess-eval-buffer-and-go)
    (define-key map (kbd "C-c C-<up>")   #'ess-eval-buffer-from-beg-to-here)
    (define-key map (kbd "C-c C-<down>") #'ess-eval-buffer-from-here-to-end)
    (define-key map "\C-c\C-p"   #'ess-eval-paragraph-and-step)
;   (define-key map "\C-c\M-p"   #'ess-eval-paragraph-and-go)
;   (define-key map "\C-\M-x"    #'ess-eval-region-or-function-or-paragraph)
    (define-key map "\C-c\C-n"   #'ess-eval-line-visibly-and-step)
    (define-key map "\C-c\C-j"   #'ess-eval-line)
    (define-key map [(control return)] #'ess-eval-region-or-line-visibly-and-step)
;   (define-key map "\C-c\M-j"   #'ess-eval-line-and-go)
    ;; FIXME: The next three can only work in S/R - mode
;   (define-key map "\C-c\C-l"   #'ess-load-file)
    ;;; Make an alias because C-c C-l is taken up by comint in inferiors
    (define-key map "\C-c\M-l"   #'ess-load-file)
    (define-key map "\C-c\C-v"   #'ess-display-help-on-object)
    (define-key map "\C-c\C-s"   #'ess-switch-process)
    (define-key map "\t"         #'ess-indent-or-complete)
    (define-key map "\C-c\C-q"   #'ess-quit)
   map)
  "Keymap for `sasbis-mode'.")

(defcustom ess-sas-tab-stop-list
  '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)
  "List of tab stop positions used by `tab-to-tab-stop' in sas-mode."
  :type '(repeat integer)
  :group 'sasbis-mode)

(define-derived-mode sasbis-mode fundamental-mode "sas"
  "Major mode for editing SAS source. "
  :group 'sasbis-mode
  ;; (ess-setq-vars-local SAS-customize-alist)
  ;; (setq ess-local-customize-alist SAS-customize-alist)
  (setq-local sentence-end ";[\t\n */]*")
  (setq-local paragraph-start "^[ \t]*$")
  (setq-local paragraph-separate "^[ \t]*$")
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local adaptive-fill-mode nil)
  (setq-local indent-line-function #'sas-indent-line)
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/[*]")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[*]/")
  (setq-local comment-column 40)
  ;;  (setq-local ess-local-process-name nil)
  (setq-local tab-stop-list ess-sas-tab-stop-list)
  (setq font-lock-defaults
        ;; KEYWORDS KEYWORDS-ONLY CASE-FOLD .....
        '(sasbis-mode-font-lock-defaults nil t)))

(defvar sasbis-mode-font-lock-defaults
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
       (cons "^1[ ]+The SAS System.*$"             font-lock-comment-face)
       (cons "^\014.*$"                            font-lock-comment-face)
       (cons "[*][*][*] ANNOTATE macros are now available [*][*][*]"
             font-lock-comment-face)
       (cons "For further information on ANNOTATE macros, enter,"
             font-lock-comment-face)
       ;; (cons "^SAS/STAT 9.3_M1, SAS/ETS 9.3_M1, SAS/OR 9.3_M1"
       ;;       font-lock-comment-face)
       (cons "\\(or \\)?%HELPANO.*$"
             font-lock-comment-face)
       (cons "^Local Variables:$"                  font-lock-comment-face)
       (cons "^End:$"                              font-lock-comment-face)
       (cons "^MPRINT([_A-Z0-9]+)"                 font-lock-comment-face)

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
       (cons "[ ][_]+$"                            font-lock-keyword-face)

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
             font-lock-comment-face)

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
             font-lock-constant-face)

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

;;;    ;; SAS procedure names
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

       ;; SAS functions and SAS macro functions
       (cons "%[a-z_][a-z_0-9]*[(;]"                  font-lock-function-name-face)
                                        ;(cons "\\<call[ \t]+[a-z]+("                   font-lock-function-name-face)

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
       )
     )
  ;; "Font Lock regexs for SAS."

(provide 'sas)
;;; sas.el ends here
