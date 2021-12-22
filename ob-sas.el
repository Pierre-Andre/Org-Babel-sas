 ;;; ob-sas.el --- org-babel functions for sas code evaluation

 ;; Copyright (C) 2019 P.A. Cornillon
 ;; Author: P.A. Cornillon
 ;;      G. Jay Kerns
 ;;      Eric Schulte
 ;;      Dan Davison


 ;; This file is not part of GNU Emacs.

 ;; This program is free software; you can redistribute it and/or modify
 ;; it under the terms of the GNU General Public License as published by
 ;; the Free Software Foundation; either version 3, or (at your option)
 ;; any later version.
 ;;
 ;; This program is distributed in the hope that it will be useful,
 ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
 ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ;; GNU General Public License for more details.
 ;;
 ;; You should have received a copy of the GNU General Public License
 ;; along with GNU Emacs; see the file COPYING.  If not, write to the
 ;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 ;; Boston, MA 02110-1301, USA.

 ;;; Commentary:

 ;; The file provides Org-Babel support for evaluating sas code.  It is
 ;; basically the adaptation of
 ;; ob-R.el by E. Schulte which results of find-and-replace "julia" by "sas" in
 ;; ob-julia.el by G. Jay Kerns.
 ;; see
 ;; https://github.com/Pierre-Andre/Org-Babel-sas
 ;;; Requirements:
 ;; Sas: http://sas.com
 ;; ESS: http://ess.r-project.org (only for session in unix/linux)

 ;;; Code:
 (require 'ob)
 (require 'cl-lib)

 (declare-function orgtbl-to-csv "org-table" (table params))
 (declare-function sasbis-shell-send-string "ext:sasbis"(string &optional process msg))
 (declare-function inferior-ess-send-string "ext:ess-inf" ())
 (declare-function ess-make-buffer-current "ext:ess-inf" ())
 (declare-function ess-eval-buffer "ext:ess-inf" (vis))
 (declare-function org-number-sequence "org-compat" (from &optional to inc))

 ;;;;;;;;;;;;;;;; could be useful to increase or decrease timeout ....
 (defcustom org-babel-sas-timeout 5
   "Timeout (in sec) used when waiting output from a submitted src block (to sas) with argument :session."
   :group 'org-babel
   :type 'integer)
 ;;;;;;;;;;;;;;;; could be useful to tweak printing page size
 (defcustom org-babel-sas-print-options "options formdlim='' pagesize=max nocenter nodate;\n"
   "general options used to have the maximum page size"
   :group 'org-babel
   :type 'string)

 ;;;;;;;;;;;;;;;; where is SAS (for :session "none" or :session
 (defcustom org-babel-sas-command "sas"
 ;  inferior-SAS-program-name
   "Command name to use for executing sas code."
   :group 'org-babel
   :type 'string)
(defcustom org-babel-sas-session-interpreter-args "-nodms -nonews -stdio -nofullstimer -nodate -nocenter -terminal -pagesize max -nosyntaxcheck"
  "Default arguments for the Sas interpreter."
  :type 'string
  :group 'org-babel)
 ;;;;;;;;;;;;;;; command line option to be used with SAS (for :session "none")
 (defcustom org-babel-sas-command-options
   "-formdlim='' -pagesize=max -nonumber -nodate -nocenter -nonews -nodms"
   "Options for sas batch"
   :group 'org-babel
   :type 'string)

 ;;;;;;;;;;;;;;; windows SAS or not
 (defcustom org-babel-sas-windows
   nil
   "SAS on windows (non nil)  or not (nil)"
   :group 'org-babel
   :type 'boolean)
 ;;;;;;;;;;;;;;; real session or not (user library)
 (defcustom org-babel-sas-realsession
   nil
   "is the :session will use ESS to make a real session (non nil, unix only)
    or use a user library (nil)"
   :group 'org-babel
   :type 'boolean)
 ;;;;;;;;;;;;;;; custom log file name (for :session "none")
 (defcustom org-babel-sas-logfile-name
   nil
   "Optionnal log-file name for :session none"
   :group 'org-babel
   :type 'string)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; global alist to take care of previous commands done in session
 (defvar org-babel-sas-lepoint (list (cons "sessionSAS" 1)))
 ;; small sas program sent to sas after the actual sas chunk
 ;; this define the end signal
 (defvar org-babel-sas-eoe-indicator "\ndata eoe_org_data;\n nbabelvareoe=1;\nrun;\nOPTIONS NODATE NONUMBER;\nTITLE1;\nTITLE2;\nproc print data=eoe_org_data;\nrun;")
 ;; output of the corresponding small program
 ;; when seen it means that all the chunk is done
 (defvar org-babel-sas-boe-output "Obs.    nbabelvareoe[ \n]+1[ ]+1")
 ;; output cursor to be trimmed
 ;; (defvar org-babel-sas-boe-output "$ tty\n/dev/pts/[0-9]+\n\\$")
 ;; cursor to be trimmed
 ;; log cursor to be trimmed
 (defvar org-babel-sas-boe-log "[0-9]+[ ]+data eoe_org_data;")
(defvar org-babel-sas-eoe-log "[0-9]+[ ]+proc print data=eoe_org_data;\n[0-9]+[ ]+run;")
  ;; log of eoe to be trimmed
;; (defvar org-babel-sas-eoe-log
;;   "[0-9]+[ ]+nbabelvareoe=1;\n[0-9]+[ ]+run")
;; (defvar org-babel-sas-eoe-log
;;   "[0-9]+[ ]+proc print data=eoe_org_data;\n[0-9]+[ ]+run;")
 ;; header stuff;\nNOTE.*seconds
 (defconst org-babel-header-args:sas
   '((hsize		 . :any)
     (vsize		 . :any)
     (xpixels		 . :any)
     (ypixels		 . :any)
     (border		 . :any)
     (width		 . :any)
     (height		 . :any)
     (sastab		 . :any)
     (results             . ((file list vector table scalar verbatim)
			     (raw org html latex code pp wrap)
			     (replace silent append prepend)
			     (output value graphics odsgraphics log))))
   "sas-specific header arguments.")

 (add-to-list 'org-babel-tangle-lang-exts '("sas" . "sas"))

 ;; session using ESS is the way to go, so make that the default
 (defvar org-babel-default-header-args:sas '((:results . "output") (:session . nil)))

 ;; from unix to windows path (ie change / to \)
 (defun org-babel-sas-path-windows (s)
   "replace / by \\"
    (replace-regexp-in-string "/" "\\\\" s))
 ;; trim white space and garbage
 (defun org-babel-sas-trim-white (s)
   "replace S by empty string if S is whitespace/tab/CR only"
   (if (string-match "\\`[ \t\n\r]+\\'" s)
       (replace-match "" t t s)
     s))
 (defun org-babel-sas-trim-doubleline (s)
   "replace elo eol by eol"
   (if (string-match "\n\n" s)
       (replace-match "\n" t t s)
     s))
 (defun org-babel-sas-trim-end (s)
   "replace eol + white by eol"
   (if (string-match "\n[ ]+\\'" s)
       (replace-match "\n" t t s)
     s))
 (defun org-babel-sas-trim-begin (s)
   "replace eol by empty string"
   (if (string-match "\\`\n" s)
       (replace-match "" t t s)
     s))

 (defun org-babel-execute:sas (body params)
   "Execute a block of sas code.
 This function is called by `org-babel-execute-src-block'."
   (save-excursion
     (let* ((result-params (cdr (assq :result-params params)))
	    (result-type (cdr (assq :result-type params)))
	    (session (org-babel-sas-initiate-session
		      (cdr (assq :session params)) params))
	   (graphics-file (org-babel-sas-graphical-output-file params))
	   (graphics-type (or (member "odsgraphics" (cdr (assq :result-params params))) (member "graphics" (cdr (assq :result-params params)))))
	   (sastab-value (if (string-equal result-type "value")
			     (cdr (assq :sastab params))
			   nil))
	   (sastab-tmp-file (if (string-equal result-type "value")
			     (org-babel-temp-file "SASexport-")
			     nil))
	   (full-body (org-babel-expand-body:sas body params graphics-file graphics-type sastab-value sastab-tmp-file))
	   (result
	    (org-babel-sas-evaluate
	     session full-body result-type result-params sastab-tmp-file)))
       (if graphics-file nil result))))

(defvar org-babel-sas-buffers '((:default . "*Sas*")))

(defun org-babel-sas-session-buffer (session)
  "Return the buffer associated with SESSION."
  (cdr (assoc session org-babel-sas-buffers)))

(defun org-babel-sas-with-earmuffs (session)
  (let ((name (if (stringp session) session (format "%s" session))))
    (if (and (string= "*" (substring name 0 1))
	     (string= "*" (substring name (- (length name) 1))))
	name
      (format "*%s*" name))))

(defun org-babel-sas-without-earmuffs (session)
  (let ((name (if (stringp session) session (format "%s" session))))
    (if (and (string= "*" (substring name 0 1))
	     (string= "*" (substring name (- (length name) 1))))
	(substring name 1 (- (length name) 1))
      name)))

 (defun org-babel-prep-session:sas (session params)
  "Prepare SESSION according to the header arguments in PARAMS.
VARS contains resolved variable references."
  (let* ((session (org-babel-sas-initiate-session session))
	 (var-lines
	  (org-babel-variable-assignments:sas params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input)
              (org-babel-comint-wait-for-output session))
	    var-lines))
    session))

(defun org-babel-load-session:sas (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:sas session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))
 (defun org-babel-sas-initiate-session (session params)
  "If there is not a current sas process then create one
  (if realsession) or give as a string the library directory
  (if not realsession)"
  (if (string= session "none") "none"
    (if (null org-babel-sas-realsession)
        (if (stringp session) session
          org-babel-temporary-directory)
      (org-babel-sas-initiate-realsession session params))))


(defun org-babel-sas-initiate-realsession (&optional session _params)
  "Create a session named SESSION according to PARAMS."
  (unless (string= session "none")
    (org-babel-sas-session-buffer
     (org-babel-sas-initiate-session-by-key session))))

(defun sasbis-shell-calculate-session-command ()
"Calculate the string used to execute the inferior Sas process."
  (format "%s %s"
          ;; `sasbis-shell-make-comint' expects to be able to
          ;; `split-string-and-unquote' the result of this function.
          (combine-and-quote-strings (list org-babel-sas-command))
          org-babel-sas-session-interpreter-args))

(defun org-babel-sas-initiate-session-by-key (&optional session)
  "Initiate a sas session.
If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (save-window-excursion
    (let* ((session (if session (intern session) :default))
           (sas-buffer (org-babel-sas-session-buffer session))
	   (cmd (if (member system-type '(cygwin windows-nt ms-dos))
		    (concat org-babel-sas-command "")
		  org-babel-sas-command)))
	(unless sas-buffer
	  (setq sas-buffer (org-babel-sas-with-earmuffs session)))
	(let ((sas-shell-buffer-name
	       (org-babel-sas-without-earmuffs sas-buffer)))
	  (run-sasbis (sasbis-shell-calculate-session-command))
	  (sleep-for 0 10))
      (setq org-babel-sas-buffers
	    (cons (cons session sas-buffer)
		  (assq-delete-all session org-babel-sas-buffers)))
      session)))

 (defun org-babel-sas-graphical-output-file (params)
   "Name of file to which sas should send graphical output."
   (and (or (member "graphics" (cdr (assq :result-params params)))
	    (member "odsgraphics" (cdr (assq :result-params params))))
	(cdr (assq :file params))))

 (defvar org-babel-sas-graphics-devices
   '((:bmp "bmp")
     (:emf "emf")
     (:tiff "tiff")
     (:png "png")
     (:png300 "png300")
     (:svg "svg")
     (:pdf "pdf")
     (:ps "pscolor")
     (:postscript "pscolor"))
   "An alist mapping graphics file types to SAS devices.

 Each member of this list is a list with three members:
 1. the file extension of the graphics file, as an elisp :keyword
 2. the SAS device function to call to generate such a file")

 ;; we need the following twolines with sas/graph :graphics
 ;; example of svg device
 ;; filename sortie "toto.svg";
 ;; goptions  device=svg gsfname=sortie
 ;; or this line with ODS graphics :odsgraphics
 ;; ods graphics on /  imagefmt=png imagename="barplot" border=off width=10cm;

 (defun org-babel-sas-construct-graphics-device-call (out-file graphics-type params)
   "Construct the string for choosing device and saving graphic file"
   (let* ((allowed-args '(:hsize :vsize :xpixels :ypixels :border :width :height))
	  (device (file-name-extension out-file))
	  (device-info (or (assq (intern (concat ":" device))
				 org-babel-sas-graphics-devices)
                           (assq :png org-babel-sas-graphics-devices)))
	  (extra-args (cdr (assq :SAS-dev-args params))) filearg args)
     (setq device (nth 1 device-info))
     (setq args (mapconcat
		 (lambda (pair)
		   (if (member (car pair) allowed-args)
		       (format " %s=%S"
			       (substring (symbol-name (car pair)) 1)
			       (cdr pair)) ""))
		 params ""))
     (if (string-equal (car graphics-type) "odsgraphics")
	 (format "ods graphics on / imagename=\"%s\" imagefmt=%s %s;\n"
		 (file-name-sans-extension out-file) device args
		 (if extra-args " " "") (or extra-args ""))
       (format "filename outfob \"%s\";\ngoptions  device=%s gsfname= outfob %s;\n"
	     out-file device args
	     (if extra-args " " "") (or extra-args "")))))

 (defun org-babel-expand-body:sas (body params &optional graphics-file graphics-type sastab-value sastab-tmp-file)
   "Expand BODY according to PARAMS, return the expanded body."
   (let ((graphics-file
	  (or graphics-file
	      (org-babel-sas-graphical-output-file params)))
	 (graphics-type
	  (or graphics-type
	      (or (member "odsgraphics" (cdr (assq :result-params params)))
		  (member "graphics" (cdr (assq :result-params params)))))))
     (concat org-babel-sas-print-options
      (if graphics-file
	    (org-babel-sas-construct-graphics-device-call
	     graphics-file graphics-type params)
	"")
      body
      (if graphics-file
		     (if (string-equal (car graphics-type) "odsgraphics")
			 "quit;\nods graphics off;\n"
		       "quit;\n"))
      (if sastab-value
	  (org-babel-sas-construct-export-call sastab-value
					       (if org-babel-sas-windows (org-babel-sas-path-windows sastab-tmp-file) sastab-tmp-file))
	""))))

 (defun org-babel-sas-construct-export-call (sastab-value sastab-tmp-file)
   (let ((tmp-file (org-babel-temp-file "SAS-")))
     (concat "proc export data=" sastab-value "\n outfile='" sastab-tmp-file
      "'\n dbms=tab replace;\nrun;")))

 (defun org-babel-sas-evaluate
   (session body result-type result-params sastab-tmp-file)
   "Evaluate sas code in BODY."
   (if (and (string-or-null-p session) (not (string= session "*Sas*")))
       (org-babel-sas-evaluate-external-process
	body result-type result-params sastab-tmp-file session)
     (org-babel-sas-evaluate-session
      session body result-type result-params sastab-tmp-file)))

	  (defun org-babel-sas-evaluate-external-process
	    (body result-type result-params sastab-tmp-file session)
	    "Evaluate BODY in external sas process.
	  If RESULT-TYPE equals 'output then return standard output as a
	  string.  If RESULT-TYPE equals 'value then return the value of the
	  :sastab SAS table, as elisp."
	    (if (car (member "log" result-params))
	       ;; log
	       (let ((tmp-file (org-babel-temp-file "SAS-")))
		     ;;((tmp-file "sas-file4677846547.sas")
		 ;;(directory-sas ""))
		 (with-current-buffer
		     (switch-to-buffer (get-buffer-create (concat tmp-file ".sas")))
		   (set-visited-file-name (concat tmp-file ".sas"))
		   (insert body)
		   (save-buffer 0))
		 (shell-command (if org-babel-sas-windows
				    (if (string= session "none")
					(format "%s -SYSIN %s -NOTERMINAL NOSPLASH -NOSTATUSWIN -NOICON -PRINT %s -LOG %s"
					org-babel-sas-command
					(concat tmp-file ".sas")
					(concat tmp-file ".lst")
					(if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					  (concat tmp-file ".log")))
					(format "%s -USER %s -SYSIN %s -NOTERMINAL NOSPLASH -NOSTATUSWIN -NOICON -PRINT %s -LOG %s"
					org-babel-sas-command session
					(concat tmp-file ".sas")
					(concat tmp-file ".lst")
					(if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					  (concat tmp-file ".log"))))
				  (if (string= session "none")
				    (format "%s %s -log %s -print %s %s"
					org-babel-sas-command org-babel-sas-command-options
					(if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					  (concat tmp-file ".log"))
					(concat tmp-file ".lst")
					(concat tmp-file ".sas"))
				    (format "%s -user %s %s -log %s -print %s %s"
					org-babel-sas-command session org-babel-sas-command-options
					(if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					  (concat tmp-file ".log"))
					(concat tmp-file ".lst")
					(concat tmp-file ".sas")))) nil nil)
		 (kill-buffer (file-name-nondirectory (concat tmp-file ".sas")))
		 (delete-file (concat tmp-file ".sas"))
		 (if (file-readable-p (if org-babel-sas-logfile-name
					  org-babel-sas-logfile-name
					  (concat tmp-file ".log")))
		     (progn
		       (with-current-buffer
			   (switch-to-buffer (find-file-noselect (if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					    (concat tmp-file ".log"))))
			 (beginning-of-buffer)
			 (setq body (buffer-string)))
		       (kill-buffer (file-name-nondirectory (if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					    (concat tmp-file ".log"))))
		       (delete-file  (if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					    (concat tmp-file ".log")))
		       body)
	 "no log file ??"))
	    (cl-case result-type
	      (value
	       ;; org-babel-eval does pass external argument...
	       (let ((tmp-file (org-babel-temp-file "SAS-")))
		     ;;((tmp-file "sas-file4677846547.sas")
		 ;;(directory-sas ""))
		 (with-current-buffer
		     (switch-to-buffer (get-buffer-create (concat tmp-file ".sas")))
		   (set-visited-file-name (concat tmp-file ".sas"))
		   (insert body)
		   (save-buffer 0))
		 (shell-command (if org-babel-sas-windows
				    (if (string= session "none")
					(format "%s -SYSIN %s -NOTERMINAL NOSPLASH -NOSTATUSWIN -NOICON -PRINT %s -LOG %s"
					org-babel-sas-command
					(concat tmp-file ".sas")
					(concat tmp-file ".lst")
					(if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					  (concat tmp-file ".log")))
				      (format "%s -USER %s -SYSIN %s -NOTERMINAL NOSPLASH -NOSTATUSWIN -NOICON -PRINT %s -LOG %s"
					org-babel-sas-command session
					(concat tmp-file ".sas")
					(concat tmp-file ".lst")
					(if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					  (concat tmp-file ".log"))))
				  (if (string= session "none")
				      (format "%s %s -log %s -print %s %s"
					org-babel-sas-command org-babel-sas-command-options
					(if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					  (concat tmp-file ".log"))
					(concat tmp-file ".lst")
					(concat tmp-file ".sas"))
				    (format "%s -user %s %s -log %s -print %s %s"
					org-babel-sas-command session org-babel-sas-command-options
					(if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					  (concat tmp-file ".log"))
					(concat tmp-file ".lst")
					(concat tmp-file ".sas")))) nil nil)
		 (kill-buffer (file-name-nondirectory (concat tmp-file ".sas")))
		 (delete-file (concat tmp-file ".sas"))
		 (if (file-readable-p sastab-tmp-file)
		     (org-babel-result-cond result-params
		       (org-babel-chomp
			(with-current-buffer (find-file-noselect sastab-tmp-file)
			  (buffer-string))
			"\n")
		       (org-babel-import-elisp-from-file sastab-tmp-file '(16)))
		   (progn
		     (if (get-buffer (if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					  (concat tmp-file ".log")))
			 (with-current-buffer (get-buffer  (if org-babel-sas-logfile-name
							       org-babel-sas-logfile-name
							     (concat tmp-file ".log")))
			   (revert-buffer :ignore-auto :noconfirm :preserve-modes))
		       (save-window-excursion (pop-to-buffer-same-window (find-file-noselect (if org-babel-sas-logfile-name
							       org-babel-sas-logfile-name
							     (concat tmp-file ".log"))))))
		     (format "Errors, please see [[file://%s][log file]] (in Buffer list)" (if org-babel-sas-logfile-name
							       org-babel-sas-logfile-name
							     (concat tmp-file ".log")))))))
	      (output
	       ;; org-babel-eval does pass external argument...
	       (let ((tmp-file (org-babel-temp-file "SAS-")))
		     ;;((tmp-file "sas-file4677846547.sas")
		 ;;(directory-sas ""))
		 (with-current-buffer
		     (switch-to-buffer (get-buffer-create (concat tmp-file ".sas")))
		   (set-visited-file-name (concat tmp-file ".sas"))
		   (insert body)
		   (save-buffer 0))
		 (shell-command (if org-babel-sas-windows
				    (if (string= session "none")
					(format "%s -SYSIN %s -NOTERMINAL NOSPLASH -NOSTATUSWIN -NOICON -PRINT %s -LOG %s"
					org-babel-sas-command
					(concat tmp-file ".sas")
					(concat tmp-file ".lst")
					(if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					  (concat tmp-file ".log")))
					(format "%s -USER %s -SYSIN %s -NOTERMINAL NOSPLASH -NOSTATUSWIN -NOICON -PRINT %s -LOG %s"
					org-babel-sas-command session
					(concat tmp-file ".sas")
					(concat tmp-file ".lst")
					(if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					  (concat tmp-file ".log"))))
				  (if (string= session "none")
				    (format "%s %s -log %s -print %s %s"
					org-babel-sas-command org-babel-sas-command-options
					(if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					  (concat tmp-file ".log"))
					(concat tmp-file ".lst")
					(concat tmp-file ".sas"))
				    (format "%s -user %s %s -log %s -print %s %s"
					org-babel-sas-command session org-babel-sas-command-options
					(if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					  (concat tmp-file ".log"))
					(concat tmp-file ".lst")
					(concat tmp-file ".sas")))) nil nil)
		  (message "SAS log file is: %s" (if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					  (concat tmp-file ".log")))
		 (kill-buffer (file-name-nondirectory (concat tmp-file ".sas")))
		 (delete-file (concat tmp-file ".sas"))
		   (if (file-readable-p (concat tmp-file ".lst"))
		     (progn
		       (with-current-buffer
			   (switch-to-buffer (find-file-noselect (concat tmp-file ".lst")))
			 (beginning-of-buffer)
			 (setq body (buffer-string)))
			(kill-buffer (file-name-nondirectory (concat tmp-file ".lst")))
		       (delete-file  (concat tmp-file ".lst"))
		       body)
		   (progn
		     (if (get-buffer (if org-babel-sas-logfile-name
					    org-babel-sas-logfile-name
					  (concat tmp-file ".log")))
			 (with-current-buffer (get-buffer  (if org-babel-sas-logfile-name
							       org-babel-sas-logfile-name
							     (concat tmp-file ".log")))
			   (revert-buffer :ignore-auto :noconfirm :preserve-modes))
		       (save-window-excursion (pop-to-buffer-same-window (find-file-noselect (if org-babel-sas-logfile-name
							       org-babel-sas-logfile-name
							     (concat tmp-file ".log"))))))
		     (format "Errors, please see [[file://%s][log file]] (in Buffer list)" (if org-babel-sas-logfile-name
							       org-babel-sas-logfile-name
							     (concat tmp-file ".log"))))))))))

(defun org-babel-sas-evaluate-session
    (session body result-type result-params sastab-tmp-file)
  "Evaluate BODY in SESSION.
 If RESULT-TYPE equals 'output then return standard output as a
 string.  If RESULT-TYPE equals 'value then return the value of the
 last statement in BODY, as elisp."
     (message "output: %s \n log: %s" result-type (if (member "log" result-params)
                   (car (member "log" result-params))))
 (let* ((tmp-file (org-babel-temp-file "SAS-"))
         (log  (if (member "log" result-params)
                   (car (member "log" result-params))))
         (output (eql result-type 'output))
         (output-string (org-babel-sas--send-string session body log output)))
   (message "output:%s" output-string)
    (cl-case result-type
      (value
       (if log
           (org-babel-chomp output-string)
         (org-babel-result-cond result-params
           (org-babel-chomp
            (with-current-buffer (find-file-noselect sastab-tmp-file)
              (buffer-string)
              )
            "\n")
                  (org-babel-import-elisp-from-file sastab-tmp-file '(16)))))
      (output
       ;; submit body through a temp buffer (in order to not go
       ;; beyond the limit of 500 bytes)
       ;; see
       ;; https://stat.ethz.ch/pipermail/ess-help/2015-April/010518.html
       output-string))))

(defun org-babel-sas--send-string (session body log output)
  "Pass BODY to the sas process in SESSION.
Return Sas output/results if OUTPUT is non nil else return Sas log if LOG is non nil."
  (let ((output-string ""))
    (with-current-buffer session
      (let  ((org-babel-errorbuffer-name (format "Log-%s"(org-babel-sas-without-earmuffs session)))
             (body (concat body  org-babel-sas-eoe-indicator "\n")))
        (sasbis-shell-send-string body)
        (let ((time (current-time))
              (elapsed-time 0))
          (with-current-buffer org-babel-errorbuffer-name
            (while (and (not (re-search-forward org-babel-sas-eoe-log nil t))
                        (< elapsed-time org-babel-sas-timeout))
              (setq elapsed-time (float-time (time-since time)))
              (sit-for 0.01)
              (goto-char (point-min)))
            (if log
                (setq output-string
                      (org-babel-remove-eoe log)))
           (comint-clear-buffer))))
      (if output
          (setq output-string
                (org-babel-remove-eoe log)))
      (comint-clear-buffer)))
  output-string)
    ;;(erase-buffer)
    ;;(save-buffer 0)))
    ;; (if (or log output)
    ;;     (org-babel-chomp
    ;;      (org-babel-sas-trim-end
    ;;       (org-babel-sas-trim-begin
    ;;        (org-babel-sas-trim-doubleline
    ;;         (org-babel-sas-trim-white
    ;;          output-string))))))))

(defun org-babel-remove-eoe (log)
  "Copy comint buffer (log or output) from the beginning until the indicator of end of execution.
These trace are different if comint buffer is Sas Log output
(ie LOG non nil) or Sas output/results (ie LOG is nil)"
      (goto-char (point-min))
      (let (pos1)
         (if (re-search-forward
                  (if log org-babel-sas-boe-log org-babel-sas-boe-output) nil t)
             (progn (setq pos1 (point))
                    (buffer-substring-no-properties (point-min) pos1)))))

 (provide 'ob-sas)

 ;;; ob-sas.el ends here
