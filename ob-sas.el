;;; ob-sas.el --- org-babel functions for sas code evaluation

;; Copyright (C) 2017 P.A. Cornillon
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
;; basically result of find-and-replace "julia" by "sas" in
;; ob-julia.el by G. Jay Kerns.
;; see 
;; https://github.com/Pierre-Andre/Org-Babel-sas
;;; Requirements:
;; Sas: http://sas.com
;; ESS: http://ess.r-project.org (for session in unix/linux)

;;; Code:
(require 'ob)
(require 'cl-lib)

(declare-function orgtbl-to-csv "org-table" (table params))
(declare-function sas "ext:ess-sas" (&optional start-args))
(declare-function inferior-ess-send-string "ext:ess-inf" ())
(declare-function ess-make-buffer-current "ext:ess-inf" ())
(declare-function ess-eval-buffer "ext:ess-inf" (vis))
(declare-function org-number-sequence "org-compat" (from &optional to inc))
;;;;;;;;;;;;;;;; could be useful to increase or decrease timeout ....
(defcustom org-babel-sas-timeout 1000
  "Timeout (in sec) used when waiting output from a submitted src block (to sas) with argument :session."
  :group 'org-babel
  :type 'integer)
;;;;;;;;;;;;;;;; could be useful to tweak printing page size
(defcustom org-babel-sas-print-options "options formdlim='' pagesize=max nocenter nodate;\n"
  "general options used to have the maximum page size"
  :group 'org-babel
  :type 'string)

;;;;;;;;;;;;;;;; where is SAS (for :session "none")
(defcustom org-babel-sas-command "/usr/local/bin/sas_u8"
;  inferior-SAS-program-name
  "Command name to use for executing sas code."
  :group 'org-babel
  :type 'string)
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
(defvar org-babel-sas-eoe-indicator "data eoe_org_data;\n nbabelvareoe=1;\nrun;\nOPTIONS NODATE NONUMBER;\nTITLE1;\nTITLE2;\nproc print data=eoe_org_data;\nrun;")
;; output of the corresponding small program
;; when seen it means that all the chunk is done
(defvar org-babel-sas-eoe-output "Obs.    nbabelvareoe[ \n]+1[ ]+1")
;; cursor to be trimmed
(defvar org-babel-sas-boe-output "$ tty\n/dev/pts/[0-9]+\n\\$")
;; header stuff
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
			    (output value graphics odsgraphics))))
  "sas-specific header arguments.")

(add-to-list 'org-babel-tangle-lang-exts '("sas" . "sas"))

;; session using ESS is the way to go, so make that the default
(defvar org-babel-default-header-args:sas '((:results . "output") (:session . nil)))

;; trim white space and garbage
(defun org-babel-sas-trim-white (s)
  "replace S by empty string if S is whitespace only"
  (if (string-match "\\`[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))
(defun org-babel-sas-trim-doubleline (s)
  "replace S by empty string if S is whitespace only"
  (if (string-match "\n\n" s)
      (replace-match "\n" t t s)
    s))
(defun org-babel-sas-trim-end (s)
  "replace S by empty string if S is whitespace only"
  (if (string-match "\n[ ]+\\'" s)
      (replace-match "\n" t t s)
    s))
(defun org-babel-sas-trim-begin (s)
  "replace S by empty string if S is whitespace only"
  (if (string-match "\\`\n" s)
      (replace-match "" t t s)
    s))
;; let's go: main function
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
 ;    	   (message ": %s" full-body)
      (if graphics-file nil result))))

(defvar ess-ask-for-ess-directory) ; dynamically scoped

(defun org-babel-sas-initiate-session (session params)
  "If there is not a current sas process then create one."
  (unless (string= session "none")
    (let ((session (or session "*SAS*"))
	  (ess-ask-for-ess-directory
	   (and (and (boundp 'ess-ask-for-ess-directory) ess-ask-for-ess-directory)
		(not (cdr (assq :dir params))))))
      (if (org-babel-comint-buffer-livep session)
	  session
	(save-window-excursion
	  (require 'ess) (SAS)
	  (rename-buffer
	   (if (bufferp session)
	       (buffer-name session)
	     (if (stringp session)
		 session
	       (buffer-name))))
	  (add-to-list 'org-babel-sas-lepoint (cons (concat "session-" (if (bufferp session)
	       (buffer-name session)
	     (if (stringp session)
		 session
	       (buffer-name)))) 1))
	  (current-buffer))))))
;;;;;;;;;;;;;;;;;;; two functions not used (at the moment ?)
(defun org-babel-sas-associate-session (session)
  "Associate sas code buffer with a sas session.
Make SESSION be the inferior ESS process associated with the
current code buffer."
  (setq ess-local-process-name
	(process-name (get-buffer-process session)))
  (ess-make-buffer-current))

(defun org-babel-load-session:sas (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:sas session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))
;;;;;;;;;;;;;;;;;;; end of not used 

(defun org-babel-sas-graphical-output-file (params)
  "Name of file to which sas should send graphical output."
  (and (or (member "graphics" (cdr (assq :result-params params)))
	   (member "odsgraphics" (cdr (assq :result-params params))))
       (cdr (assq :file params))))

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
	   (org-babel-sas-construct-export-call sastab-value sastab-tmp-file)
       ""))))

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

(defun org-babel-sas-construct-export-call (sastab-value sastab-tmp-file)
  (let ((tmp-file (org-babel-temp-file "SAS-")))
    (concat "proc export data=" sastab-value "\n outfile='" sastab-tmp-file 
     "'\n dbms=tab replace;\nrun;")))
(defun org-babel-sas-evaluate
  (session body result-type result-params sastab-tmp-file)
  "Evaluate sas code in BODY."
  (if session
      (org-babel-sas-evaluate-session
       session body result-type result-params sastab-tmp-file)
    (org-babel-sas-evaluate-external-process
     body result-type result-params sastab-tmp-file)))

(defun org-babel-sas-evaluate-external-process
  (body result-type result-params sastab-tmp-file)
  "Evaluate BODY in external sas process.
If RESULT-TYPE equals 'output then return standard output as a
string.  If RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
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
			  (format "%s -SYSIN %s -NOSPLASH -NOICON -PRINT %s -LOG %s"
			      org-babel-sas-command 
			      (concat tmp-file ".sas")
			      (concat tmp-file ".lst")
			      (if org-babel-sas-logfile-name
				  org-babel-sas-logfile-name
				(concat tmp-file ".log")))
			  (format "%s %s -log %s -print %s %s"
			      org-babel-sas-command org-babel-sas-command-options
			      (if org-babel-sas-logfile-name
				  org-babel-sas-logfile-name
				(concat tmp-file ".log"))
			      (concat tmp-file ".lst")
			      (concat tmp-file ".sas"))) nil nil)
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
			(format "%s -SYSIN %s -NOSPLASH -NOICON -PRINT %s -LOG %s"
			      org-babel-sas-command 
			      (concat tmp-file ".sas")
			      (concat tmp-file ".lst")
			      (if org-babel-sas-logfile-name
				  org-babel-sas-logfile-name
				(concat tmp-file ".log")))
			  (format "%s %s -log %s -print %s %s"
			      org-babel-sas-command org-babel-sas-command-options
			      (if org-babel-sas-logfile-name
				  org-babel-sas-logfile-name
				(concat tmp-file ".log"))
			      (concat tmp-file ".lst")
			      (concat tmp-file ".sas"))) nil nil)
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
						   (concat tmp-file ".log")))))))))

(defun org-babel-sas-evaluate-session
    (session body result-type result-params sastab-tmp-file)
  "Evaluate BODY in SESSION.
If RESULT-TYPE equals 'output then return standard output as a
string.  If RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (cl-case result-type
    (value
     ;;     (let* ((allowed-args '(:sastab))
     (let ((org-babel-sas-ess-process-name  (process-name (get-buffer-process session))))
      (with-temp-buffer
	(insert body)
	(let ((ess-local-process-name
	       (process-name (get-buffer-process session)))
	      (ess-eval-visibly-p nil))
	  (ess-eval-buffer nil)))
      (ess-send-string (get-process org-babel-sas-ess-process-name) org-babel-sas-eoe-indicator)
      ;;    excursion for cut/paste results from output buffer
      ;;   as output buffer is not the same as session buffer
      ;; org-babel-comint-with-output cannot be used 
      (save-excursion
      	(set-buffer (format "*%s.lst*" org-babel-sas-ess-process-name))
      	(let* ((a 0) (b 0) (ancienpoint (cdr (assoc (concat "session-" (if (stringp session) session (buffer-name session))) org-babel-sas-lepoint))))
      	 (while (< a org-babel-sas-timeout)
      	   (setq b a)
      	   (goto-char (cdr (assoc (concat "session-" (if (stringp session) session (buffer-name session))) org-babel-sas-lepoint)))
      	   (setq a (re-search-forward org-babel-sas-eoe-output nil t))
      	   (if a
      	       (progn (setq a org-babel-sas-timeout)
      	 	      (goto-char (cdr (assoc (concat "session-" (if (stringp session) session (buffer-name session))) org-babel-sas-lepoint)))
      	 	      (setq ancienpoint (cdr (assoc (concat "session-" (if (stringp session) session (buffer-name session))) org-babel-sas-lepoint)))
		      ;; well well, this is embarassing but
		      ;; as there's not history like in comint
		      ;; the last point is saved in this global
		      ;; alist variable (that will be used the
		      ;; next time)
      	 	      (setf (cdr (assoc (concat "session-" (if (stringp session) session (buffer-name session))) org-babel-sas-lepoint)) (point-max)))
      	     (setq a (+ b 1)))
      	   (sit-for 0.01)))))
      ;; get export value from sastab-tmp-file
      (org-babel-result-cond result-params
	(org-babel-chomp
	 (with-current-buffer (find-file-noselect sastab-tmp-file)
;	   (message ": %s" (buffer-string))
	   (buffer-string)
	   )
	 "\n")
	(org-babel-import-elisp-from-file sastab-tmp-file '(16))))
    (output
     ;; submit body through a temp buffer (in order to not go
     ;; beyond the limit of 500 bytes)
     ;; see 
     ;; https://stat.ethz.ch/pipermail/ess-help/2015-April/010518.html
    (let ((org-babel-sas-ess-process-name  (process-name (get-buffer-process session))))
      (with-temp-buffer
	(insert body)
	(let ((ess-local-process-name
	       (process-name (get-buffer-process session)))
	      (ess-eval-visibly-p nil))
	  (ess-eval-buffer nil)))
      (ess-send-string (get-process org-babel-sas-ess-process-name) org-babel-sas-eoe-indicator)
      ;;    excursion for cut/paste results from output buffer
      ;;   as output buffer is not the same as session buffer
      ;; org-babel-comint-with-output cannot be used 
      (save-excursion
      	(set-buffer (format "*%s.lst*" org-babel-sas-ess-process-name))
      	(let* ((a 0) (b 0) (ancienpoint (cdr (assoc (concat "session-" (if (stringp session) session (buffer-name session))) org-babel-sas-lepoint))))
      	 (while (< a org-babel-sas-timeout)
      	   (setq b a)
      	   (goto-char (cdr (assoc (concat "session-" (if (stringp session) session (buffer-name session))) org-babel-sas-lepoint)))
      	   (setq a (re-search-forward org-babel-sas-eoe-output nil t))
      	   (if a
      	       (progn (setq a org-babel-sas-timeout)
      	 	      (goto-char (cdr (assoc (concat "session-" (if (stringp session) session (buffer-name session))) org-babel-sas-lepoint)))
      	 	      (setq ancienpoint (cdr (assoc (concat "session-" (if (stringp session) session (buffer-name session))) org-babel-sas-lepoint)))
		      ;; well well, this is embarassing but
		      ;; as there's not history like in comint
		      ;; the last point is saved in this global
		      ;; alist variable (that will be used the
		      ;; next time)
      	 	      (setf (cdr (assoc (concat "session-" (if (stringp session) session (buffer-name session))) org-babel-sas-lepoint)) (point-max)))
      	     (setq a (+ b 1)))
      	   (sit-for 0.01))
       	 (org-babel-chomp (org-babel-sas-trim-end (org-babel-sas-trim-begin (org-babel-sas-trim-doubleline (org-babel-sas-trim-white (replace-regexp-in-string (concat "\\(\f\\)\\|\\(" org-babel-sas-boe-output "\\)\\|\\(" org-babel-sas-eoe-output "\\)") "" (buffer-substring ancienpoint (cdr (assoc (concat "session-" (if (stringp session) session (buffer-name session))) org-babel-sas-lepoint)))))))))))))))

(provide 'ob-sas)

;;; ob-sas.el ends here
