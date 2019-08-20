;; loading ob-sas
(load "Z:/ob-sas.el")
(require 'ob-sas)
;(require 'ob-R)
;; adding sas language to org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sas . t) ))
;; variable for ob-sas
(setq org-babel-sas-windows t)
(setq org-babel-sas-realsession nil)
(setq org-babel-sas-command "C:\\Progra~1\\SASHome\\SASFoundation\\9.4\\sas.exe")
;; no confirmation for evaluation
(setq org-confirm-babel-evaluate nil)
;; fontify source block (with ess it leading to syntax coloration)
(setq org-src-fontify-natively t)
