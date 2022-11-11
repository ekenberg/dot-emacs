;;; ob-php.el --- org-babel functions for php evaluation

;; Copyright (C) Tristan Huang

;; Author: Tristan Huang
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org
;; Version: 0.01

;;; License:

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

;; Org-Babel support for evaluating php source code.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(add-to-list 'org-babel-tangle-lang-exts '("php" . "php"))

(defvar org-babel-default-header-args:php '())

(defcustom org-babel-php-command "php"
  "Name of the command for executing php code."
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-babel
  :type 'string)

(defcustom org-babel-php-hline-to "null"
  "Replace hlines in incoming tables with this when translating to php."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string
  )

(defcustom org-babel-php-null-to 'hline
  "Replace `null' in php with this before returning."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'symbol)

(defconst org-babel-php-wrapper-method
  "<?php
function main() {%s}
$handle = fopen('%s', 'w'); fwrite($handle, var_export(main(), true)); fclose($handle);?>")

(defconst org-babel-php-pp-wrapper-method
  "<?php
function main() {%s}
$handle = fopen('%s', 'w');fwrite($handle, print_r(main(), true));fclose($handle);?>")

;; Convert
(defun org-babel-php-array-escape (results)
  "Safely convert php array into elisp lists."
  (org-babel-read
   (if (and (stringp results)
	    (string-prefix-p "array (" results))
       (concat "'"
	       (org-babel--script-escape-inner
		(replace-regexp-in-string
		 "array ($" "("
		 (replace-regexp-in-string
		  "[[:print:]\']+[[:space:]]=>[[:space:]]" "" results))))
     results)))

(defun org-babel-php-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (let ((res (org-babel-php-array-escape results)))
    (if (listp res)
	(mapcar (lambda (el) (if (eq el 'null)
				 org-babel-php-null-to el))
		res)
      res)))

(defun org-babel-php-var-to-php (var)
  "Convert an elisp var into a string of php source code
specifying a var of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-php-var-to-php var ", ") "]") ;; array
    (if (eq var 'hline)
	org-babel-php-hline-to
      (format "%S" var))))

(defun org-babel-variable-assignments:php (params)
  "Return list of php statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (format "$%s=%s;"
	     (car pair)
	     (org-babel-php-var-to-php (cdr pair))))
   (org-babel--get-vars params)))

;; Evaluate
(defun org-babel-execute:php (body params)
  "Execute a block of Template code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing php source code block")
  (let* ((session (org-babel-php-initiate-session
		   (cdr (assq :session params))))
         (result-params (cdr (assq :result-params params)))
	 (result-type (cdr (assq :result-type params)))
         (full-body (org-babel-expand-body:generic
                     body params (org-babel-variable-assignments:php params)))
	 (result (org-babel-php-evaluate
		  session full-body result-type result-params)))
    (org-babel-reassemble-table
     (org-babel-result-cond result-params
       result
       (org-babel-php-table-or-string result))
     (org-babel-pick-name (cdr (assq :colname-names params))
    			  (cdr (assq :colnames params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
    			  (cdr (assq :rownames params))))))

(defun org-babel-php-evaluate
    (session body &optional result-type result-params)
  "Evaluate BODY as php code."
  (if (not session)
      (org-babel-php-evaluate-external-process
       body result-type result-params)
    (org-babel-php-evaluate-session
     session body result-type result-params)))

(defun org-babel-php-evaluate-external-process
    (body &optional result-type result-params)
  "Evaluate BODY in external php process.
If RESULT-TYPE equals `output' then return standard output as a
string. If RESULT-TYPE equals `value' then return the value of the
last statement in BODY, as elisp."
  (pcase result-type
    (`output (org-babel-eval org-babel-php-command (format "<?php %s ?>" body)))
    (`value (let ((tmp-file (org-babel-temp-file "php-")))
	      (org-babel-eval
	       org-babel-php-command
	       (format
		(if (member "pp" result-params)
		    org-babel-php-pp-wrapper-method
		  org-babel-php-wrapper-method)
		body (org-babel-process-file-name tmp-file 'noquote)))
	      (org-babel-eval-read-file tmp-file)))))

;; Session
(defun org-babel-prep-session:php (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "Sessions are not supported for php"))

(defun org-babel-php-initiate-session (&optional session)
  "Sessions are not supported for php right now."
  nil)

(defun org-babel-php-evaluate-session
    (session body &optional result-type result-params)
  "TODO: Pass BODY to the php process in SESSION.")

(provide 'ob-php)
;;; ob-php.el ends here
