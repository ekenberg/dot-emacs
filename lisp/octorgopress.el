;;; octorgopress.el --- Octopress backend for Org-mode.

;;; Author: Michiyaki Yamada <spacemanaki@gmail.com>
;;; URL: https://github.com/spacemanaki/octorgopress
;;; Version: 20130828
;;; Keywords: org-mode, octopress, blog, literate-programming
;;; Package-Requires: ((org-mode "8.0.7"))

;;; Comentary:

;; Uses generic export. For more information see
;; http://orgmode.org/worg/dev/org-export-reference.html

;;; License:
;; MIT License. See License for more info

;;; Code:

(require 'ox-md)

(defun octorg:normalize-lang (str)
  (downcase (replace-regexp-in-string " " "-" str)))

;; pygments supports the following languages
(defvar *org-octopress-pygments-langs*
  (mapcar #'octorg:normalize-lang
          '("SML" "ActionScript" "Ada" "ANTLR" "AppleScript" "Assembly" "Asymptote" "Awk" "Befunge" "Boo" "BrainFuck" "C" "C++" "C#" "Clojure" "CoffeeScript" "ColdFusion" "Common Lisp" "Coq" "Cython" "D" "Dart" "Delphi" "Dylan" "Erlang" "Factor" "Fancy" "Fortran" "F#" "Gherkin" "GL shaders" "Groovy" "Haskell" "IDL" "Io" "Java" "JavaScript" "LLVM" "Logtalk" "Lua" "Matlab" "MiniD" "Modelica" "Modula-2" "MuPad" "Nemerle" "Nimrod" "Objective-C" "Objective-J" "Octave" "OCaml" "PHP" "Perl" "PovRay" "PostScript" "PowerShell" "Prolog" "Python" "Rebol" "Redcode" "Ruby" "Rust" "sh" "S" "S-Plus" "R" "Scala" "Scheme" "Scilab" "Smalltalk" "SNOBOL" "Tcl" "Text" "Vala" "Verilog" "VHDL" "Visual Basic.NET" "Visual FoxPro" "XQuery")))

(org-export-define-derived-backend 'octopress 'md
  :menu-entry
  '(?b "Export to Octopress"
       ((?B "As MARKDOWN buffer" org-octopress-export-as-octopress)
        (?b "As MARKDOWN file" org-octopress-export-to-octopress)))
  :options-alist '((:sidebar "SIDEBAR" nil nil t) (:publish "PUBLISH" nil "true" t) (:tags "TAGS" nil nil split)(:categories "CATEGORIES" nil nil split)(:title "TITLE" nil nil space) (:author "AUTHOR" nil user-full-name t) (:email "EMAIL" nil user-mail-address t) (:date "DATE" nil nil t))
  :translate-alist
  '(
    (headline . org-octopress-headline)
    (link . org-octopress-link)
    (paragraph . org-octopress-paragraph)
    (src-block . org-octopress-src-block)
    (quote-block . org-octopress-quote-block)
    (verse-block . org-octopress-plain-text-block)
    (code . org-octopress-verbatim)
    (inline-src-block . org-octopress-verbatim)
    (verbatim . org-octopress-verbatim)
    )
)

(defun get-lang (lang)
  (and lang
       (let ((lang (octorg:normalize-lang lang)))
         (cond ((string= lang "emacs-lisp") "common-lisp")
               ((not (member lang *org-octopress-pygments-langs*)) nil)
               (t lang)))))

(defun org-octopress-plain-text-block (input-block contents info)
  "Transcode a #+begin_verse or src-code lang:text block"
  ;; start each line with <tab> (except end-block) for the markdown conversion
  (setq contents (replace-regexp-in-string
                  "\t$" "" (replace-regexp-in-string
                            "^" "\t" contents)))
  (format "{:lang='text'}\n%s{:endlang}\n" contents)
  )

(defun org-octopress-src-block (src-block contents info)
  "Transcode a #+begin_src block from Org to Github style backtick code blocks"
  (let* ((lang (get-lang (org-element-property :language src-block)))
    (value (org-element-property :value src-block))
    (name (org-element-property :name src-block))
    (header
     ;; backtick code blocks support lang or lang and name, but not name alone
     (cond ((and lang name)
            (concat "``` " lang " " name "\n"))
           (lang
            (concat "``` " lang "\n"))
           (t "{% codeblock %}\n")))
    (footer (if lang "```\n" "{% endcodeblock %}\n")))
    (if (string-equal lang "text")
        (org-octopress-plain-text-block src-block value info)
      (concat
       header
       value
       footer
       contents))))


(defun org-octopress-quote-block (quote-block contents info)
  "Transcode a #+begin_quote block from Org to {% blockquote %}"
  (let* ((name (or (org-element-property :name quote-block) "bogus"))
         (header (format "{%% blockquote %s %%}\n" name))
         (footer "{% endblockquote %}\n")
         )
    (concat
     header
     contents
     footer)))

(defun repeat (x n)
  (let (acc)
    (dotimes (_ n acc)
      (push x acc))))

(defun org-octopress-headline (headline contents info)
  (let ((value (org-element-property :raw-value headline))
        (level (org-element-property :level headline)))
    (concat (apply 'concat (repeat "#" level))
            " "
            value
            "\n\n"
            contents)))

(defun org-octopress-link (link contents info)
  (let ((path (org-element-property :raw-link link)))
    (format "[%s](%s)" contents path)))

(defun org-octopress-paragraph (paragraph contents info)
  contents)

(defun org-octopress-verbatim (verbatim contents info)
  (let ((value (org-element-property :value verbatim)))
    (format (cond ((not (string-match "`" value)) "`%s`")
		  ((or (string-prefix-p "`" value)
		       (string-suffix-p "`" value))
		   "`` %s ``")
		  (t "``%s``"))
	    value)))

(defun org-octopress-export-as-octopress
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an octopress (markdown) buffer."
  (interactive)
  (org-export-to-buffer 'octopress "*Org Octopress Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

(defun org-octopress-export-to-octopress
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an octopress (markdown) file."
  (interactive)
  (let* ((extension ".md")
         (file (org-export-output-file-name extension subtreep)))
    (org-export-to-file 'octopress file
      async subtreep visible-only body-only ext-plist)))


;;; octorgopress.el ends here
