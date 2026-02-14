;;; templ-ts-web-mode.el --- Web-editing conveniences for templ-ts-mode -*- lexical-binding: t; -*-

;; Author: Johan Ekenberg
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, templ, web
;; URL: https://github.com/ekenberg/templ-ts-web-mode

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Minor mode adding web-mode-like editing conveniences on top of
;; templ-ts-mode.  Uses tree-sitter for structural awareness — requires
;; a templ tree-sitter parser active in the buffer.
;;
;; Features:
;; - Auto-close tags: typing `>' after `<div' inserts `</div>'
;; - Auto-complete closing tags: typing `</' completes the tag name
;; - Smart Enter: RET between `<div>|</div>' opens a new indented line
;; - Element navigation: jump to beginning/end of enclosing element
;; - Element select: mark enclosing element, expand on repeat
;; - Element rename: change tag name in both open and close tags
;; - Element wrap: wrap region or enclosing element with a new tag

;;; Code:

(require 'treesit)

;;; Customization

(defgroup templ-ts-web nil
  "Web-editing conveniences for templ buffers."
  :group 'languages
  :prefix "templ-ts-web-")

;;; Constants

(defconst templ-ts-web--void-elements
  '("area" "base" "br" "col" "embed" "hr" "img" "input"
    "link" "meta" "source" "track" "wbr")
  "HTML void elements that must not have a closing tag.")

(defface templ-ts-web-data-attr-face
  '((t :inherit font-lock-builtin-face))
  "Face for `data-*' attribute names in templ HTML.
Customize to taste — the intent is to visually distinguish data
attributes from regular HTML attributes."
  :group 'templ-ts-web)

(defcustom templ-ts-web-tag-list
  '("a" "abbr" "address" "area" "article" "aside" "audio" "b"
    "base" "bdi" "bdo" "blockquote" "body" "br" "button" "canvas"
    "caption" "cite" "code" "col" "colgroup" "data" "datalist"
    "dd" "del" "details" "dfn" "dialog" "div" "dl" "dt" "em"
    "embed" "fieldset" "figcaption" "figure" "footer" "form" "h1"
    "h2" "h3" "h4" "h5" "h6" "head" "header" "hgroup" "hr" "html"
    "i" "iframe" "img" "input" "ins" "kbd" "label" "legend" "li"
    "link" "main" "map" "mark" "math" "menu" "meta" "meter" "nav"
    "noscript" "object" "ol" "optgroup" "option" "output" "p"
    "picture" "pre" "progress" "q" "rp" "rt" "ruby" "s" "samp"
    "script" "search" "section" "select" "slot" "small" "source"
    "span" "strong" "style" "sub" "summary" "sup" "svg" "table"
    "tbody" "td" "template" "textarea" "tfoot" "th" "thead" "time"
    "title" "tr" "track" "u" "ul" "var" "video" "wbr")
  "HTML tag names offered for completion.
Used by `templ-ts-web-element-rename' and other tag-prompting commands."
  :type '(repeat string)
  :group 'templ-ts-web)

(defcustom templ-ts-web-element-auto-close t
  "Auto-close tags when typing `>'.
When non-nil, typing `>' after `<div' inserts `</div>'."
  :type 'boolean
  :group 'templ-ts-web)

(defcustom templ-ts-web-element-auto-complete t
  "Auto-complete closing tags when typing `</'.
When non-nil, typing `/' after `<' inserts the matching tag name and `>'."
  :type 'boolean
  :group 'templ-ts-web)

(defcustom templ-ts-web-attr-auto-quote t
  "Insert double quotes after `=' in HTML attributes.
Disable if using `smartparens-mode' or `electric-pair-mode',
which provide their own quote pairing."
  :type 'boolean
  :group 'templ-ts-web)

(defvar templ-ts-web--tag-history nil
  "History list for tag name prompts.")

;;; Tree-sitter helpers

(defun templ-ts-web--in-templ-p ()
  "Return non-nil if the treesit language at point is `templ'."
  (eq (treesit-language-at (point)) 'templ))

(defun templ-ts-web--ancestor-of-type (node &rest types)
  "Walk up from NODE returning first ancestor whose type is in TYPES."
  (let ((cur (treesit-node-parent node)))
    (while (and cur (not (member (treesit-node-type cur) types)))
      (setq cur (treesit-node-parent cur)))
    cur))

(defun templ-ts-web--tag-name (tag-node)
  "Extract the tag name string from TAG-NODE (tag_start, tag_end, etc)."
  (when-let* ((name-node (treesit-node-child-by-field-name tag-node "name")))
    (treesit-node-text name-node t)))

(defun templ-ts-web--element-tag-name (element)
  "Return the tag name string from ELEMENT (element or self_closing_tag)."
  (pcase (treesit-node-type element)
    ("element"
     (when-let* ((tag (treesit-search-subtree element "^tag_start$" nil nil 1)))
       (templ-ts-web--tag-name tag)))
    ("self_closing_tag"
     (when-let* ((name (treesit-node-child-by-field-name element "name")))
       (treesit-node-text name t)))))

(defun templ-ts-web--element-name-nodes (element)
  "Return name nodes for ELEMENT, ordered start-to-end.
For an element, returns the name nodes from tag_start and tag_end.
For a self_closing_tag, returns a single name node."
  (pcase (treesit-node-type element)
    ("element"
     (let ((nodes nil))
       (when-let* ((tag-start (treesit-search-subtree element "^tag_start$" nil nil 1))
                   (name (treesit-node-child-by-field-name tag-start "name")))
         (push name nodes))
       (when-let* ((tag-end (treesit-search-subtree element "^tag_end$" nil nil 1))
                   (name (treesit-node-child-by-field-name tag-end "name")))
         (push name nodes))
       (nreverse nodes)))
    ("self_closing_tag"
     (when-let* ((name (treesit-node-child-by-field-name element "name")))
       (list name)))))

(defun templ-ts-web--find-tag-start-at (pos)
  "Find a `tag_start' node at or near POS.
Returns the tag_start node or nil."
  (let* ((node (treesit-node-at pos 'templ))
         (type (and node (treesit-node-type node))))
    (cond
     ;; Directly on tag_start
     ((equal type "tag_start") node)
     ;; On the ">" token — parent is tag_start
     ((equal type ">")
      (let ((parent (treesit-node-parent node)))
        (when (and parent (equal (treesit-node-type parent) "tag_start"))
          parent)))
     ;; Walk up to find tag_start ancestor
     (t (when node
          (templ-ts-web--ancestor-of-type node "tag_start"))))))

(defun templ-ts-web--enclosing-element (&optional pos)
  "Return the innermost `element' or `self_closing_tag' node containing POS.
POS defaults to point.  Handles gap positions where `treesit-node-at'
returns a nearby child rather than the true enclosing element."
  (let ((p (or pos (point))))
    (when-let* ((node (treesit-node-at p 'templ)))
      (let ((element
             (if (member (treesit-node-type node) '("element" "self_closing_tag"))
                 node
               (templ-ts-web--ancestor-of-type node "element" "self_closing_tag"))))
        ;; Gap detection: treesit-node-at finds the nearest node, which
        ;; may be a child element when point is in text between children.
        (when (and element
                   (or (< p (treesit-node-start element))
                       (>= p (treesit-node-end element))))
          (setq element (templ-ts-web--ancestor-of-type
                         element "element" "self_closing_tag")))
        element))))

(defun templ-ts-web--find-unclosed-tag-name ()
  "Find the innermost unclosed tag name at point using text scanning.
Scans forward from the enclosing component_block start to point,
tracking a stack of open/close tags.  Returns the tag name or nil."
  (let* ((node (treesit-node-at (point) 'templ))
         (scope (and node (templ-ts-web--ancestor-of-type node "component_block")))
         (start (if scope (treesit-node-start scope) (point-min)))
         (end (point))
         (open-stack nil))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "<\\(/\\)?\\([a-zA-Z][a-zA-Z0-9-]*\\)" end t)
        (let ((closing (match-string 1))
              (name (match-string 2)))
          (if closing
              ;; Closing tag: pop innermost matching open from stack
              (let ((found nil)
                    (result nil))
                (dolist (item open-stack)
                  (if (and (not found) (string= item name))
                      (setq found t)
                    (push item result)))
                (setq open-stack (nreverse result)))
            ;; Opening tag: push unless void or self-closing
            (unless (or (member (downcase name) templ-ts-web--void-elements)
                        (looking-at "[^>]*/[ \t]*>"))
              (push name open-stack))))))
    (car open-stack)))

;;; Feature: auto-close on `>'

(defun templ-ts-web--tag-is-unclosed-p (name)
  "Return non-nil if NAME has more opens than closes in the current scope.
Counts <NAME...> and </NAME> occurrences within the enclosing
component_block, avoiding reliance on tree-sitter's element pairing
which is unreliable during mid-edit parse states."
  (let* ((node (treesit-node-at (point) 'templ))
         (scope (and node (templ-ts-web--ancestor-of-type node "component_block")))
         (start (if scope (treesit-node-start scope) (point-min)))
         (end (if scope (treesit-node-end scope) (point-max)))
         (open-re (concat "<" (regexp-quote name) "[ \t\n>]"))
         (close-re (concat "</" (regexp-quote name) ">"))
         (opens 0)
         (closes 0))
    (save-excursion
      (goto-char start)
      (while (re-search-forward open-re end t)
        (setq opens (1+ opens)))
      (goto-char start)
      (while (re-search-forward close-re end t)
        (setq closes (1+ closes))))
    (> opens closes)))

(defun templ-ts-web--post-close-angle ()
  "After `>' is inserted, auto-close the tag if appropriate."
  (when (and templ-ts-web-element-auto-close
             (eq last-command-event ?>)
             (templ-ts-web--in-templ-p)
             ;; Not self-closing: char before `>' isn't `/'
             (not (eq (char-before (1- (point))) ?/)))
    ;; Re-parse so tree-sitter sees the just-typed `>'.
    (treesit-update-ranges)
    (let ((tag-start (templ-ts-web--find-tag-start-at (1- (point)))))
      (when tag-start
        (let ((name (templ-ts-web--tag-name tag-start)))
          (when (and name
                     (not (member (downcase name) templ-ts-web--void-elements))
                     ;; Confirm this tag_start ends at point — the `>'
                     ;; we just typed is the closing `>' of THIS tag,
                     ;; not some ancestor found by walking up.
                     (= (treesit-node-end tag-start) (point))
                     ;; Only auto-close if opens > closes in scope.
                     ;; Text-counting avoids tree-sitter's greedy pairing
                     ;; which steals close tags during mid-edit states.
                     (templ-ts-web--tag-is-unclosed-p name))
            (save-excursion (insert "</" name ">"))))))))

;;; Feature: auto-quote on `='

(defun templ-ts-web--post-equals ()
  "After `=' is inserted, auto-quote if inside an HTML tag attribute.
Inserts double quotes and positions point between them: attr=\"|\"."
  (when (and templ-ts-web-attr-auto-quote
             (eq last-command-event ?=)
             (>= (point) 3)
             (templ-ts-web--in-templ-p)
             ;; Don't fire if there's already a quote ahead.
             (not (looking-at-p "[ \t]*[\"'{]")))
    (treesit-update-ranges)
    ;; Check that the character(s) before `=' form an attribute_name node.
    (let ((node-before (treesit-node-at (- (point) 2) 'templ)))
      (when (and node-before
                 (equal (treesit-node-type node-before) "attribute_name"))
        (insert "\"\"")
        (backward-char)))))

;;; Feature: auto-complete on `</'

(defun templ-ts-web--post-close-slash ()
  "After `/' is inserted following `<', auto-complete the closing tag."
  (when (and templ-ts-web-element-auto-complete
             (eq last-command-event ?/)
             (>= (point) 3)
             (eq (char-before (1- (point))) ?<)
             (templ-ts-web--in-templ-p))
    (let ((name (save-excursion
                  (goto-char (- (point) 2))
                  (templ-ts-web--find-unclosed-tag-name))))
      (when name
        (insert name ">")))))

;;; Feature: smart Enter between tags

(defun templ-ts-web--between-tags-p ()
  "Return non-nil if point is in whitespace between an open and close tag.
Scans backward over whitespace for `>' of an opening tag and forward
over whitespace for `</', verifying only whitespace separates them."
  (save-excursion
    (let ((start (progn (skip-chars-backward " \t") (point))))
      (and (eq (char-before start) ?>)
           ;; Not a self-closing tag (/>)
           (not (eq (char-before (1- start)) ?/))
           ;; The > belongs to an opening tag, not a closing tag
           (save-excursion
             (goto-char (1- start))
             (and (search-backward "<" (max (- (point) 500) (point-min)) t)
                  (not (eq (char-after (1+ (point))) ?/))))
           ;; Forward from > finds only same-line whitespace then </
           (progn
             (goto-char start)
             (skip-chars-forward " \t")
             (looking-at "</"))))))

(defun templ-ts-web--smart-enter ()
  "If point is between an open and close tag, open an indented blank line.
Otherwise run the normal binding for RET."
  (interactive)
  (if (and (templ-ts-web--in-templ-p)
           (templ-ts-web--between-tags-p))
      (let ((open-end (save-excursion
                        (skip-chars-backward " \t")
                        (point)))
            (close-start (save-excursion
                           (skip-chars-forward " \t")
                           (point))))
        ;; Delete all whitespace between > and </
        (delete-region open-end close-start)
        (goto-char open-end)
        ;; Insert two newlines: one for the cursor line, one before the close tag
        (insert "\n\n")
        ;; Point is now on the line with the close tag — indent it
        (indent-according-to-mode)
        ;; Move back to the middle (blank) line and indent
        (forward-line -1)
        (indent-according-to-mode))
    ;; Not between tags — normal RET
    (newline-and-indent)))

;;; Feature: element navigation

(defun templ-ts-web-element-beginning ()
  "Move point to the beginning of the enclosing HTML element."
  (interactive)
  (when-let* ((element (templ-ts-web--enclosing-element)))
    (goto-char (treesit-node-start element))))

(defun templ-ts-web-element-end ()
  "Move point to the end of the enclosing HTML element."
  (interactive)
  (when-let* ((element (templ-ts-web--enclosing-element)))
    (goto-char (treesit-node-end element))))

(defun templ-ts-web-element-navigate ()
  "Jump between matching open and close tags of the enclosing element.
If point is within the closing tag, jump to the opening `<'.
Otherwise jump to the closing tag's `<'.
No-op on self-closing tags."
  (interactive)
  (when-let* ((element (templ-ts-web--enclosing-element)))
    (when (equal (treesit-node-type element) "element")
      (let ((tag-start (treesit-search-subtree element "^tag_start$" nil nil 1))
            (tag-end (treesit-search-subtree element "^tag_end$" nil nil 1)))
        (when (and tag-start tag-end)
          (if (and (<= (treesit-node-start tag-end) (point))
                   (<= (point) (treesit-node-end tag-end)))
              (goto-char (treesit-node-start tag-start))
            (goto-char (treesit-node-start tag-end))))))))

(defun templ-ts-web--sibling-reference (element)
  "Return the node to use for sibling navigation from ELEMENT.
When ELEMENT is a `self_closing_tag' wrapped in an `element',
returns the wrapper so that sibling traversal operates at the
correct tree level."
  (if (and (equal (treesit-node-type element) "self_closing_tag")
           (let ((parent (treesit-node-parent element)))
             (and parent (equal (treesit-node-type parent) "element")
                  parent)))
      (treesit-node-parent element)
    element))

(defun templ-ts-web-element-next ()
  "Move point to the beginning of the next sibling HTML element.
Stays within the same parent — does not cross parent boundaries."
  (interactive)
  (when-let* ((element (templ-ts-web--sibling-reference
                        (templ-ts-web--enclosing-element))))
    (let ((sibling (treesit-node-next-sibling element)))
      (while (and sibling
                  (not (member (treesit-node-type sibling)
                               '("element" "self_closing_tag"))))
        (setq sibling (treesit-node-next-sibling sibling)))
      (when sibling
        (goto-char (treesit-node-start sibling))))))

(defun templ-ts-web-element-previous ()
  "Move point to the beginning of the previous sibling HTML element.
Stays within the same parent — does not cross parent boundaries."
  (interactive)
  (when-let* ((element (templ-ts-web--sibling-reference
                        (templ-ts-web--enclosing-element))))
    (let ((sibling (treesit-node-prev-sibling element)))
      (while (and sibling
                  (not (member (treesit-node-type sibling)
                               '("element" "self_closing_tag"))))
        (setq sibling (treesit-node-prev-sibling sibling)))
      (when sibling
        (goto-char (treesit-node-start sibling))))))

(defun templ-ts-web-element-select ()
  "Select the enclosing HTML element.
On repeat, expand selection to the parent element."
  (interactive)
  (when-let* ((element (templ-ts-web--enclosing-element)))
    ;; If region already matches this element, expand to parent.
    (when (and (use-region-p)
               (= (region-beginning) (treesit-node-start element))
               (= (region-end) (treesit-node-end element)))
      (setq element (templ-ts-web--ancestor-of-type element "element")))
    (when element
      (push-mark (treesit-node-end element) nil t)
      (goto-char (treesit-node-start element)))))

;;; Feature: element rename

(defun templ-ts-web-element-rename (new-name)
  "Rename the enclosing HTML element's tag to NEW-NAME.
Replaces the tag name in both the opening and closing tags."
  (interactive
   (let ((name (when-let* ((el (templ-ts-web--enclosing-element)))
                 (templ-ts-web--element-tag-name el))))
     (list (completing-read
            (if name (format "Rename <%s> to: " name) "Tag name: ")
            templ-ts-web-tag-list nil nil nil
            'templ-ts-web--tag-history))))
  (when (and new-name (not (string-empty-p new-name)))
    (when-let* ((element (templ-ts-web--enclosing-element))
                (name-nodes (templ-ts-web--element-name-nodes element)))
      ;; Replace end-to-start to preserve positions.
      (dolist (node (sort (copy-sequence name-nodes)
                          (lambda (a b) (> (treesit-node-start a)
                                           (treesit-node-start b)))))
        (delete-region (treesit-node-start node) (treesit-node-end node))
        (goto-char (treesit-node-start node))
        (insert new-name)))))

;;; Feature: element wrap

(defun templ-ts-web-element-wrap (tag-name)
  "Wrap the region or enclosing element with TAG-NAME tags.
With an active region, wraps the selected text.  Otherwise wraps
the enclosing element.  Block-wraps (with newlines and
re-indentation) when the content spans multiple lines or when
wrapping a whole element; inline-wraps otherwise."
  (interactive
   (list (completing-read "Wrap with tag: "
                          templ-ts-web-tag-list nil nil nil
                          'templ-ts-web--tag-history)))
  (when (and tag-name (not (string-empty-p tag-name)))
    (let (beg end block-p)
      (if (use-region-p)
          (setq beg (region-beginning)
                end (region-end)
                block-p (string-match-p "\n" (buffer-substring-no-properties beg end)))
        (when-let* ((element (templ-ts-web--enclosing-element)))
          (setq beg (treesit-node-start element)
                end (treesit-node-end element)
                block-p t)))
      (when (and beg end)
        (let ((open-tag (concat "<" tag-name ">"))
              (close-tag (concat "</" tag-name ">")))
          (goto-char end)
          (if block-p
              (insert "\n" close-tag)
            (insert close-tag))
          (goto-char beg)
          (if block-p
              (insert open-tag "\n")
            (insert open-tag))
          (when block-p
            (treesit-update-ranges)
            (indent-region beg (+ end (length open-tag) (length close-tag) 2)))
          (goto-char beg))))))

;;; Feature: element kill

(defun templ-ts-web-element-kill ()
  "Kill the enclosing HTML element (tags and content).
Puts the killed text on the kill ring.  When point is in the gap
between child elements (e.g. leading whitespace), kills the parent
element rather than the next child."
  (interactive)
  (when-let* ((element (templ-ts-web--enclosing-element)))
    ;; treesit-node-at returns the nearest node at-or-after point.
    ;; When point is in a gap between children (whitespace before or
    ;; after an element), it finds the nearest child — but point
    ;; isn't actually inside it.  Go to parent.
    (when (or (< (point) (treesit-node-start element))
              (>= (point) (treesit-node-end element)))
      (setq element (or (templ-ts-web--ancestor-of-type element
                                                         "element" "self_closing_tag")
                        element)))
    (kill-region (treesit-node-start element) (treesit-node-end element))))

;;; Feature: element vanish (unwrap)

(defun templ-ts-web--line-empty-p ()
  "Return non-nil if the current line contains only whitespace."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[ \t]*$")))

(defun templ-ts-web--delete-current-line ()
  "Delete the current line including its newline."
  (delete-region (line-beginning-position)
                 (min (1+ (line-end-position)) (point-max))))

(defun templ-ts-web-element-vanish ()
  "Remove the enclosing element's tags, keeping its content.
Cleans up empty lines left by tag removal and re-indents the
remaining content.  For self-closing tags, removes the entire element."
  (interactive)
  (when-let* ((element (templ-ts-web--enclosing-element)))
    (pcase (treesit-node-type element)
      ("self_closing_tag"
       (delete-region (treesit-node-start element) (treesit-node-end element)))
      ("element"
       (let* ((tag-start (treesit-search-subtree element "^tag_start$" nil nil 1))
              (tag-end (treesit-search-subtree element "^tag_end$" nil nil 1)))
         (when (and tag-start tag-end)
           (let ((beg (copy-marker (treesit-node-start element)))
                 (end (copy-marker (treesit-node-end element))))
             ;; Delete end tag first to preserve start positions.
             (delete-region (treesit-node-start tag-end) (treesit-node-end tag-end))
             (delete-region (treesit-node-start tag-start) (treesit-node-end tag-start))
             ;; Remove empty boundary lines (end first to preserve beg).
             (save-excursion
               (goto-char end)
               (when (templ-ts-web--line-empty-p)
                 (templ-ts-web--delete-current-line)))
             (save-excursion
               (goto-char beg)
               (when (templ-ts-web--line-empty-p)
                 (templ-ts-web--delete-current-line)))
             ;; Re-indent remaining content.
             (indent-region beg end)
             (goto-char beg)
             (set-marker beg nil)
             (set-marker end nil))))))))

;;; Feature: element close

(defun templ-ts-web--inside-tag-p ()
  "Return non-nil if point is inside an HTML tag.
First checks tree-sitter for a `tag_start', `tag_end', or
`self_closing_tag' ancestor with point strictly inside its range.
Falls back to a text heuristic (unclosed `<' before point) to
catch partial tags in ERROR nodes."
  (or
   ;; Tree-sitter: proper tag node containing point.
   (when-let* ((node (treesit-node-at (point) 'templ))
               (tag (or (and (member (treesit-node-type node)
                                     '("tag_start" "tag_end" "self_closing_tag"))
                             node)
                        (templ-ts-web--ancestor-of-type
                         node "tag_start" "tag_end" "self_closing_tag"))))
     (and (>= (point) (treesit-node-start tag))
          (< (point) (treesit-node-end tag))))
   ;; Text fallback: an unclosed `<' before point (handles ERROR nodes).
   (save-excursion
     (let ((pos (point)))
       (and (search-backward "<" (max (- pos 500) (point-min)) t)
            (not (search-forward ">" pos t)))))))

(defun templ-ts-web-element-close ()
  "Close the innermost unclosed HTML element by inserting its closing tag.
Adapts to context:
- After `</': inserts the tag name (and `>' if needed).
- After `<': inserts `/name>' (and `>' if needed).
- In content: inserts `</name>'.
Does nothing if point is inside a tag, no unclosed element exists,
or the context is not templ HTML."
  (interactive)
  (when (templ-ts-web--in-templ-p)
    (let ((has-close-angle (looking-at-p "[ \t]*>")))
      (cond
       ;; After "</" with optional partial name — complete the closing tag
       ((looking-back "</\\([a-zA-Z]*\\)" (- (point) 50))
        (let* ((partial (match-string 1))
               (name (save-excursion
                       (goto-char (match-beginning 0))
                       (templ-ts-web--find-unclosed-tag-name))))
          (when (and name (string-prefix-p partial name))
            (insert (substring name (length partial))
                    (if has-close-angle "" ">")))))
       ;; After "<" (but not "</") — insert /name>
       ((and (looking-back "<" (1- (point)))
             (not (looking-back "</" (- (point) 2)))
             (not (looking-at-p "[a-zA-Z/]")))
        (when-let* ((name (save-excursion
                            (goto-char (1- (point)))
                            (templ-ts-web--find-unclosed-tag-name))))
          (insert "/" name (if has-close-angle "" ">"))))
       ;; General case — insert </name> if not inside a tag
       ((not (templ-ts-web--inside-tag-p))
        (when-let* ((name (templ-ts-web--find-unclosed-tag-name)))
          (insert "</" name ">")))))))

;;; Feature: select element content

(defun templ-ts-web-element-content-select ()
  "Select the content of the enclosing HTML element (between the tags).
No-op on self-closing tags."
  (interactive)
  (when-let* ((element (templ-ts-web--enclosing-element)))
    (when (equal (treesit-node-type element) "element")
      (let ((tag-start (treesit-search-subtree element "^tag_start$" nil nil 1))
            (tag-end (treesit-search-subtree element "^tag_end$" nil nil 1)))
        (when (and tag-start tag-end)
          (push-mark (treesit-node-start tag-end) nil t)
          (goto-char (treesit-node-end tag-start)))))))

;;; Feature: element clone

(defun templ-ts-web-element-clone ()
  "Clone the enclosing HTML element, inserting a copy on the next line.
The clone is placed at the same indentation as the original.
Point is left at the beginning of the clone."
  (interactive)
  (when-let* ((element (templ-ts-web--enclosing-element)))
    ;; Gap detection: if point is outside the found element, use parent.
    (when (or (< (point) (treesit-node-start element))
              (>= (point) (treesit-node-end element)))
      (setq element (or (templ-ts-web--ancestor-of-type element
                                                         "element" "self_closing_tag")
                        element)))
    (let* ((beg (treesit-node-start element))
           (end (treesit-node-end element))
           (text (buffer-substring-no-properties beg end))
           (col (save-excursion (goto-char beg) (current-column))))
      (goto-char end)
      (insert "\n" (make-string col ?\s) text)
      (goto-char (+ end 1 col)))))

;;; Feature: data-* attribute fontification

(defun templ-ts-web--install-data-attr-fontification ()
  "Add tree-sitter font-lock rules for `data-*' attribute names.
Idempotent — removes any existing data-attr rules before appending."
  ;; Remove stale data-attr entries (idempotent reinstall).
  (setq treesit-font-lock-settings
        (append (seq-remove (lambda (entry) (eq (nth 2 entry) 'data-attr))
                            (or treesit-font-lock-settings '()))
                (treesit-font-lock-rules
                 :language 'templ
                 :feature 'data-attr
                 :override t
                 '(((attribute_name) @templ-ts-web-data-attr-face
                    (:match "\\`data-" @templ-ts-web-data-attr-face))))))
  ;; Add feature to level 1 if not already present.
  (unless (seq-some (lambda (level) (memq 'data-attr level))
                    treesit-font-lock-feature-list)
    (if treesit-font-lock-feature-list
        (setf (car treesit-font-lock-feature-list)
              (append (car treesit-font-lock-feature-list) '(data-attr)))
      (setq treesit-font-lock-feature-list '((data-attr)))))
  (treesit-font-lock-recompute-features)
  (font-lock-flush))

(defun templ-ts-web--remove-data-attr-fontification ()
  "Remove tree-sitter font-lock rules for `data-*' attribute names."
  (setq treesit-font-lock-settings
        (seq-remove (lambda (entry) (eq (nth 2 entry) 'data-attr))
                    (or treesit-font-lock-settings '())))
  (setq treesit-font-lock-feature-list
        (mapcar (lambda (level) (remq 'data-attr level))
                treesit-font-lock-feature-list))
  (treesit-font-lock-recompute-features)
  (font-lock-flush))

;;; Feature: mark and expand

(defvar-local templ-ts-web--expand-state nil
  "Current expansion level for `templ-ts-web-mark-and-expand'.
One of nil, \"attribute\", \"element-content\", \"element\",
or \"ceiling\".  Cleared when the mark is deactivated.")

(defun templ-ts-web--expand-clear-state ()
  "Clear mark-and-expand state.  Added to `deactivate-mark-hook'."
  (setq templ-ts-web--expand-state nil))

(defun templ-ts-web--expand-find-context ()
  "Determine the expansion context at point.
Returns (TYPE . NODE) where TYPE is one of `attribute', `tag',
`element-content', or `element', and NODE is the relevant
tree-sitter node."
  (let ((node (treesit-node-at (point) 'templ)))
    (when node
      (let ((cur node) found)
        ;; Walk up from node at point to find first interesting ancestor.
        ;; Skip any node that doesn't actually contain point — treesit-node-at
        ;; returns the nearest node, which may be a sibling/child when point
        ;; is in a text gap between elements.
        (while (and cur (not found))
          (if (or (< (point) (treesit-node-start cur))
                  (>= (point) (treesit-node-end cur)))
              ;; Node doesn't contain point — skip to parent.
              (setq cur (treesit-node-parent cur))
            (let ((type (treesit-node-type cur)))
              (cond
               ((equal type "attribute")
                (setq found (cons 'attribute cur)))
               ((member type '("tag_start" "tag_end"))
                ;; Point is inside a tag — select the whole element.
                (when-let* ((el (templ-ts-web--ancestor-of-type cur "element")))
                  (setq found (cons 'element el))))
               ((equal type "self_closing_tag")
                (setq found (cons 'element cur)))
               ((equal type "element")
                (let ((ts (treesit-search-subtree cur "^tag_start$" nil nil 1))
                      (te (treesit-search-subtree cur "^tag_end$" nil nil 1)))
                  (if (and ts te)
                      (setq found (cons 'element-content cur))
                    (setq found (cons 'element cur)))))
               ((equal type "component_block")
                ;; Don't go above component_block — use nearest element.
                (setq found nil cur nil))))
            (unless found
              (setq cur (treesit-node-parent cur)))))
        ;; Fallback: try enclosing element.
        (unless found
          (when-let* ((el (templ-ts-web--enclosing-element)))
            (setq found (cons 'element el))))
        found))))

(defun templ-ts-web--expand-select (beg end state)
  "Set region to BEG..END and record expansion STATE."
  (goto-char beg)
  (push-mark end nil t)
  (setq templ-ts-web--expand-state state))

(defun templ-ts-web--expand-to-component-block ()
  "Expand selection to the component_block content (ceiling).
Returns non-nil on success."
  (when-let* ((node (treesit-node-at (point) 'templ))
              (cb (templ-ts-web--ancestor-of-type node "component_block")))
    ;; Select from after opening `{' to before closing `}'.
    ;; component_block children: first is `{', last is `}'.
    (let ((first-child (treesit-node-child cb 0))
          (last-child (treesit-node-child cb (1- (treesit-node-child-count cb)))))
      (when (and first-child last-child)
        (templ-ts-web--expand-select
         (treesit-node-end first-child)
         (treesit-node-start last-child)
         "ceiling")
        t))))

(defun templ-ts-web-mark-and-expand ()
  "Progressively expand the region through structural levels.
Each call expands to the next level: attribute, element content,
element, then parent levels up to the component block.
Use \\[keyboard-quit] to deactivate the mark and reset."
  (interactive)
  (unless (templ-ts-web--in-templ-p)
    (user-error "Not in a templ context"))
  (pcase templ-ts-web--expand-state
    ;; Already at ceiling — no-op.
    ("ceiling" nil)

    ;; Attribute → expand to enclosing element.
    ("attribute"
     (when-let* ((node (treesit-node-at (point) 'templ))
                 (el (templ-ts-web--ancestor-of-type
                      node "element" "self_closing_tag")))
       (templ-ts-web--expand-select
        (treesit-node-start el) (treesit-node-end el) "element")))

    ;; Element content → expand to full element.
    ;; Find the element whose content bounds match the current region,
    ;; not just any element at point (avoids confusion when a sole child
    ;; element starts at the same position as the parent's content).
    ("element-content"
     (let ((beg (region-beginning))
           (end (region-end)))
       (when-let* ((node (treesit-node-at beg 'templ)))
         (let ((el node))
           ;; Walk up to find the element whose content matches the region.
           (while (and el
                       (not (and (equal (treesit-node-type el) "element")
                                 (let ((ts (treesit-search-subtree el "^tag_start$" nil nil 1))
                                       (te (treesit-search-subtree el "^tag_end$" nil nil 1)))
                                   (and ts te
                                        (= (treesit-node-end ts) beg)
                                        (= (treesit-node-start te) end))))))
             (setq el (treesit-node-parent el)))
           (when el
             (templ-ts-web--expand-select
              (treesit-node-start el) (treesit-node-end el) "element"))))))

    ;; Element → expand to parent content, or ceiling.
    ;; Find the element whose bounds match the current region, then
    ;; get its parent.
    ("element"
     (let ((beg (region-beginning))
           (end (region-end)))
       (when-let* ((node (treesit-node-at beg 'templ)))
         (let ((el node))
           ;; Walk up to find the element matching the current region.
           (while (and el
                       (not (and (member (treesit-node-type el)
                                         '("element" "self_closing_tag"))
                                 (= (treesit-node-start el) beg)
                                 (= (treesit-node-end el) end))))
             (setq el (treesit-node-parent el)))
           ;; Find a parent element strictly larger than the current one.
           ;; Needed because self_closing_tag is wrapped in an element
           ;; node with identical bounds — we must skip that wrapper.
           (let ((parent
                  (when el
                    (let ((p (templ-ts-web--ancestor-of-type el "element")))
                      (while (and p
                                  (= (treesit-node-start p) beg)
                                  (= (treesit-node-end p) end))
                        (setq p (templ-ts-web--ancestor-of-type p "element")))
                      p))))
             (cond
              ;; Parent element exists — expand to its content.
              ((and parent (equal (treesit-node-type parent) "element"))
               (let ((ts (treesit-search-subtree parent "^tag_start$" nil nil 1))
                     (te (treesit-search-subtree parent "^tag_end$" nil nil 1)))
                 (if (and ts te)
                     (templ-ts-web--expand-select
                      (treesit-node-end ts) (treesit-node-start te) "element-content")
                   (templ-ts-web--expand-to-component-block))))
              ;; No parent element — ceiling.
              (t (templ-ts-web--expand-to-component-block))))))))

    ;; No state / fresh start — detect context and select.
    (_
     (when-let* ((ctx (templ-ts-web--expand-find-context))
                 (type (car ctx))
                 (node (cdr ctx)))
       (pcase type
         ('attribute
          (templ-ts-web--expand-select
           (treesit-node-start node) (treesit-node-end node) "attribute"))
         ('element-content
          (let ((ts (treesit-search-subtree node "^tag_start$" nil nil 1))
                (te (treesit-search-subtree node "^tag_end$" nil nil 1)))
            (when (and ts te)
              (templ-ts-web--expand-select
               (treesit-node-end ts) (treesit-node-start te) "element-content"))))
         ('element
          (templ-ts-web--expand-select
           (treesit-node-start node) (treesit-node-end node) "element")))))))

;;; Minor mode

(defvar templ-ts-web-mode-map
  (let ((map (make-sparse-keymap))
        (elt-map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'templ-ts-web--smart-enter)
    (define-key map (kbd "C-c C-e") elt-map)
    (define-key map (kbd "C-c C-m") #'templ-ts-web-mark-and-expand)
    ;; Element prefix: C-c C-e
    (define-key elt-map (kbd "/") #'templ-ts-web-element-close)
    (define-key elt-map (kbd "a") #'templ-ts-web-element-content-select)
    (define-key elt-map (kbd "b") #'templ-ts-web-element-beginning)
    (define-key elt-map (kbd "c") #'templ-ts-web-element-clone)
    (define-key elt-map (kbd "e") #'templ-ts-web-element-end)
    (define-key elt-map (kbd "k") #'templ-ts-web-element-kill)
    (define-key elt-map (kbd "m") #'templ-ts-web-element-navigate)
    (define-key elt-map (kbd "n") #'templ-ts-web-element-next)
    (define-key elt-map (kbd "p") #'templ-ts-web-element-previous)
    (define-key elt-map (kbd "r") #'templ-ts-web-element-rename)
    (define-key elt-map (kbd "s") #'templ-ts-web-element-select)
    (define-key elt-map (kbd "v") #'templ-ts-web-element-vanish)
    (define-key elt-map (kbd "w") #'templ-ts-web-element-wrap)
    map)
  "Keymap for `templ-ts-web-mode'.")

;;;###autoload
(define-minor-mode templ-ts-web-mode
  "Web-editing conveniences for templ buffers.

Adds auto-close tags, closing tag completion, and smart Enter
on top of a templ tree-sitter major mode."
  :lighter " tw"
  :keymap templ-ts-web-mode-map
  (if templ-ts-web-mode
      (progn
        (add-hook 'post-self-insert-hook #'templ-ts-web--post-close-angle nil t)
        (add-hook 'post-self-insert-hook #'templ-ts-web--post-close-slash nil t)
        (add-hook 'post-self-insert-hook #'templ-ts-web--post-equals nil t)
        (add-hook 'deactivate-mark-hook #'templ-ts-web--expand-clear-state nil t)
        (templ-ts-web--install-data-attr-fontification))
    (remove-hook 'post-self-insert-hook #'templ-ts-web--post-close-angle t)
    (remove-hook 'post-self-insert-hook #'templ-ts-web--post-close-slash t)
    (remove-hook 'post-self-insert-hook #'templ-ts-web--post-equals t)
    (remove-hook 'deactivate-mark-hook #'templ-ts-web--expand-clear-state t)
    (templ-ts-web--remove-data-attr-fontification)))

(provide 'templ-ts-web-mode)

;;; templ-ts-web-mode.el ends here
