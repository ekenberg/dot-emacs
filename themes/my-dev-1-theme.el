;; ---------------------------
;;
;; my-dev-1-theme
;; Baserat på:
;; Spolsky : A dark color theme samt emacs default
;;
;; ----------------------------

(unless (>= 24 emacs-major-version)
  (error "requires Emacs 24 or later."))

(deftheme my-dev-1  "A dark color theme for Emacs based on Sublime Text 2")

(custom-theme-set-variables
  'my-dev-1
  '(linum-format " %7i "))

(let ((*background*         "#050505")
;;      (*comments*           "#8C8C8C")
      (*comments*           "#AF0000")
;;      (*constant*           "#FF80F4")
      (*constant*           "#C41EB5")
      (*current-line*       "#151515")
      (*cursor-underscore*  "#EEDC82")
;;      (*keywords*           "#F92672")
      (*keywords*           "#00ffff")

      ;; Sidebar line numbers
      (*line-number*        "#161A1F")
      (*line-fg*            "#666")

      (*type-face*          "#66D9EF")
;;      (*method-declaration* "#A6E22E")
      (*method-declaration* "#5c5cff")
      (*mode-line-bg*       "#333")
      (*mode-inactive-bg*   "#222")
      (*mode-line-fg*       "#EEDC82")
;;      (*mode-inactive-fg*   "#555")
      (*mode-inactive-fg*   "#777")
      (*normal*             "#DEDEDE")
      (*number*             "#FC580C")
;;      (*operators*          "#FF80F4")
      (*operators*          "#cd00cd")
      (*warning*            "#FF6C60")
      (*regexp*             "#A63A62")
;;      (*string*             "#EEDC82")
      (*string*             "#17A917")
;;      (*variable*           "#FD971F")
      (*variable*           "#D78700")
      (*visual-selection*   "#555")
      (*diff-add*             "#17A917")
      (*diff-del*             "#AF0000"))

  (custom-theme-set-faces
   'my-dev-1

   `(bold ((t (:bold t))))
   `(button ((t (:foreground, *keywords* :underline t))))
   `(default ((t (:background, *background* :foreground, *normal*))))
   `(header-line ((t (:background, *mode-line-bg* :foreground, *normal*)))) ;; info header
   `(highlight ((t (:background, *current-line*))))
   `(highlight-face ((t (:background, *current-line*))))
   `(hl-line ((t (:background, *current-line* :underline t))))
   `(info-xref ((t (:foreground, *keywords* :underline t))))
   `(region ((t (:background, *visual-selection*))))
   `(underline ((nil (:underline t))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground, *operators*))))
   `(font-lock-comment-delimiter-face ((t (:foreground, *comments*))))
   `(font-lock-comment-face ((t (:foreground, *comments*))))
   `(font-lock-constant-face ((t (:foreground, *constant*))))
   `(font-lock-doc-face ((t (:foreground, *string*))))
   `(font-lock-doc-string-face ((t (:foreground, *string*))))
   `(font-lock-function-name-face ((t (:foreground, *method-declaration*))))
   `(font-lock-keyword-face ((t (:foreground, *keywords*))))
   `(font-lock-negation-char-face ((t (:foreground, *warning*))))
   `(font-lock-number-face ((t (:foreground, *number*))))
   `(font-lock-preprocessor-face ((t (:foreground, *method-declaration*))))
   `(font-lock-reference-face ((t (:foreground, *constant*))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground, *regexp*))))
   `(font-lock-regexp-grouping-construct ((t (:foreground, *regexp*))))
   `(font-lock-string-face ((t (:foreground, *string*))))
   `(font-lock-type-face ((t (:foreground, *type-face*))))
   `(font-lock-variable-name-face ((t (:foreground, *variable*))))
   `(font-lock-warning-face ((t (:foreground, *warning*))))

   ;; GUI
   `(fringe ((t (:background, *background*))))
   `(linum ((t (:background, *line-number* :foreground, *line-fg*))))
   `(minibuffer-prompt ((t (:foreground, *variable*))))
   `(mode-line ((t (:background, *mode-line-bg* :foreground, *mode-line-fg*))))
   `(mode-line-inactive ((t (:background, *mode-inactive-bg* :foreground, *mode-inactive-fg*))))
   `(cursor ((t (:background, *cursor-underscore*))))
   `(text-cursor ((t (:background, *cursor-underscore*))))
   `(vertical-border ((t (:foreground, *background*)))) ;; between splits

   ;; show-paren
   `(show-paren-mismatch ((t (:background, *warning* :foreground, *normal* :weight bold))))
   `(show-paren-match ((t (:background, *visual-selection* :foreground, *normal* :weight bold))))

   ;; search
   `(isearch ((t (:background, *regexp* :foreground, *visual-selection*))))
   `(isearch-fail ((t (:background, *warning*))))
   `(lazy-highlight ((t (:background, *operators* :foreground, *visual-selection*))))

   ;; Magit
   `(magit-diff-add ((t (:foreground, *diff-add* :background, *background*))))
   `(magit-diff-del ((t (:foreground, *diff-del* :background, *background*))))
   `(magit-item-highlight ((t (:background, *background*))))
   `(magit-diff-hunk-header ((t (:background, *mode-inactive-bg*))))
   `(magit-diff-file-header ((t (:background, *mode-inactive-bg*))))

   ;; grep/compile highlight match
   `(next-error ((t (:background, *method-declaration*))))

   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'my-dev-1)

;; Local Variables:
;; no-byte-compile: t
;; End:
