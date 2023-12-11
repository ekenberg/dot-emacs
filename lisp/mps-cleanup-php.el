(defun mps-cleanup-php (&optional indent)
  "Replace quotes \\\" -> ', format if/while/for/else constructs,
code-indent according to major mode if INDENT is given"
  (interactive "P")
  (save-restriction
    (let ((orig-mode major-mode))
      (unwind-protect
          (progn
            (fundamental-mode)

            ;; replace:
            ;; if(...)  =>
            ;; if (...)
            ;; and so on with while/for/foreach
            (message "Fixing if/while/for/foreach")
            (goto-char (point-min))
            (while (re-search-forward "\\(if\\|while\\|for\\|foreach\\)(" nil t)
              (replace-match "\\1 ("))

            ;; replace:
            ;; (...){   =>
            ;; (...) {
            (message "Fixing ){")
            (goto-char (point-min))
            (while (search-forward "){" nil t)
              (replace-match ") {"))

            ;; replace:
            ;; }else{   =>
            ;; }
            ;; else {
            (message "Fixing }else{")
            (goto-char (point-min))
            (while (re-search-forward "}[:blank:]*else[:blank:]*{" nil t)
              (replace-match "}\nelse {"))

            ;; replace:
            ;; \"   =>
            ;; '
            (message "Fixing quotes \\\" -> '")
            (goto-char (point-min))
            (while (search-forward "\\\"" nil t)
              (replace-match "'"))

            ;; search-and-replace is done, restore original major mode
            (funcall orig-mode)

            ;; indent buffer
            (when indent
              (message "Indenting buffer")
              (indent-region (point-min) (point-max)))

            ;; swipe to onload/javascript which will probably need manual fixing of quotes
            (goto-char (point-min))
            (when (re-search-forward "onload\\|javascript")
              (add-to-history 'query-replace-defaults (cons "\\\"" "'"))
              (swiper "onload\\|javascript"))

            )
        (or (equal major-mode orig-mode) (funcall orig-mode))))))
