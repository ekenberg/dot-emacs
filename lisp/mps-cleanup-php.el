
(defun mps-cleanup-php (&optional indent)
  "Replace quotes \\\" -> ', format if/while/for/else constructs,
code-indent according to major mode if INDENT is given.
Works on region if active, else entire buffer"
  (interactive "P")
  (save-restriction
    (let ((orig-mode major-mode)
          (point-start (or (and (use-region-p) #'region-beginning) #'point-min))
          (point-end   (or (and (use-region-p) #'region-end) #'point-max))
          )
      (unwind-protect
          (progn
            ;; speed up search-and-replace using fundamental-mode
            (fundamental-mode)

            ;; if(...)  =>  if (...)
            ;; and so on with while/for/foreach/switch
            (replace-regexp-in-region "\\(if\\|while\\|for\\|foreach\\|switch\\)(" "\\1 (" (funcall point-start) (funcall point-end))

            ;; (...){   =>  (...) {
            (replace-string-in-region "){" ") {" (funcall point-start) (funcall point-end))

            ;; else{   =>  else {
            (replace-string-in-region "else{" "else {" (funcall point-start) (funcall point-end))

            ;; }else   =>
            ;; }
            ;; else
            (replace-string-in-region "}else" "}\nelse" (funcall point-start) (funcall point-end))

            ;; \"   =>  '
            (replace-string-in-region "\\\"" "'" (funcall point-start) (funcall point-end))

            ;; search-and-replace is done, restore original major mode
            (funcall orig-mode)

            ;; indent buffer
            (when indent
              (message "Indenting %s" (or (and (use-region-p) "region") "buffer"))
              (indent-region (funcall point-start) (funcall point-end)))

            ;; swiper to onload/javascript which will probably need manual fixing of quotes
            (goto-char (funcall point-start))
            (when (re-search-forward "onload\\|javascript" (funcall point-end) t)
              (add-to-history 'query-replace-defaults (cons "\\\"" "'"))
              (swiper "onload\\|javascript"))

            )
        (or (equal major-mode orig-mode) (funcall orig-mode))))))
