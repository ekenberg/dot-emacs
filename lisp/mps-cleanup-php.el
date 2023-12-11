(defun mps-cleanup-php ()
  "Replace quotes, format if/while/for/else etc, indent code"
  (interactive)
  (save-restriction
    ;; replace:
    ;; if(...)  =>
    ;; if (...)
    ;; and so on with while/for/foreach
    (goto-char (point-min))
    (while (re-search-forward "\\(if\\|while\\|for\\|foreach\\)(" nil t)
      (replace-match "\\1 ("))

    ;; replace:
    ;; (...){   =>
    ;; (...) {
    (goto-char (point-min))
    (while (re-search-forward "){" nil t)
      (replace-match ") {"))

    ;; replace:
    ;; }else{   =>
    ;; }
    ;; else {
    (goto-char (point-min))
    (while (re-search-forward "}[:blank:]*else[:blank:]*{" nil t)
      (replace-match "}\nelse {"))

    ;; replace:
    ;; \"   =>
    ;; '
    (goto-char (point-min))
    (while (re-search-forward "\\\\\"" nil t)
      (replace-match "'"))

    (add-to-history 'query-replace-defaults (cons "\\\"" "'"))

    ;; indent buffer
    (indent-region (point-min) (point-max))

    ;; swipe to onload/javascript which will probably need manual fixing of quotes
    (goto-char (point-min))
    (when (re-search-forward "onload\\|javascript")
        (swiper "onload\\|javascript"))

    ))
