(defun save-copy-as (new-filename)
  (interactive "FFilename:")
  (save-excursion
    (mark-whole-buffer)
    (write-region (point-min) (point-max) new-filename)
    )
)
