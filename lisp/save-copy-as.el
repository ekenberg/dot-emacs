(defun save-copy-as (new-filename)
  (interactive "FSave Copy As Filename: ")
  (save-excursion
    (mark-whole-buffer)
    (write-region (point-min) (point-max) new-filename)
    )
)
