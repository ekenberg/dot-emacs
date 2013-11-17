(require 'swap-windows)

(defun rotate-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.

Assumes that the frame is only split into two."

  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))

  (let ((is-vertical-now (window-combined-p))
        (first-window-is-selected (or (eql(window-left-child (window-parent)) (selected-window))
                                      (eql(window-top-child  (window-parent)) (selected-window)))))

      ; close current window
      (delete-window)

      ; do the split
      (if is-vertical-now
          (split-window-horizontally)
        (split-window-vertically))

      ; restore closed buffer
      (switch-to-buffer nil)

      ; restore previous window order
      (when (not first-window-is-selected) (swap-windows))))
