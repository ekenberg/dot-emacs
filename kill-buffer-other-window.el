(defun kill-buffer-other-window ()
  "Kill the buffer in the next (clockwise) window without leaving the current window"
  (interactive)
  (progn
    (other-window 1)
    (kill-buffer)
    (other-window -1)))

(provide 'kill-buffer-other-window)
