(defun kill-buffer-other-window ()
  "Kill the buffer in the next (clockwise) window without leaving the current window"
  (interactive)
  (cond ((< (count-windows) 2)
         (message "There is no other window to kill")
         )
        (t
            (other-window 1)
            (if (y-or-n-p (format "Kill buffer %s?" (buffer-name)))
                (kill-buffer))
            (other-window -1)
            (message ""))))

(provide 'kill-buffer-other-window)
