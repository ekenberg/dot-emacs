(defun quit-popup-window ()
  "Quit the popup (help/debug etc) in other window (if there are 2 windows)"
  (interactive)

  (if (and (eq (count-windows) 2)
           (member (with-current-buffer (window-buffer (next-window)) major-mode)
                   '(help-mode
                     debugger-mode)))
      (quit-restore-window (next-window))

    (message "Cannot find a popup-window to quit")))

(provide 'quit-popup-window)
