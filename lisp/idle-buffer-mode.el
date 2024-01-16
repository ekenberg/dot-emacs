
;; buffer-local variables:
(defvar-local idle-buffer-auto-save-sec     0   "How many seconds idle buffer before saving, 0 means disabled")
(defvar-local idle-buffer-auto-close-sec    0   "How many seconds idle buffer before closing, 0 means disabled")

(defvar-local IB/save-idle-timer            nil "For internal use only")
(defvar-local IB/close-idle-timer           nil "For internal use only")
(defvar-local IB/save-timer                 nil "For internal use only")
(defvar-local IB/close-timer                nil "For internal use only")
(defvar-local IB/poll-timer                 nil "For internal use only")

(defconst idle-buffer-poll-sec 1 "Poll interval for checking buffer focus and timers")

(define-minor-mode idle-buffer-mode
  "Perform automatic actions on idle buffers."
  :lighter "IB"

  ;; check file-local value of each relevant variable and start timers if applicable
  (if idle-buffer-mode
      ;; mode toggled ON
      (progn

        (when (> idle-buffer-auto-save-sec 0)
          ;;(message "idle-buffer-mode: %s activating idle auto save after %d sec" (buffer-name) idle-buffer-auto-save-sec)
          (IB/start-save-idle-timer (current-buffer) idle-buffer-auto-save-sec))

        (when (> idle-buffer-auto-close-sec 0)
          ;;(message "idle-buffer-mode: %s activating idle auto close after %d sec" (buffer-name) idle-buffer-auto-close-sec)
          (IB/start-close-idle-timer (current-buffer) idle-buffer-auto-close-sec))

        (when (or (> idle-buffer-auto-save-sec 0)
                  (> idle-buffer-auto-close-sec 0))
          ;;(message "idle-buffer-mode: starting poll-timer with interval %d sec" idle-buffer-poll-sec)
          (IB/start-poll-timer (current-buffer) idle-buffer-poll-sec))

        (add-hook 'kill-buffer-hook #'IB/kill-buffer-handler))

    ;; mode toggled OFF
    (IB/cancel-all-timers)
    (remove-hook 'kill-buffer-hook #'IB/kill-buffer-handler)))

(defun IB/kill-buffer-handler ()
  (IB/cancel-all-timers))

(defun IB/cancel-all-timers ()
  (IB/cancel-timer 'IB/poll-timer)
  (IB/cancel-timer 'IB/save-idle-timer)
  (IB/cancel-timer 'IB/close-idle-timer)
  (IB/cancel-timer 'IB/save-timer)
  (IB/cancel-timer 'IB/close-timer))

(defun IB/start-poll-timer (my-buffer interv)
  (setq IB/poll-timer (run-with-timer interv interv 'IB/run-poll my-buffer)))

(defun IB/start-save-timer (buf sec)
  (with-current-buffer buf
    (when (null IB/save-timer)
      (setq IB/save-timer (run-with-timer sec sec #'IB/save buf)))))

(defun IB/start-close-timer (buf sec)
  (with-current-buffer buf
    (when (null IB/close-timer)
      (setq IB/close-timer (run-with-timer sec nil #'IB/close buf))))) ; no repeat for close timer

(defun IB/start-save-idle-timer (buf sec)
  (when (null IB/save-idle-timer)
    (setq IB/save-idle-timer (run-with-idle-timer sec t #'IB/save buf))))

(defun IB/start-close-idle-timer (buf sec)
  (when (null IB/close-idle-timer)
    (setq IB/close-idle-timer (run-with-idle-timer sec nil #'IB/close buf)))) ; no repeat for close timer

(defun IB/run-poll (my-buffer)
  (if (not (buffer-live-p my-buffer))
      ;; buffer gone sayonara
      (IB/cancel-all-timers)
    ;; buffer still here
    (if (eq my-buffer (current-buffer))
        ;; buffer has focus
        (progn
          (IB/cancel-timer 'IB/save-timer)
          (IB/cancel-timer 'IB/close-timer))
      ;; buffer has lost focus
      (IB/start-save-timer my-buffer (buffer-local-value 'idle-buffer-auto-save-sec my-buffer))
      (IB/start-close-timer my-buffer (buffer-local-value 'idle-buffer-auto-close-sec my-buffer)))))

(defun IB/close (buf)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (bound-and-true-p idle-buffer-mode)
        (when (buffer-modified-p buf)
          (save-buffer)) ; kolla returvärde? inte kill-buffer om save-buffer misslyckats?
        (message "idle-buffer-mode: Closing buffer %s after idle for %d sec" (buffer-name buf) idle-buffer-auto-close-sec)
        (kill-buffer)))))

(defun IB/save (buf)
  (IB/cancel-timer 'IB/save-timer)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and (bound-and-true-p idle-buffer-mode) (buffer-modified-p buf))
        (save-buffer)))))

(defun IB/cancel-timer (tsym)
  (let ((timer (symbol-value tsym)))
    (when timer
      (cancel-timer timer)
      (set tsym nil))))

(provide 'idle-buffer-mode)
