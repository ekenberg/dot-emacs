
;; buffer-local variables:
(defvar-local idle-buffer-auto-save-sec     0   "How many seconds idle buffer before saving, 0 means disabled")
(defvar-local idle-buffer-auto-close-sec    0   "How many seconds idle buffer before closing, 0 means disabled")
(defvar-local idle-buffer--save-idle-timer  nil "For internal use only")
(defvar-local idle-buffer--close-idle-timer nil "For internal use only")
(defvar-local idle-buffer--save-timer       nil "For internal use only")
(defvar-local idle-buffer--close-timer      nil "For internal use only")
(defvar-local idle-buffer--poll-timer       nil "For internal use only")

(defconst idle-buffer-poll-sec 1 "Poll interval for checking buffer focus and timers")

(define-minor-mode idle-buffer-mode
  "Perform automatic actions on idle buffers"
  :lighter "ib-mode"

  ;; check file-local value of each relevant variable and start timers if applicable
  (if idle-buffer-mode
      ;; mode toggled ON
      (progn

        (when (> idle-buffer-auto-save-sec 0)
          (message "idle-buffer-mode: %s activating idle auto save after %d sec" (buffer-name) idle-buffer-auto-save-sec)
          (idle-buffer--start-save-idle-timer (current-buffer) idle-buffer-auto-save-sec))

        (when (> idle-buffer-auto-close-sec 0)
          (message "idle-buffer-mode: %s activating idle auto close after %d sec" (buffer-name) idle-buffer-auto-close-sec)
          (idle-buffer--start-close-idle-timer (current-buffer) idle-buffer-auto-close-sec))

        (when (or (> idle-buffer-auto-save-sec 0)
                  (> idle-buffer-auto-close-sec 0))
          (message "idle-buffer-mode: starting poll-timer with interval %d sec" idle-buffer-poll-sec)
          (idle-buffer--start-poll-timer (current-buffer) idle-buffer-poll-sec))

        (add-hook 'kill-buffer-hook #'idle-buffer--kill-buffer-handler)

        )

    ;; mode toggled OFF
    (idle-buffer--cancel-all-timers)
    (remove-hook 'kill-buffer-hook #'idle-buffer--kill-buffer-handler)
    ))

(defun idle-buffer--kill-buffer-handler ()
  (idle-buffer--cancel-all-timers))

(defun idle-buffer--cancel-all-timers ()
  (idle-buffer--cancel-poll-timer)
  (idle-buffer--cancel-save-idle-timer)
  (idle-buffer--cancel-close-idle-timer)
  (idle-buffer--cancel-save-timer)
  (idle-buffer--cancel-close-timer)
  )

(defun idle-buffer--start-poll-timer (my-buffer interv)
  (setq idle-buffer--poll-timer (run-with-timer interv interv 'idle-buffer--run-poll my-buffer)))

(defun idle-buffer--start-save-timer (buf sec)
  (with-current-buffer buf
    (when (and (null idle-buffer--save-timer) (boundp 'sec) (integerp sec) (> sec 0))
      (setq idle-buffer--save-timer (run-with-timer sec sec #'idle-buffer--save buf)))))

(defun idle-buffer--start-close-timer (buf sec)
  (with-current-buffer buf
    (when (and (null idle-buffer--close-timer) (boundp 'sec) (integerp sec) (> sec 0))
      (setq idle-buffer--close-timer (run-with-timer sec nil #'idle-buffer--close buf)))))

(defun idle-buffer--start-save-idle-timer (buf sec)
  (when (and (null idle-buffer--save-idle-timer) (boundp 'sec) (integerp sec) (> sec 0))
    (setq idle-buffer--save-idle-timer (run-with-idle-timer sec t #'idle-buffer--save buf))))

(defun idle-buffer--start-close-idle-timer (buf sec)
  (when (and (null idle-buffer--close-idle-timer) (boundp 'sec) (integerp sec) (> sec 0))
    (setq idle-buffer--close-idle-timer (run-with-idle-timer sec nil #'idle-buffer--close buf)))) ; no repeat for close timer

(defun idle-buffer--run-poll (my-buffer)
  (if (eq my-buffer (current-buffer))
      (progn
        (idle-buffer--cancel-save-timer)
        (idle-buffer--cancel-close-timer))
    ;; else
    (idle-buffer--start-save-timer my-buffer (buffer-local-value 'idle-buffer-auto-save-sec my-buffer))
    (idle-buffer--start-close-timer my-buffer (buffer-local-value 'idle-buffer-auto-close-sec my-buffer))))

(defun idle-buffer--close (buf)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (bound-and-true-p idle-buffer-mode)
        (when (buffer-modified-p buf)
          (save-buffer)) ; kolla returvärde? inte kill-buffer om save-buffer misslyckats?
        (message "closing buffer %s after being idle for %d sec" (buffer-name buf) idle-buffer-auto-close-sec)
        (kill-buffer)))))

(defun idle-buffer--save (buf)
  (idle-buffer--cancel-save-timer)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and (bound-and-true-p idle-buffer-mode) (buffer-modified-p buf))
        (save-buffer)))))

(defun idle-buffer--cancel-poll-timer()
  (when idle-buffer--poll-timer
    (cancel-timer idle-buffer--poll-timer)
    (setq idle-buffer--poll-timer nil)))

(defun idle-buffer--cancel-save-idle-timer()
  (when idle-buffer--save-idle-timer
    (cancel-timer idle-buffer--save-idle-timer)
    (setq idle-buffer--save-idle-timer nil)))

(defun idle-buffer--cancel-close-idle-timer()
  (when idle-buffer--close-idle-timer
    (cancel-timer idle-buffer--close-idle-timer)
    (setq idle-buffer--close-idle-timer nil)))

(defun idle-buffer--cancel-save-timer()
  (when idle-buffer--save-timer
    (cancel-timer idle-buffer--save-timer)
    (setq idle-buffer--save-timer nil)))

(defun idle-buffer--cancel-close-timer()
  (when idle-buffer--close-timer
    (cancel-timer idle-buffer--close-timer)
    (setq idle-buffer--close-timer nil)))


(provide 'idle-buffer-mode)
