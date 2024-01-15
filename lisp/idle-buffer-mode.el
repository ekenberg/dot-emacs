;; The following variables are bound to ALWAYS be local to the current buffer (se docs)
;;(make-variable-buffer-local 'idle-buffer-active-timers)  ; to cancel or restart all when needed
(defvar-local idle-buffer-auto-save-sec    nil "How many seconds idle buffer before saving")
(defvar-local idle-buffer-auto-close-sec   nil "How many seconds idle buffer before closing")
(defvar-local idle-buffer--save-idle-timer nil "For internal use only")
(defvar-local idle-buffer--save-timer      nil "For internal use only")
(defvar-local idle-buffer--close-timer     nil "For internal use only")
(defvar-local idle-buffer--my-buffer       nil "For internal use only")

(define-minor-mode idle-buffer-mode
  "Perform automatic actions on idle buffers"
  :lighter "ib-mode"

  ;; check file-local value of each relevant variable and start timers if applicable
  (if idle-buffer-mode
      ;; mode toggled ON
      (progn
        (setq idle-buffer--my-buffer (current-buffer))

        (when (> idle-buffer-auto-save-sec 0)
          (message "idle-buffer-mode: %s activating idle auto save after %d sec" (buffer-name) idle-buffer-auto-save-sec)
          (idle-buffer--start-save-idle-timer (current-buffer) idle-buffer-auto-save-sec))

        (when (> idle-buffer-auto-close-sec 0)
          (message "idle-buffer-mode: %s activating idle auto close after %d sec" (buffer-name) idle-buffer-auto-close-sec)
          (idle-buffer--start-close-idle-timer (current-buffer) idle-buffer-auto-close-sec))

        (add-hook 'kill-buffer-hook #'idle-buffer--kill-buffer-handler)
        (add-hook 'buffer-list-update-hook #'idle-buffer--buffer-list-update-handler))

    ;; mode toggled OFF
    (remove-hook 'kill-buffer-hook #'idle-buffer--kill-buffer-handler)
    (remove-hook 'buffer-list-update-hook #'idle-buffer--buffer-list-update-handler)))

;; Ok problemet är att när man triggar buffer-list-update-hook så vet man inte
;; vilken buffer man lämnar och vilken man anländer till
;; Beroende på vilken funktion som triggar hooken så får man från-buffern eller till-buffern i (current-buffer)
;; Men om man väntar en sekund (run-with-timer) så får man konsekvent till-buffern i (current-buffer)
;; Problemet är då att om man lämnat "vår" buffer eller växlar mellan två andra buffers, så
;; är inte de buffer-lokalt definierade variablerna tillgängliga för att sätta timers (när man lämnar "vår" buffer)
;;
;; Skit i lambdas eller konstiga defalias-konstruktioner för nu
;;
;; Vad om vi när hooken körs utan argument direkt kollar (current-buffer) samt idle-buffer--my-buffer
;; sen skickar vi med dem båda i run-with-timer
;; och när vi kör igen så kan vi hämta nya (current-buffer)
;; Nu vet vi
;;   - vilken buffer som var aktiv när hooken anropades
;;   - vilken vi slutligen landade i
;;   - om idle-buffer--my-buffer var satt från början
;;   - om idle-buffer--my-buffer är satt när vi landat
;;
;; med hjälp av dessa värden kan vi (väl) lista ut läget?
;; Exempel:
;; (defun handler (&optional hook-buffer hook-my-buffer)
;;   (when (and hook-buffer hook-my-buffer)
;;     (let ((final-buffer (current-buffer))
;;           (final-my-buffer (and (boundp idle-buffer--my-buffer) idle-buffer--my-buffer)))
;;       ;; case växlar mellan två andra buffers
;;       ;; case växlar från "vår" buffer till annan
;;       ;; case växlar från annan buffer till "vår"
;;       ;; case växlar mellan "vår" buffer och en annan som också har idle-buffer-mode

;;       ;; kan man räkna med att om hook-buffer == final-buffer så är vi i TILL-buffern och vi vet inte FRÅN-buffern
;;       ;;    det betyder i så fall att växling mellan två idle-buffer-mode
;;       ;; om de inte är samma

;;       )
;;     ))


(defun idle-buffer--buffer-list-update-handler (&optional really-run)
  (message "id dbg in update handler (%s)" really-run)
  (if really-run
      (progn
        ;; back in our buffer, cancel timers
        (if (eq idle-buffer--my-buffer (current-buffer))
            (progn
              (message "id dbg BACK IN OUR BUFFER")
              (idle-buffer--cancel-save-timer)
              (idle-buffer--cancel-close-timer))
          ;; leaving our buffer, start timers as needed
          (message "id dbg OUTSIDE OUR BUFFER")
          (when (and (> idle-buffer-auto-save-sec 0)
                     (not idle-buffer--save-timer))
            (setq idle-buffer--save-timer (run-with-timer idle-buffer-auto-save-sec nil 'idle-buffer--save idle-buffer--my-buffer)))
          (when (and (> idle-buffer-auto-close-sec 0)
                     (not idle-buffer--close-timer))
            (setq idle-buffer--close-timer (run-with-timer idle-buffer-auto-close-sec nil 'idle-buffer--close idle-buffer--my-buffer)))))
    ;; else run this function after one second wait, to get correct current buffer
    (run-with-timer 1 nil 'idle-buffer--buffer-list-update-handler t)))

(defun idle-buffer--kill-buffer-handler ()
  (idle-buffer--cancel-all-timers))

(defun idle-buffer--cancel-all-timers ()
  (idle-buffer--cancel-save-idle-timer)
  (idle-buffer--cancel-save-timer)
  (idle-buffer--cancel-close-timer))

(defun idle-buffer--start-close-idle-timer (buf sec)
  (when (and (boundp 'sec) (integerp sec) (> sec 0))
    (run-with-idle-timer sec nil 'idle-buffer--close buf))) ; no repeat for close timer

(defun idle-buffer--start-save-idle-timer (buf sec)
  (when (and (boundp 'sec) (integerp sec) (> sec 0))
    (setq idle-buffer--save-idle-timer (run-with-idle-timer sec t 'idle-buffer--save buf))))

(defun idle-buffer--close (buf)
  (idle-buffer--cancel-close-timer)
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

(defun idle-buffer--cancel-save-idle-timer()
  (when idle-buffer--save-idle-timer
    (cancel-timer idle-buffer--save-idle-timer)
    (setq idle-buffer--save-idle-timer nil)))

(defun idle-buffer--cancel-save-timer()
  (when idle-buffer--save-timer
    (cancel-timer idle-buffer--save-timer)
    (setq idle-buffer--save-timer nil)))

(defun idle-buffer--cancel-close-timer()
  (when idle-buffer--close-timer
    (cancel-timer idle-buffer--close-timer)
    (setq idle-buffer--close-timer nil)))

;; TODO
;;  - starta vanlig timer när fokus lämnar buffern, och cancel när fokus kommer tillbaka
;;    även om man är aktiv i andra buffers skall det räknas inaktivitet i lokal buffer
;;    timers skall utföra samma handlingar som de idle-timers som startas
;;  - försök återskapa 100% cpu med annat än org-mode buffers
;;  - använd fil-lokal variabel för att aktivera idle-buffer-mode
;;
;; BUGS
;; [2024-01-15 01:51:57.493 CET] closing buffer freewriting.org.gpg after being idle for 10 sec
;; [2024-01-15 01:51:57.494 CET] Error running timer ‘idle-buffer--close’: (wrong-type-argument timerp nil)

(provide 'idle-buffer-mode)
