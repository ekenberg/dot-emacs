(defun active-minor-modes (other-window)
  "List of minor-modes active in other-window"
  (seq-filter
   (lambda (m)
     (and (boundp m) (symbol-value m)))
   (with-current-buffer other-window minor-mode-list)))

(defun popup-minor-modes ()
  "List of minor modes considered to be popups"
  ;; Ironiskt nog när denna utökning väl var klar hade jag glömt
  ;; vilken minor-mode i vilken popup som orsakade problemet... :D
  ;; Lägg till i listan under när det dyker upp igen:
  '())

(defun minor-popup-p (active-mms)
  "Is any of the minor-modes in active-mms in our list of popup-minor-modes?"
  (if (> (length active-mms) 0)
      (if (member (car active-mms) (popup-minor-modes))
          t
        (minor-popup-p (cdr active-mms)))
    nil))

(defun popup-major-modes ()
  "List of major modes considered to be popups"
  '(help-mode
    Info-mode
    Man-mode
    debugger-mode
    compilation-mode
    grep-mode
    Buffer-menu-mode
    apropos-mode
    ivy-occur-mode
    ivy-occur-grep-mode
    cargo-process-mode
    cider-stacktrace-mode))

(defun major-popup-p (other-window)
  "Is major-mode of other-window in our list of popup-major-modes?"
  (member (with-current-buffer other-window major-mode)
          (popup-major-modes)))

(defun quit-popup-window ()
  "Quit the popup (help/debug etc) in other window (if there are 2 windows)"
  (interactive)
  (let* ((other-window (window-buffer (next-window)))
         (minor-modes (active-minor-modes other-window)))
    (if (and
         (eq (count-windows) 2)
         (or
          (major-popup-p other-window)
          (minor-popup-p (active-minor-modes other-window)))
         (quit-restore-window (next-window)))

        (message "Cannot find a popup-window to quit"))))

(provide 'quit-popup-window)
