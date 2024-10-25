;; Buffer-local variables for state
(defvar-local binary-move-left-stop nil
  "Left boundary of current binary movement sequence.")

(defvar-local binary-move-right-stop nil
  "Right boundary of current binary movement sequence.")

(defvar-local binary-move-upper-stop nil
  "Upper boundary line number of current binary movement sequence.")

(defvar-local binary-move-lower-stop nil
  "Lower boundary line number of current binary movement sequence.")

;; Helper functions
(defun binary-move--midpoint (start end)
  "Calculate the midpoint between START and END positions."
  (/ (+ start end) 2))

(defun binary-move--current-line ()
  "Get current line number relative to window-start."
  (count-screen-lines (window-start) (point)))

(defun binary-move--visible-lines ()
  "Get number of visible lines in current window."
  (count-screen-lines (window-start) (window-end)))

;; Main movement functions
(defun binary-move-forward ()
  "Move point forward using binary search movement."
  (interactive)
  (unless (or (eq last-command 'binary-move-forward)
              (eq last-command 'binary-move-backward))
    (setq binary-move-left-stop (point)
          binary-move-right-stop (line-end-position)))
  (setq binary-move-left-stop (point))
  (goto-char (binary-move--midpoint binary-move-left-stop binary-move-right-stop)))

(defun binary-move-backward ()
  "Move point backward using binary search movement."
  (interactive)
  (unless (or (eq last-command 'binary-move-forward)
              (eq last-command 'binary-move-backward))
    (setq binary-move-left-stop (line-beginning-position)
          binary-move-right-stop (point)))
  (setq binary-move-right-stop (point))
  (goto-char (binary-move--midpoint binary-move-left-stop binary-move-right-stop)))

(defun binary-move-down ()
  "Move down lines using binary search movement within visible window."
  (interactive)
  (unless (or (eq last-command 'binary-move-down)
              (eq last-command 'binary-move-up))
    (setq binary-move-upper-stop (binary-move--current-line)
          binary-move-lower-stop (binary-move--visible-lines)))
  (setq binary-move-upper-stop (binary-move--current-line))
  (let ((target-line (binary-move--midpoint binary-move-upper-stop binary-move-lower-stop)))
    (line-move-visual (- target-line (binary-move--current-line)))))

(defun binary-move-up ()
  "Move up lines using binary search movement within visible window."
  (interactive)
  (unless (or (eq last-command 'binary-move-down)
              (eq last-command 'binary-move-up))
    (setq binary-move-upper-stop 0
          binary-move-lower-stop (binary-move--current-line)))
  (setq binary-move-lower-stop (binary-move--current-line))
  (let ((target-line (binary-move--midpoint binary-move-upper-stop binary-move-lower-stop)))
    (line-move-visual (- target-line (binary-move--current-line)))))
