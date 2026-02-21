;;; restore-framegeometry.el --- Save/restore Emacs frame geometry  -*- lexical-binding: t; -*-
;;
;; Save and restore frame size (and position on X11).
;; On Wayland (pgtk), only size is saved — the protocol does not allow
;; clients to read or set window position.
;;
;; If fullscreen on exit, transition to maximized and wait for the
;; compositor to resize the frame before saving.

(defun my/framegeometry--wayland-p ()
  "Return non-nil if the current frame is running on Wayland (pgtk)."
  (eq (window-system) 'pgtk))

(defun save-framegeometry ()
  "Save frame geometry to ~/.emacs.d/framegeometry.
If fullscreen, transition to maximized first and wait for resize.
On Wayland, only width and height are saved."
  ;; Capture fullscreen/maximized state before any transition.
  (let* ((fs (frame-parameter nil 'fullscreen))
         (maximized (memq fs '(fullscreen fullboth maximized))))
    ;; If fullscreen, switch to maximized and give the compositor time
    ;; to actually resize the frame before we read the parameters.
    (when (memq fs '(fullscreen fullboth))
      (set-frame-parameter nil 'fullscreen 'maximized)
      (sit-for 0.5))
    (let ((width  (frame-text-width (selected-frame)))
          (height (frame-text-height (selected-frame)))
          (top    (frame-parameter (selected-frame) 'top))
          (left   (frame-parameter (selected-frame) 'left))
          (wayland (my/framegeometry--wayland-p))
          (file   (expand-file-name "~/.emacs.d/framegeometry")))
      (unless (number-or-marker-p width)  (setq width 800))
      (unless (number-or-marker-p height) (setq height 600))
      (unless (number-or-marker-p top)    (setq top 0))
      (unless (number-or-marker-p left)   (setq left 0))
      (with-temp-buffer
        (insert
         ";;; This is the previous emacs frame's geometry.\n"
         ";;; Last generated " (current-time-string) ".\n"
         "(setq default-frame-alist\n"
         "      (append '(\n"
         ;; When maximized, save 80% of maximized size as the windowed
         ;; restore target — otherwise the compositor has no sensible
         ;; size to unmaximize to.
         (let ((w (max width 0))
               (h (max height 0)))
           (when maximized
             (setq w (round (* w 0.8))
                   h (round (* h 0.8))))
           (concat
            (format "        (width . (text-pixels . %d))\n" w)
            (format "        (height . (text-pixels . %d))\n" h)))
         (if wayland
             ""
           (format "        (top . %d)\n        (left . %d)\n"
                   (max top 0) (max left 0)))
         "        ) default-frame-alist))\n"
         ;; Maximize after the frame exists so the compositor registers
         ;; the base size as the unmaximize restore target.
         (if maximized
             "(add-hook 'window-setup-hook #'toggle-frame-maximized)\n"
           ""))
        (when (file-writable-p file)
          (write-file file))))))

(defun load-framegeometry ()
  "Load ~/.emacs.d/framegeometry to restore previous frame geometry."
  (let ((file (expand-file-name "~/.emacs.d/framegeometry")))
    (when (file-readable-p file)
      (load-file file))))

(provide 'restore-framegeometry)
;;; restore-framegeometry.el ends here
