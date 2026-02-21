;;; restore-framegeometry.el --- Save/restore Emacs frame geometry  -*- lexical-binding: t; -*-
;;
;; Save and restore frame size (and position on X11).
;; On Wayland (pgtk), only size is saved — the protocol does not allow
;; clients to read or set window position.
;;
;; If fullscreen on exit, skip saving — keep the previous file.
;; Maximized is fine to save.

(defun my/framegeometry--wayland-p ()
  "Return non-nil if the current frame is running on Wayland (pgtk)."
  (eq (window-system) 'pgtk))

(defun save-framegeometry ()
  "Save frame geometry to ~/.emacs.d/framegeometry.
Skip saving if fullscreen — keep the previous file.
On Wayland, only width and height are saved."
  (when (not (memq (frame-parameter nil 'fullscreen)
                   '(fullscreen fullboth)))
    (let ((width  (frame-parameter (selected-frame) 'width))
          (height (frame-parameter (selected-frame) 'height))
          (top    (frame-parameter (selected-frame) 'top))
          (left   (frame-parameter (selected-frame) 'left))
          (wayland (my/framegeometry--wayland-p))
          (file   (expand-file-name "~/.emacs.d/framegeometry")))
      (unless (number-or-marker-p width)  (setq width 80))
      (unless (number-or-marker-p height) (setq height 40))
      (unless (number-or-marker-p top)    (setq top 0))
      (unless (number-or-marker-p left)   (setq left 0))
      (with-temp-buffer
        (insert
         ";;; This is the previous emacs frame's geometry.\n"
         ";;; Last generated " (current-time-string) ".\n"
         "(setq initial-frame-alist\n"
         "      (append '(\n"
         (format "        (width . %d)\n" (max width 0))
         (format "        (height . %d)" (max height 0))
         (if wayland
             "\n"
           (format "\n        (top . %d)\n        (left . %d)\n"
                   (max top 0) (max left 0)))
         "        ) initial-frame-alist))\n")
        (when (file-writable-p file)
          (write-file file))))))

(defun load-framegeometry ()
  "Load ~/.emacs.d/framegeometry to restore previous frame geometry."
  (let ((file (expand-file-name "~/.emacs.d/framegeometry")))
    (when (file-readable-p file)
      (load-file file))))

(provide 'restore-framegeometry)
;;; restore-framegeometry.el ends here
