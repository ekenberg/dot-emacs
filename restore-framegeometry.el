; Funktioner för att spara/återställa x-emacs window (frame) size automatiskt
; Kommer från: http://ck.kennt-wayne.de/2011/jul/emacs-restore-last-frame-size-on-startup

(defun save-framegeometry ()
  "Gets the current frame's geometry and saves to ~/.emacs.d/framegeometry."
  (let (
        (framegeometry-left (frame-parameter (selected-frame) 'left))
        (framegeometry-top (frame-parameter (selected-frame) 'top))
        (framegeometry-width (frame-parameter (selected-frame) 'width))
        (framegeometry-height (frame-parameter (selected-frame) 'height))
        (framegeometry-file (expand-file-name "~/.emacs.d/framegeometry"))
        )

    (with-temp-buffer
      (insert
       ";;; This is the previous emacs frame's geometry.\n"
       ";;; Last generated " (current-time-string) ".\n"
       "(setq initial-frame-alist\n"
       "      '(\n"
       (format "        (top . %d)\n" (max framegeometry-top 0))
       (format "        (left . %d)\n" (max framegeometry-left 0))
       (format "        (width . %d)\n" (max framegeometry-width 0))
       (format "        (height . %d)))\n" (max framegeometry-height 0)))
      (when (file-writable-p framegeometry-file)
        (write-file framegeometry-file))))
  )

(defun load-framegeometry ()
  "Loads ~/.emacs.d/framegeometry which should load the previous frame's geometry."
  (let ((framegeometry-file (expand-file-name "~/.emacs.d/framegeometry")))
    (when (file-readable-p framegeometry-file)
      (load-file framegeometry-file)))
  )
