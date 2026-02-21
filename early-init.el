;;; early-init.el --- Pre-frame-creation settings  -*- lexical-binding: t; -*-

;; Disable character-cell resize increments in GTK geometry hints.
;; Without this, Emacs pgtk + fractional scaling sends inflated size
;; hints to the compositor, causing bad window placement.
(setq frame-resize-pixelwise t)

;; Load saved frame geometry into initial-frame-alist BEFORE the
;; initial frame is created — the only time initial-frame-alist
;; actually takes effect.
(let ((geom-file (expand-file-name "framegeometry" user-emacs-directory)))
  (when (file-readable-p geom-file)
    (load geom-file)))

;;; early-init.el ends here
