;;; early-init.el --- Pre-frame-creation settings  -*- lexical-binding: t; -*-

;; Disable character-cell resize increments in GTK geometry hints.
;; Without this, Emacs pgtk + fractional scaling sends inflated size
;; hints to the compositor, causing bad window placement.
(setq frame-resize-pixelwise t)

;; Disable UI chrome before the frame is created to avoid flicker.
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)

;; Set font before frame creation so set-frame-font doesn't resize
;; the frame later (which causes progressive height shrinking).
(push '(font . "Bitstream Vera Sans Mono-10") default-frame-alist)

;; Load saved frame geometry into default-frame-alist BEFORE the
;; initial frame is created.
(let ((geom-file (expand-file-name "framegeometry" user-emacs-directory)))
  (when (file-readable-p geom-file)
    (load geom-file)))

;;; early-init.el ends here
