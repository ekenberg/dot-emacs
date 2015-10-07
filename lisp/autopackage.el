(defun check-install-packages (package-list)
  "Check if any package in package-list needs installing. Prompt user before installation"
  (interactive)
  (let ((need-install 0))
    (dolist (p package-list)
      (when (not (package-installed-p p))
        (setq need-install 1)))

    (if (> need-install 0)
        (if (y-or-n-p (format "Additional packages required - download and install now?"))

	    (progn
	      (when (not package-archive-contents)
		(package-refresh-contents))
	      (dolist (p package-list)
		(when (not (package-installed-p p))
		  (package-install p)))
	      t)
	  nil)
      t)))
