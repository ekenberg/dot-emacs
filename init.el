;; Let the next line stay commented, else package.el will just add it again
;;(package-initialize)

;;
;; THIS IS THE REAL CONFIGURATION:
;;
(org-babel-load-file "~/.emacs.d/configuration.org")

;; Keep automatic custom settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
(put 'list-timers 'disabled nil)
