;; Let the next line stay commented, else package.el will just add it again
;;(package-initialize)


;;
;; THIS IS THE REAL CONFIGURATION:
;;
(org-babel-load-file "~/.emacs.d/configuration.org")



;; CUSTOM AUTOMATIC SETTINGS
;; DO NOT TOUCH
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-safe-themes
   (quote
    ("98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(hl-sexp-background-color "#1c1f26")
 '(ns-right-alternate-modifier (quote meta))
 '(package-selected-packages
   (quote
    (solarized-theme ox-twbs org-babel-eval-in-repl org-bullets counsel swiper try smex ivy neotree material-theme sws-mode swift-mode smart-tab slime scss-mode rainbow-mode rainbow-delimiters php-mode php-eldoc pandoc-mode markdown-mode+ magit-find-file json-mode jedi jade-mode helm graphene goto-last-change feature-mode f))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block-begin-line ((t (:background nil :foreground "#b3e5fc" :box nil))) t)
 '(org-block-end-line ((t (:background nil :foreground "#b3e5fc" :box nil))) t)
 '(org-level-1 ((t (:inherit outline-1 :foreground "DarkOrange2" :background nil :box nil :weight bold :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "RoyalBlue1" :background nil :box nil :height 1.2)))))
