(defun smart-shell-command-prompt (prompt-prefix)
  (car (list
          (read-from-minibuffer
           prompt-prefix nil nil nil 'shell-command-history)
          current-prefix-arg)))


(defun smart-shell-command (arg)
  "No region selected: shell-command arg, output to mini-buffer.
Region selected: shell-command-on-region, output to mini-buffer.
Argument passed (C-u): output at point.
Double argument passed (C-u C-u): copy output to kill ring"
  (interactive "P")
  (let ((p (if mark-active (region-beginning) 0))
        (m (if mark-active (region-end) 0)))
    (if (= p m)
        ;; no active region
        (if (eq arg nil)
            (shell-command (smart-shell-command-prompt "Shell command: "))
          (if (> (prefix-numeric-value arg) 4)
              ;; C-u C-u:
              (progn
                (shell-command (smart-shell-command-prompt "Shell command (copy output): "))
                (with-current-buffer shell-command-buffer-name
                  (clipboard-kill-ring-save (point-min) (point-max) )))
            ;; C-u:
            (shell-command (smart-shell-command-prompt "Shell command (insert): ") t)))

      ;; active region
      (if (eq arg nil)
          (shell-command-on-region p m (smart-shell-command-prompt "Shell command (region): "))
        (if (> (prefix-numeric-value arg) 4)
            ;; C-u C-u:
            (progn
              (shell-command-on-region p m (smart-shell-command-prompt "Shell command (region, copy output): "))
              (with-current-buffer shell-command-buffer-name
                (clipboard-kill-ring-save (point-min) (point-max))))
          ;; C-u:
          (shell-command-on-region p m (smart-shell-command-prompt "Shell command (region replace): ") t t))))))
