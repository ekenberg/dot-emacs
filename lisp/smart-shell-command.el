(defun smart-shell-command-prompt (prompt-prefix)
  (first (list (read-from-minibuffer prompt-prefix nil nil nil 'shell-command-history) current-prefix-arg))
  )


(defun smart-shell-command (arg)
  "No region selected: shell-command arg, output to mini-buffer. Argument passed (C-u) and no region selected: shell-command arg, output at point. Region selected: shell-command-on-region, output to mini-buffer. Argument passed (C-u) and region selected: shell-command-on-region, output at point."
  (interactive "P")
  (let ((p (if mark-active (region-beginning) 0))
        (m (if mark-active (region-end) 0)))
    (if (= p m)
        ;; no active region
        (if (eq arg nil)
            (shell-command (smart-shell-command-prompt "Shell command: "))
          (shell-command (smart-shell-command-prompt "Shell command (insert): ") t))

      ;; active region
      (if (eq arg nil)
          (shell-command-on-region p m (smart-shell-command-prompt "Shell command (region): "))
        (shell-command-on-region p m (smart-shell-command-prompt "Shell command (region replace): ") t t)))))
