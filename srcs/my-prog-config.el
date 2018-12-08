(defun my-prog-config()
  "For use in 'prog-mode-hook'."
  ;; moving text
  (global-set-key (kbd "C-x C-p") 'move-text-up)
  (global-set-key (kbd "C-x C-n") 'move-text-down)

  ;; hoghlight current line
  (global-hl-line-mode 1)
  ;; (set-face-background 'hl-line nil)
  (set-face-foreground 'hl-line nil)
  ;; (set-face-underline  'hl-line t)

  ;; sick modes
  (abbrev-mode t)
  (font-lock-mode t)
  (hl-todo-mode t)
  (hl-line-mode t)
  (show-paren-mode t)
  )
