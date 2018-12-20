(defun my-prog-config()
  "For use in 'prog-mode-hook'."
  ;; moving text
  (global-set-key (kbd "C-x C-p") 'move-text-up)
  (global-set-key (kbd "C-x C-n") 'move-text-down)

  ;; sick modes
  (abbrev-mode t)
  (font-lock-mode t)
  (hl-todo-mode t)
  (hl-line-mode t)
  (show-paren-mode t)
  (fci-mode))
