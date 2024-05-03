(require 'my-defs)

(defun prog-config ()
  "For use in `prog-mode-hook'."
  ;; moving text
  (local-set-key (kbd "C-x C-p") 'move-text-up)
  (local-set-key (kbd "C-x C-n") 'move-text-down)
  (local-set-key (kbd "C-c C-w") 'copy-region-as-kill)

  ;; Set fill column indicator defaults
  (setq fci-rule-column 80)
  (setq fci-rule-width 1)

  ;; sick modes
  (abbrev-mode t)
  (font-lock-mode t)
  (hl-todo-mode t)
  (hl-line-mode 1)
  (set-face-foreground 'hl-line nil)
  (show-paren-mode t)
  (fci-mode))

(provide 'prog-config)
