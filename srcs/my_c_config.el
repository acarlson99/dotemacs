(defun my_c_config()
  "For use in 'c-mode-hook'."
  ;; keyboard macros
  (global-set-key (kbd "<backtab>") 'kmacro-start-macro-or-insert-counter)
  (global-set-key (kbd "<C-tab>") 'kmacro-end-or-call-macro)
  ;; moving text
  (global-set-key (kbd "C-x C-p") 'move-text-up)
  (global-set-key (kbd "C-x C-n") 'move-text-down)
  ;; whitespace cleanup
  (global-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; uncommenting
  (global-set-key (kbd "C-c c") 'uncomment-region)
  ;; sick modes
  (abbrev-mode)
  (font-lock-mode)
  (hl-todo-mode)
  (show-paren-mode)
  )
