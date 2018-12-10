(defun my-c-config()
  "For use in 'c-mode-hook'."
  ;; keyboard macros
  (global-set-key (kbd "<backtab>") 'kmacro-start-macro-or-insert-counter)
  (global-set-key (kbd "<C-tab>") 'kmacro-end-or-call-macro)
  ;; whitespace cleanup
  (global-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; uncommenting
  (global-set-key (kbd "C-c c") 'uncomment-region))
