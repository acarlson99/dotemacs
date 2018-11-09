(defun my_c_config()
  "For use in 'c-mode-hook'."
  (global-set-key (kbd "<backtab>") 'kmacro-start-macro-or-insert-counter)
  (global-set-key (kbd "<C-tab>") 'kmacro-end-or-call-macro)
  )
