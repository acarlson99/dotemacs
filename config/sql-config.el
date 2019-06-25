(defun my-sql-config()
  "For use in 'c-mode-hook'."
  ;; whitespace cleanup
  (local-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; sql upcase
  (sql-upcase-mode 1))
