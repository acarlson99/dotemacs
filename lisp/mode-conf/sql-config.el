(require 'sql-upcase)

(defun sql-config ()
  "For use in `sql-mode-hook'."
  ;; whitespace cleanup
  (local-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; sql upcase
  (sql-upcase-mode 1))

(provide 'sql-config)
