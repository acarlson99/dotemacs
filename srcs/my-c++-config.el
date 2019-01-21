(defun my-c++-config()
  "For use in 'c++-mode-hook'."
  ;; whitespace cleanup
  (global-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; uncommenting
  (global-set-key (kbd "C-c c") 'uncomment-region))
