(defun php-config ()
  "For use in 'php-mode-hook'."
  ;; good indentation fight me
  (setq indent-tabs-mode 1)
  ;; uncommenting
  (local-set-key (kbd "C-c c") 'uncomment-region))

(provide 'php-config)
