(defun web-config ()
  "For use in 'web-mode-hook'."
  ;; commenting
  (local-set-key (kbd "C-c C-c") 'comment-region)
  ;; uncommenting
  (local-set-key (kbd "C-c c") 'uncomment-region)
  (setq web-mode-enable-current-element-highlight t))

(provide 'web-config)
