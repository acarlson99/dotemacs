(defun org-config ()
  "For use in 'org-mode-hook'."
  (hl-todo-mode t)
  (setq-local indent-tabs-mode nil))

(provide 'org-config)
