(defun term-config ()
  "For use in ans-term"
  (font-lock-mode 1)
  (linum-mode 0) ;; TODO: replace with `display-line-numbers-mode’
  (evil-mode nil))

(provide 'term-config)
