(defun ruby-config ()
  "For use in `ruby-mode-hook'."
  (if (fboundp 'ruby-end-mode)
	  (ruby-end-mode 1))
  (if (fboundp 'ruby-extra-highlight-mode)
	  (ruby-extra-highlight-mode 1)))

(provide 'ruby-config)
