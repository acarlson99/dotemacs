(defun my-web-config()
  "For use in 'web-mode-hook'."
  (web-mode-toggle-current-element-highlight)	;; TODO: this is bad.  Fix so it just sets it rather than toggle
  )
