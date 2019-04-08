(defun go-updade-and-save ()
  "Run goreturns to update buffer, includes, etc and save result."
  (interactive)
  (save-buffer)
  (point-to-register 1)
  (erase-buffer)
  (if (shell-command (concat "~/go/bin/goreturns " buffer-file-name) 1)
	  (progn ;; success
		(jump-to-register 1)
		(save-buffer)
		)
	(error "ERROR: Save failed.  Panic")) ;; failure
  )

(defun my-go-config ()
  "For use in 'go-mode-hook'."
  (if (and (file-exists-p "~/go/bin/goreturns") (file-executable-p "~/go/bin/goreturns"))
	  (local-set-key (kbd "C-x C-a") 'go-updade-and-save)
	(local-set-key (kbd "C-x C-a") (lambda () (interactive) (message "~/go/bin/goreturns does not exist or does not have execute permissions"))))
  ;; whitespace cleanup
  (local-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; commenting
  (local-set-key (kbd "C-c C-c") 'comment-region)
  ;; uncommenting
  (local-set-key (kbd "C-c c") 'uncomment-region))
