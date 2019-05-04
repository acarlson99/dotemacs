(defun go-updade-and-save ()
  "Run goreturns to update buffer, includes, etc and save result."
  (interactive)
  (if (and (file-exists-p "~/go/bin/goreturns") (file-executable-p "~/go/bin/goreturns"))
	  (progn
		(save-buffer)
		(erase-buffer)
		(if (shell-command (concat "~/go/bin/goreturns " buffer-file-name) 1)
			(save-buffer) ;; success
		  (error "ERROR: Save failed.  Panic"))) ;; failure
	(error "~/go/bin/goreturns does not exist or does not have execute permissions.  Run 'go get -v github.com/sqs/goreturns' to install")))

(defun my-go-config ()
  "For use in 'go-mode-hook'."
  (local-set-key (kbd "C-x C-a") 'go-updade-and-save)
  ;; (if (and (file-exists-p "~/go/bin/goreturns") (file-executable-p "~/go/bin/goreturns"))
  ;; 	  (local-set-key (kbd "C-x C-a") 'go-updade-and-save)
  ;; 	(local-set-key (kbd "C-x C-a") (lambda () (interactive) (message "~/go/bin/goreturns does not exist or does not have execute permissions.  Run 'go get -v github.com/sqs/goreturns' to unstall"))))
  ;; whitespace cleanup
  (local-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; commenting
  (local-set-key (kbd "C-c C-c") 'comment-region)
  ;; uncommenting
  (local-set-key (kbd "C-c c") 'uncomment-region))
