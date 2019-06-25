(defun go-updade-and-save ()
  "Run goreturns to update buffer, includes, etc and save result."
  (interactive)
  (save-excursion
	(if (and (file-exists-p "~/go/bin/goreturns") (file-executable-p "~/go/bin/goreturns"))
		(progn
		  (save-buffer)
		  (erase-buffer)
		  (if (shell-command (concat "~/go/bin/goreturns " buffer-file-name) 1)
			  (save-buffer) ;; success
			(error "ERROR: Save failed.  Panic"))) ;; failure
	  (if (y-or-n-p "~/go/bin/goreturns does not exist or does not have execute permissions.  Run 'go get -v github.com/sqs/goreturns' to install?")
		  (shell-command "go get -v github.com/sqs/goreturns")))))

(defun go-errcatch ()
  "Insert go error catch."
  (interactive)
  (insert "if err != nil {")
  (indent-for-tab-command)
  (insert "\npanic(1) // TODO: address error")
  (indent-for-tab-command)
  (insert "\n}")
  (indent-for-tab-command)
  (insert "\n"))

(defun my-go-config ()
  "For use in 'go-mode-hook'."
  (local-set-key (kbd "C-x C-a") 'go-updade-and-save)
  ;; (if (and (file-exists-p "~/go/bin/goreturns") (file-executable-p "~/go/bin/goreturns"))
  ;; 	  (local-set-key (kbd "C-x C-a") 'go-updade-and-save)
  ;; 	(local-set-key (kbd "C-x C-a") (lambda () (interactive) (message "~/go/bin/goreturns does not exist or does not have execute permissions.  Run 'go get -v github.com/sqs/goreturns' to unstall"))))
  ;; go error check
  (local-set-key (kbd "C-c C-e") 'go-errcatch)
  ;; whitespace cleanup
  (local-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; commenting
  (local-set-key (kbd "C-c C-c") 'comment-region)
  ;; uncommenting
  (local-set-key (kbd "C-c c") 'uncomment-region))
