(defun go-update-and-save ()
  "Run goreturns to update buffer, includes, etc and save result."
  (interactive)
  (save-excursion
	(if (executable-find "goreturns")
		(progn
		  (save-buffer)
		  (erase-buffer)
		  (if (shell-command (concat "goreturns " buffer-file-name) 1)
			  (message "Success") ;; success
			(error "ERROR: goreturns failed.  Panic"))) ;; failure
	  (progn
		(if (not (and (file-exists-p "~/go/bin/goreturns") (file-executable-p "~/go/bin/goreturns")))
			(if (y-or-n-p "Executable 'goreturns' does not exist or is not executable.  Run 'go get -v github.com/sqs/goreturns' to install? ")
				(if (eq (shell-command "go get -v github.com/sqs/goreturns") 0)
					(message "Successfully go got")
				  (error "Command 'go get -v github.com/sqs/goreturns' failed"))))
		(if (y-or-n-p "Add ~/go/bin/goreturns to exec-path? ")
			(setq exec-path (append exec-path '("~/go/bin/goreturns"))))))))

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
  (local-set-key (kbd "C-x C-a") 'go-update-and-save)
  ;; (if (and (file-exists-p "~/go/bin/goreturns") (file-executable-p "~/go/bin/goreturns"))
  ;; 	  (local-set-key (kbd "C-x C-a") 'go-update-and-save)
  ;; 	(local-set-key (kbd "C-x C-a") (lambda () (interactive) (message "~/go/bin/goreturns does not exist or does not have execute permissions.  Run 'go get -v github.com/sqs/goreturns' to unstall"))))
  ;; go error check
  (local-set-key (kbd "C-c C-e") 'go-errcatch)
  ;; whitespace cleanup
  (local-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; commenting
  (local-set-key (kbd "C-c C-c") 'comment-region)
  ;; uncommenting
  (local-set-key (kbd "C-c c") 'uncomment-region))
