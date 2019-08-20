(defun go-run-goreturns ()
  "Run goreturns to update buffer, includes, etc and save result."
  (interactive)
  (save-excursion
	(if (executable-find "goreturns")
		  (if (not (shell-command-on-region (point-min) (point-max) "goreturns" t t))
			  (error "ERROR: goreturns failed.  Panic"))
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

(defun go-config ()
  "For use in 'go-mode-hook'."
  (local-set-key (kbd "C-c f") 'go-run-goreturns)
  ;; (if (and (file-exists-p "~/go/bin/goreturns") (file-executable-p "~/go/bin/goreturns"))
  ;; 	  (local-set-key (kbd "C-x C-a") 'go-run-goreturns)
  ;; 	(local-set-key (kbd "C-x C-a") (lambda () (interactive) (message "~/go/bin/goreturns does not exist or does not have execute permissions.  Run 'go get -v github.com/sqs/goreturns' to unstall"))))
  ;; go error check
  (local-set-key (kbd "C-c C-e") 'go-errcatch)
  ;; whitespace cleanup
  (local-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; commenting
  (local-set-key (kbd "C-c C-c") 'comment-region)
  ;; uncommenting
  (local-set-key (kbd "C-c c") 'uncomment-region))

(provide 'go-config)
