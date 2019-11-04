;; (defun go-run-goreturns ()
;;   "Run goreturns to update buffer, includes, etc and save result."
;;   (interactive)
;;   (save-excursion
;;	(if (executable-find "goreturns")
;;		(if (not (shell-command-on-region (point-min) (point-max) "goreturns" t t))
;;			(error "ERROR: goreturns failed.  Panic"))
;;	  (progn
;;		(if (not (and (file-exists-p "~/go/bin/goreturns") (file-executable-p "~/go/bin/goreturns")))
;;			(if (y-or-n-p "Executable 'goreturns' does not exist or is not executable.  Run 'go get -v github.com/sqs/goreturns' to install? ")
;;				(if (eq (shell-command "go get -v github.com/sqs/goreturns") 0)
;;					(message "Successfully go got")
;;				  (error "Command 'go get -v github.com/sqs/goreturns' failed"))))
;;		(if (y-or-n-p "Add ~/go/bin/goreturns to exec-path? ")
;;			(setq exec-path (append exec-path '("~/go/bin/goreturns"))))))))

;; Install these packages:
;; go get -u github.com/mdempsky/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/sqs/goreturns

(defun go-errcatch ()
  "Insert go error catch."
  (interactive)
  (insert "if err != nil {")
  (indent-for-tab-command)
  (insert "\npanic(err) // TODO: address error")
  (indent-for-tab-command)
  (insert "\n}")
  (indent-for-tab-command))

(if (not (getenv "GOPATH"))
	(setenv "GOPATH" (if on-nfs-p
						 "/tmp/go"
					   (concat
						(getenv "HOME") "/go"))))

(with-eval-after-load 'go-mode
  (require 'go-autocomplete nil 'noerror))

(setq gofmt-command "goreturns")

(defun go-config ()
  "For use in 'go-mode-hook'."
  ;; (local-set-key (kbd "C-c f") 'go-run-goreturns)
  ;; These do the same thing, but the latter is probably better than my hack
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; go error check
  (local-set-key (kbd "C-c C-e") 'go-errcatch)
  ;; whitespace cleanup
  (local-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; Godef jump key binding
  ;; C-t pops tag
  (local-set-key [(f5)] 'godef-jump))

(provide 'go-config)
