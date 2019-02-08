;; Norminette-compliant block commenting
(defun comment-norminette ()
  "Redefinition of block commenting to appease Norminette"
  (interactive)
  (if (use-region-p)
	  (progn
		(setq s (line-number-at-pos))
		(setq rs (region-beginning))
		(setq re (region-end))
		(exchange-point-and-mark)
		(setq e (line-number-at-pos))
		(if (> s e) (setq s  (prog1 e (setq e  s))))
		(goto-line s)
		(beginning-of-line)
		(insert "/*\n")
		(setq i s)
		(while (< i e)
		  (progn
			(beginning-of-line)
			(insert "** ")
			(setq i (+ i 1))
			(next-line)
			)
		  )
		(beginning-of-line)
		(insert "*/\n")
		)
	(progn
	  (beginning-of-line)
	  (insert "/*\n**")
	  (end-of-line)
	  (insert "\n*/")
	  )
	)
  )

(defun my-c-config()
  "For use in 'c-mode-hook'."
  ;; better comments
  (local-set-key (kbd "C-c C-c") 'comment-norminette) ;; wtf why no work
  ;; whitespace cleanup
  (local-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; uncommenting
  (local-set-key (kbd "C-c c") 'uncomment-region))
