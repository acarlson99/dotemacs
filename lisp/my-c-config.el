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

(defun c-protect-header (name)
  "Insert inclusion protection"
  (interactive "sInclusion protection name: ")
  (if (equal name "")
	  (setq name (upcase (concat (file-name-base) "_" (file-name-extension(buffer-file-name)))))
	(setq name (upcase name))
	)
  (insert "#ifndef " name "\n# define " name "\n\n#endif")
  )

(defun my-c-config()
  "For use in 'c-mode-hook'."
  ;; header protection
  (local-set-key (kbd "C-c C-p") 'c-protect-header)
  ;; better comments
  (local-set-key (kbd "C-c C-c") 'comment-norminette)
  ;; whitespace cleanup
  (local-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; uncommenting
  (local-set-key (kbd "C-c c") 'uncomment-region))
