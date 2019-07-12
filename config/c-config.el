(defun comment-norminette ()
  "Redefinition of block commenting to appease Norminette."
  (interactive)
  (if (use-region-p)
	  (let ((s (line-number-at-pos))
			(rs (region-beginning))
			(re (region-end))
			(e 0)
			(i s))
		(progn
		  (exchange-point-and-mark)
		  (setq e (line-number-at-pos))
		  (if (> s e) (setq s  (prog1 e (setq e  s))))
		  (goto-line s)
		  (beginning-of-line)
		  (insert "/*\n")
		  (while (< i e)
			(progn
			  (beginning-of-line)
			  (insert "** ")
			  (setq i (+ i 1))
			  (next-line)))
		  (beginning-of-line)
		  (insert "*/\n")))
	(progn
	  (beginning-of-line)
	  (insert "/*\n**")
	  (end-of-line)
	  (insert "\n*/"))))

(defun c-protect-header (name)
  "Insert inclusion protection given NAME."
  (interactive "sInclusion protection name(blank for FILENAME_EXT): ")
  (if (equal name "")
	  (setq name (upcase (concat (file-name-base) "_" (file-name-extension(buffer-file-name)))))
	(setq name (upcase name)))
  (insert "#ifndef " name "\n# define " name "\n\n#endif"))

(defun my-c-config()
  "For use in 'c-mode-hook'."
  ;; header protection
  (local-set-key (kbd "C-c C-p") 'c-protect-header)
  ;; (c-toggle-comment-style -1)							; Commented out for norminette
  ;; Norm comments
  ;; (local-set-key (kbd "C-c C-c") 'comment-norminette)	; removed because annoying
  ;; whitespace cleanup
  (local-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; uncommenting
  (local-set-key (kbd "C-c c") 'uncomment-region))
