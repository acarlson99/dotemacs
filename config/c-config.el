;; TODO: fix.  Breaks with long or massively indented line
(defun comment-norminette ()
  "Redefinition of block commenting to appease Norminette."
  (interactive)
  (if (use-region-p)
	  (let ((s (line-number-at-pos))
			(rs (region-beginning))
			(re (region-end))
			(e 0))
		(progn
		  (exchange-point-and-mark)
		  (setq e (line-number-at-pos))
		  (if (> s e) (setq s (prog1 e (setq e  s))))
		  (let ((i s))
			(progn (goto-line s)
				   (beginning-of-line)
				   (insert "/*\n")
				   (while (< i e)
					 (progn
					   (beginning-of-line)
					   (insert "** ")
					   (setq i (+ i 1))
					   (next-line)))
				   (beginning-of-line)
				   (insert "*/\n")))))
	(progn
	  (beginning-of-line)
	  (insert "/*\n**")
	  (end-of-line)
	  (insert "\n*/"))))

(defun c-protect-header (name)
  "Insert inclusion protection given NAME."
  (interactive "sInclusion protection name(blank for FILENAME_EXT): ")
  (if (equal name "")
	  (setq name (upcase (replace-regexp-in-string
						  "\\."
						  "_"
						  (file-name-nondirectory (buffer-file-name))))))
  (insert "#ifndef " name "\n# define " name "\n\n#endif"))

(defun c-clang-format (&optional style)
  "Run clang-format with STYLE on buffer or region."
  (interactive "P")
  (if (equal style "")
	  (setq style "llvm"))
  (if (executable-find "clang-format")
	  (if (not (shell-command-on-region (point-min) (point-max) "clang-format" t t))
		  (error "Command clang-format failed"))
	(message "clang-format not found")))

(defun c-config ()
  "For use in 'c-mode-hook'."
  ;; header protection
  (local-set-key (kbd "C-c C-p") 'c-protect-header)
  ;; (c-toggle-comment-style -1)							; Commented out for norminette
  ;; Norm comments
  ;; (local-set-key (kbd "C-c C-c") 'comment-norminette)	; removed because annoying
  (local-set-key (kbd "C-c f") 'c-clang-format)
  ;; whitespace cleanup
  (local-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; uncommenting
  (local-set-key (kbd "C-c c") 'uncomment-region))

(provide 'c-config)
