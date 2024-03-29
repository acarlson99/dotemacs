(require 'el-log)

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
  (if (executable-find "clang-format")
	  (let ((cmd (if style (concat "clang-format --style=" style) "clang-format")))
		(if (or (not style) (member style '("llvm" "google" "chromium" "mozilla" "webkit" "file")))
			(if (not
				 (shell-command-on-region (point-min) (point-max) cmd t t))
				(error "Command clang-format failed"))
		  (error "Unsupported style.  Use llvm,google,chromium,mozilla,webkit,file")))
	(el-log-lvl 'WARN "clang-format not found")))

(defun c-config ()
  "For use in `c-mode-hook'."
  ;; header protection
  (local-set-key (kbd "C-c C-p") 'c-protect-header)
  ;; (c-toggle-comment-style -1)							; Commented out for norminette
  ;; Norm comments
  ;; (local-set-key (kbd "C-c C-c") 'comment-norminette)	; removed because annoying
  ;; using package to clang format by default
  (local-set-key (kbd "C-c f")
				 (if (require 'clang-format nil 'noerror)
					 'clang-format-buffer
				   'c-clang-format))
  ;; clang-format on save
  (if (executable-find "clang-format")
	  (add-hook 'before-save-hook 'clang-format-buffer nil t))
  ;; whitespace cleanup
  (local-set-key (kbd "C-c w") 'whitespace-cleanup))

(provide 'c-config)
