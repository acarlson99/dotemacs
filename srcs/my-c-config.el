;; Norminette-compliant block commenting
(defun comment-norminette (jim)
  "Redefinition of block commenting to appease Norminette"
  (interactive "s: ")
  (setq s (line-number-at-pos))
  (setq rs (region-beginning))
  (setq re (region-end))
  (exchange-point-and-mark)
  (setq e (line-number-at-pos))
  (if (> s e) (setq s  (prog1 e (setq e  s))))
  ;; (setq i s)
  ;; (while (< i e)
  ;; 	(
  ;; 	 (goto-line i)
  ;; 	 (beginning-of-line)
  ;; 	 (insert "** ")
  ;; 	 (setq i (1+ i))
  ;; 	 )
  ;; 	)
  (replace-rectangle rs re "** ")	;; This comments one line too many.  Fix loop
  (goto-line e)	;; This comments one line too many.  Subtract one from this when loop is fixed
  (end-of-line)
  (insert "\n*/")
  (goto-line s)
  (beginning-of-line)
  (insert "/*\n")
  )


(defun my-c-config()
  "For use in 'c-mode-hook'."
  (global-unset-key (kbd "C-c C-c"))
  (global-set-key (kbd "C-c C-c") 'comment-norminette) ;; WHY TF DOES THIS NOT WORK
  ;; whitespace cleanup
  (global-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; uncommenting
  (global-set-key (kbd "C-c c") 'uncomment-region))
