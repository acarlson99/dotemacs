;;; prune-backups.el --- Prune emacs auto-backups.
;;; Commentary:
;;; Set backup directory to "~/.emacs.d/backups" and clear directory weekly
;;; Code:

(message "BEGIN PRUNE-BACKUPS")

;; Make sure backup directory exists
(if (not (file-directory-p "~/.emacs.d/backups"))
	(make-directory "~/.emacs.d/backups"))

;; Redirects emacs backups
(setq backup-directory-alist
	  `((".*" . , "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
	  `((".*" , "~/.emacs.d/backups" t)))

(let ((last-run-time-file "~/.emacs.d/.lastIwiped")
	  (week (* 60 60 24 7)))
  (if (not (file-exists-p last-run-time-file))
	  (write-region (number-to-string (float-time (current-time))) nil last-run-time-file))
  (let ((time (with-temp-buffer
				(insert-file-contents last-run-time-file)
				(string-to-number (buffer-string))))
		(current (float-time (current-time))))
	(if (> current (+ time week))
		(progn
		  ;; Purges files not accessed in a week
		  (message "Deleting old backup files...")
		  (dolist (file (directory-files "~/.emacs.d/backups" t))
			(when (and (backup-file-name-p file)
					   (> (- current (float-time (nth 5 (file-attributes file))))
						  week))
			  (message "%s" file)
			  (delete-file file)))
		  t)
	  nil)))

(message "END PRUNE-BACKUPS")
