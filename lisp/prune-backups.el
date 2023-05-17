;;; prune-backups.el --- Prune emacs auto-backups.
;;; Commentary:
;;; Set backup directory to "~/.emacs.d/backups" and clear directory weekly
;;; Code:

(message "BEGIN PRUNE-BACKUPS")

(let ((backupdir (mapconcat 'identity `(,user-emacs-directory "backups") ""))
	  (wipefile (mapconcat 'identity `(,user-emacs-directory ".lastIwiped") "")))

  ;; Make sure backup directory exists
  (if (not (file-directory-p backupdir))
	  (make-directory backupdir))

  ;; Redirects emacs backups
  (setq backup-directory-alist
		`((".*" . , backupdir)))
  (setq auto-save-file-name-transforms
		`((".*" , backupdir)))

  (let ((last-run-time-file wipefile)
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
			(dolist (file (directory-files backupdir))
			  (when (and (backup-file-name-p file)
						 (> (- current (float-time (nth 5 (file-attributes file))))
							week))
				(message "%s" file)
				(delete-file file)))
			t)
		nil)))
  )

(message "END PRUNE-BACKUPS")
