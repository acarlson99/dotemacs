;;; my-defs.el --- little funcs                      -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
	(if (> (point) (mark))
		(exchange-point-and-mark))
	(let ((column (current-column))
		  (text (delete-and-extract-region (point) (mark))))
	  (forward-line arg)
	  (move-to-column column t)
	  (set-mark (point))
	  (insert text)
	  (exchange-point-and-mark)
	  (setq deactivate-mark nil)))
   (t
	(beginning-of-line)
	(when (or (> arg 0) (not (bobp)))
	  (forward-line)
	  (when (or (< arg 0) (not (eobp)))
		(transpose-lines arg))
	  (forward-line -1)))))

;; Move text up and down
(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line ARG lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun back-window ()
  "Switch to previous window."
  (interactive)
  (other-window -1))

(defmacro curry (fun &rest args)
  "Curry curries FUN with ARGS."
  `(lambda (&rest args2)
    (apply ,fun (append '(,@args) args2))))

(defun prune-backups ()
  "Set backup directory to `~/.emacs.d/backups' and clear directory weekly"
  (progn
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
			nil))))
	(message "END PRUNE-BACKUPS")))

(provide 'my-defs)
;;; my-defs.el ends here
