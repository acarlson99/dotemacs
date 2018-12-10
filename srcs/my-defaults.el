;; Redirects emacs backups
(setq backup-directory-alist
	  `((".*" . , "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
	  `((".*" , "~/.emacs.d/backups" t)))

;; Purges files not accessed in a week
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files "~/.emacs.d/backups" t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (nth 5 (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;; Theme
(load-theme 'manoj-dark t)

;; Line numbers
(global-linum-mode 1)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable scroll bar in GUI
(if (window-system)
	(scroll-bar-mode -1)
  )

;; Set line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Make search case sensetive
(setq-default case-fold-search nil)
