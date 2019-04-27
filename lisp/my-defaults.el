;; Make sure backup directory exists
(if (not (file-directory-p "~/.emacs.d/backups"))
	(make-directory "~/.emacs.d/backups"))
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
(global-nlinum-mode 1)

;; Preset `nlinum-format' for minimum width.
(defun my-nlinum-mode-hook ()
  (when nlinum-mode
	(setq-local nlinum-format
				(concat "%" (number-to-string
							 ;; Guesstimate number of buffer lines.
							 (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
						"d"))))
(add-hook 'nlinum-mode-hook #'my-nlinum-mode-hook)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable scroll bar and toolbar in GUI
(if (window-system)
	(progn
	(tool-bar-mode -1)
	(scroll-bar-mode -1)))

;; Set line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Make search case sensetive
(setq-default case-fold-search nil)

;; Highlight current line
(global-hl-line-mode 1)
(set-face-foreground 'hl-line nil)

;; Line wrap
(global-visual-line-mode 1)

;; Disable annoying bell
(setq ring-bell-function 'ignore)
