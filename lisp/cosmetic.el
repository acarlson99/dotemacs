;; set initial-scratch-message
(org-babel-load-file (format "%sscratch-msg.org" (file-name-directory load-file-name)))

;; Theme
(defvar my-default-dark-theme 'manoj-dark)
(defvar my-default-light-theme 'adwaita)

(defun dark-mode ()
  (interactive)
  (load-theme my-default-dark-theme t))
(defun light-mode ()
  (interactive)
  (load-theme my-default-light-theme t))
(dark-mode)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable scroll bar and toolbar in GUI
;; (if (window-system)
;; 	(progn
;; 	  (tool-bar-mode -1)
;; 	  (scroll-bar-mode -1)))

;; Highlight current line
;; (global-hl-line-mode 1)

;; Line wrap
(global-visual-line-mode 1)

;; Disable annoying bell
(setq ring-bell-function 'ignore)

(provide 'cosmetic)
