;;; package -- summary:
;; init.el
;;; Commentary:
;; init file
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
   '("47ec21abaa6642fefec1b7ace282221574c2dd7ef7715c099af5629926eb4fd7" "11e57648ab04915568e558b77541d0e94e69d09c9c54c06075938b6abc0189d8" default))
 '(frame-brackground-mode 'dark)
 '(org-babel-load-languages '((python . t) (shell . t) (emacs-lisp . t) (dot . t)))
 '(package-selected-packages '(auto-complete evil evil-numbers flycheck))
 '(send-mail-function 'mailclient-send-it)
 '(show-trailing-whitespace t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(trailing-whitespace ((t (:background "purple4")))))

;;; BEGIN MY CODE

(let ((req-packages '(flycheck evil-numbers evil)))
  (progn
	;; google-emacs ships with patched auto-complete
	;; so attempt to load google version first
	(if (require 'google nil 'noerror)
		(require 'auto-complete)
	  (add-to-list 'req-packages 'auto-complete))

	;; init pkg
	(require 'package)
	(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
	(package-initialize)
	;; install required uninstalled packages
	(let ((missing-packages (cl-remove-if 'package-installed-p req-packages)))
	  (if (/= (length missing-packages) 0)
		  (if (yes-or-no-p (format "Packages not installed %s. Install? " missing-packages))
			  (progn
				(package-refresh-contents)
				(mapc 'package-install missing-packages)))))))

;; TODO: find better way to determine filesystem
;; oh no nfs
(defvar on-nfs-p (string-match "/nfs/" (getenv "HOME")))

;; append shell (SHELL) path to path and exec-path.  Set path
;; NOTE: not run in interactive mode, so only /etc/profile, ~/.profile, etc. is run
(if (require 'exec-path-from-shell nil 'noerror)
	(let ((exec-path-from-shell-check-startup-files))
	  (exec-path-from-shell-initialize))
  (progn
	(message "exec-path-from-shell not found.  Loading weird janky version")
	(let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -c 'echo $PATH'"))))
	  (setenv "PATH" (concat (getenv "PATH") ":" path-from-shell))
	  (setq exec-path (append exec-path (split-string path-from-shell path-separator))))))

(defvar my-default-dark-theme 'manoj-dark)
(defvar my-default-light-theme 'adwaita)

(let* ((my-lisp-directory
		(concat
		 (file-name-directory (or load-file-name (buffer-file-name)))
		 "lisp/"))
	   (load-path (append (list my-lisp-directory) load-path)))
  (progn
	(let ((default-directory my-lisp-directory))
	  (normal-top-level-add-subdirs-to-load-path))

	;; Other packages
	;; (require 'column-marker)			; Commented out in favor of fci
	(require 'fill-column-indicator)
	(require 'nlinum)
	(require 'hl-todo)
	(require 'escreen)
	(escreen-install)
	(require 'sql-upcase)

	(require 'prettier-js)

	;; default stuff
	(require 'cosmetic)
	(load "prune-backups")
	(require 'defaults)
	(require 'globals)
	(load "mode-conf")
	(require 'alist)

	;; rando
	(require 'gptmacs)

	;; TODO: add API key loader, why not
	))

(provide 'init)
;;; init.el ends here
