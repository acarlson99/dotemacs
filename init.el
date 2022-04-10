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
 '(custom-safe-themes
   (quote
	("47ec21abaa6642fefec1b7ace282221574c2dd7ef7715c099af5629926eb4fd7" "11e57648ab04915568e558b77541d0e94e69d09c9c54c06075938b6abc0189d8" default)))
 '(frame-brackground-mode (quote dark))
 '(org-babel-load-languages (quote ((python . t) (shell . t) (emacs-lisp . t))))
 '(send-mail-function (quote mailclient-send-it))
 '(show-trailing-whitespace t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(trailing-whitespace ((t (:background "purple4")))))

;;; BEGIN MY CODE

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

(let* ((submodules '("flycheck" "evil" "tramp"))
	  (load-path
	   (append
		(list nil "~/.emacs.d/dump/" "~/.emacs.d/lisp" "~/.emacs.d/packages" "~/.emacs.d/config")
		(mapcar (lambda (x) (concat "~/.emacs.d/"x"/")) submodules)
		load-path)))
  (progn

	;; Load general features files from 42
	(require 'list)
	(require 'string)
	(require 'comments)
	(require 'header)

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

	;; configs
	(require 'prog-config)
	(require 'c-config)
	(require 'c++-config)
	(require 'ruby-config)
	(require 'term-config)
	(require 'web-config)
	(require 'php-config)
	(require 'js-config)
	(require 'sql-config)
	(require 'go-config)))

;; Set mode hooks
;; (add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-2 80)))
;; commented out in favor of fci-mode
(add-hook 'prog-mode-hook 'prog-config)
(add-hook 'c-mode-hook 'c-config)
(add-hook 'c++-mode-hook 'c++-config)
(add-hook 'ruby-mode-hook 'ruby-config)
(add-hook 'org-mode-hook 'font-lock-mode)
(add-hook 'term-mode-hook 'term-config)
(add-hook 'web-mode-hook 'web-config)
(add-hook 'php-mode-hook 'php-config)
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'js-mode-hook 'js-config)
(add-hook 'sql-mode-hook 'sql-config)
(add-hook 'go-mode-hook 'go-config)

(provide 'init)
;;; init.el ends here
