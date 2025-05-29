;;; package -- summary:                              -*- lexical-binding: t; -*-
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
   '("47ec21abaa6642fefec1b7ace282221574c2dd7ef7715c099af5629926eb4fd7" "11e57648ab04915568e558b77541d0e94e69d09c9c54c06075938b6abc0189d8" default))
 '(evil-undo-system 'undo-tree)
 '(frame-brackground-mode 'dark)
 '(org-babel-load-languages '((python . t) (shell . t) (emacs-lisp . t) (dot . t) (glsl . t)))
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

(defvar el-init-start-time (current-time))

(setq load-prefer-newer t)

(defun el-current-file () (or load-file-name (buffer-file-name)))

(defvar my-init-file-path (el-current-file))
(defvar my-emacs-d-path
  (file-name-directory my-init-file-path))

(let* ((my-lisp-directory
		(concat my-emacs-d-path "lisp/"))
	   (my-module-directory
		(concat my-emacs-d-path "module/")))
  (progn
	(setq load-path (append (list my-lisp-directory my-module-directory) load-path))
	(let ((default-directory my-lisp-directory))
	  (normal-top-level-add-subdirs-to-load-path))
	))

(with-eval-after-load 'bytecomp
  (byte-recompile-file my-init-file-path nil 0)
  (byte-recompile-directory
   (concat my-emacs-d-path "lisp/")
   0))

(require 'el-log)
(require 'my-defs)
(require 'mod-init)
(require 'cosmetic)

(let ((req-packages
	   ;; google-emacs ships with patched auto-complete
	   ;; so attempt to load google version first
	   (if (require 'auto-complete nil 'noerror)
	       '(flycheck evil-numbers evil)
	     '(flycheck evil-numbers evil auto-complete))))
  (progn
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
	(exec-path-from-shell-initialize)
  (progn
	(el-log-lvl 'WARN "exec-path-from-shell not found.  Loading weird janky version")
	(let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -c 'echo $PATH'"))))
	  (setenv "PATH" (concat (getenv "PATH") ":" path-from-shell))
	  (setq exec-path (append exec-path (split-string path-from-shell path-separator))))))

;; (require 'column-marker)			; Commented out in favor of fci
(require 'fill-column-indicator)
(require 'nlinum)
(require 'hl-todo)
(require 'escreen)
(escreen-install)

;; default stuff
(prune-backups)

(require 'defaults)
(load "mode-conf")
(require 'alist)

;; rando
(require 'gptmacs)
(require 'google nil :noerror)

(auto-insert-mode 1)
;; (add-hook 'emacs-lisp-mode-hook 'auto-make-header)

(when (require 'el-keystore nil 'noerror)
  (el-keystore-load-keys))

(let* ((el-init-end-time (current-time))
	   (init-load-duration
		(float-time (time-subtract el-init-end-time el-init-start-time))))
  (el-log-lvl 'INFO "Loaded emacs config in %d seconds" init-load-duration))

(if (< emacs-major-version 30)
	(el-log-lvl 'WARN "Low emacs version; please update"))

(provide 'init)
;;; init.el ends here
