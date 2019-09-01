;;; package -- summary:
;; init.el
;;; Commentary:
;; init file
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("11e57648ab04915568e558b77541d0e94e69d09c9c54c06075938b6abc0189d8" default)))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(package-selected-packages
   (quote
	(clang-format exec-path-from-shell powershell yaml-mode golint govet bison-mode slime counsel-spotify which-key projectile erlang nasm-mode htmlize tuareg caml tramp-term ssh lisp-extra-font-lock scheme-here scheme-complete chicken-scheme go-autocomplete go-gopath go-imports cargo php-mode web-mode fish-mode evil-tutor evil-numbers ruby-end ruby-extra-highlight ahk-mode molokai-theme opencl-mode glsl-mode elisp-lint flycheck-golangci-lint python-pylint pylint flycheck rust-playground rust-mode x-path-walker helm go-mode neotree auto-complete evil magit elpy)))
 '(send-mail-function (quote mailclient-send-it))
 '(show-trailing-whitespace t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(trailing-whitespace ((t (:background "purple4")))))

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "http://marmalade-repo.org/packages/")
						 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; TODO: find better way to determine filesystem
;; oh no nfs
(setq on-nfs-p (cl-search "/nfs/" (getenv "HOME")))

;; append shell (SHELL) path to path and exec-path.  Set path
;; (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;;   (setenv "PATH" (concat (getenv "PATH") ":" path-from-shell))
;;   (setq exec-path (append exec-path (split-string path-from-shell path-separator))))
;; does the same thing (I think) as above code, but probably better because I didn't write it
;; NOTE: not run in interactive mode, so only /etc/profile, ~/.profile, etc. is run
(if (require 'exec-path-from-shell nil 'noerror)
	(let ((exec-path-from-shell-check-startup-files))
	  (when (memq window-system '(mac ns x))
		(exec-path-from-shell-initialize)))
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
	(setenv "PATH" (concat (getenv "PATH") ":" path-from-shell))
	(setq exec-path (append exec-path (split-string path-from-shell path-separator)))))

(setq load-path (append (list nil "~/.emacs.d/dump/" "~/.emacs.d/lisp" "~/.emacs.d/packages" "~/.emacs.d/config") load-path))

;; Load general features files
(require 'list)
(require 'string)
(require 'comments)
(require 'header)

;; Other packages
;; (require 'column-marker)			; Commented out in favor of fci
(require 'fill-column-indicator)
(require 'nlinum)
(require 'hl-todo)
(require 'xahk-mode)
(require 'escreen)
(escreen-install)
(require 'sql-upcase)

;; default stuff
(require 'cosmetic)
(load "prune-backups")
(require 'defaults)
(require 'globals)
(require 'mode-settings)

;; configs
(require 'prog-config)
(require 'c-config)
(require 'c++-config)
(require 'ruby-config)
(require 'term-config)
(require 'web-config)
(require 'php-config)
(require 'sql-config)
(require 'go-config)

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
(add-hook 'sql-mode-hook 'sql-config)
(add-hook 'go-mode-hook 'go-config)

(provide 'init)
;;; init.el ends here
