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
	(bison-mode slime counsel-spotify which-key projectile erlang nasm-mode htmlize tuareg caml tramp-term ssh lisp-extra-font-lock scheme-here scheme-complete chicken-scheme go-autocomplete go-gopath go-imports golint cargo php-mode web-mode fish-mode evil-tutor evil-numbers ruby-end ruby-extra-highlight ahk-mode molokai-theme opencl-mode glsl-mode elisp-lint flycheck-golangci-lint python-pylint pylint flycheck rust-playground rust-mode x-path-walker helm go-mode neotree auto-complete evil magit elpy)))
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

;; append shell (SHELL) path to path and exec-path.  Set path
(let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
  (setenv "PATH" (concat (getenv "PATH") ":" path-from-shell))
  (setq exec-path (append exec-path (split-string path-from-shell path-separator))))

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

(require 'project-start)

;; default stuff
(require 'defaults)
(load "prune-backups.el")
(load "cosmetic.el")
(load "mode-settings.el")
(load "hotkeys.el")

;; configs
(load "prog-config.el")
(load "c-config.el")
(load "c++-config.el")
(load "ruby-config.el")
(load "term-config.el")
(load "web-config.el")
(load "php-config.el")
(load "sql-config.el")
(load "go-config.el")

;; Set mode hooks
;; (add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-2 80)))
;; commented out in favor of fci-mode
(add-hook 'prog-mode-hook 'my-prog-config)
(add-hook 'c-mode-hook 'my-c-config)
(add-hook 'c++-mode-hook 'my-c++-config)
(add-hook 'ruby-mode-hook 'my-ruby-config)
(add-hook 'org-mode-hook 'font-lock-mode)
(add-hook 'term-mode-hook 'my-term-config)
(add-hook 'web-mode-hook 'my-web-config)
(add-hook 'php-mode-hook 'my-php-config)
(add-hook 'sql-mode-hook 'my-sql-config)
(add-hook 'go-mode-hook 'my-go-config)

(provide 'init)
;;; init.el ends here
