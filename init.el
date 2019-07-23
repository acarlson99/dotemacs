;;; package -- summary:
;; init.el
;;; Commentary:
;; init file
;;; Code:

;; Load general features files
(setq load-path (append (list nil "~/.emacs.d/dump/") load-path))

(load "list.el")
(load "string.el")
(load "comments.el")
(load "header.el")

(autoload 'php-mode "php-mode" "Major mode for editing PHP code" t)
(add-to-list 'auto-mode-alist '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))

										; Set default emacs configuration
(set-language-environment "UTF-8")
(setq-default font-lock-global-modes nil)
(setq-default line-number-mode nil)
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(global-set-key (kbd "DEL") 'backward-delete-char)
(setq-default c-backspace-function 'backward-delete-char)
(setq-default c-basic-offset 4)
(setq-default c-default-style "linux")
(setq-default tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
								64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

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
	(counsel-spotify which-key projectile slime erlang nasm-mode htmlize tuareg caml tramp-term ssh lisp-extra-font-lock scheme-here scheme-complete chicken-scheme go-autocomplete go-gopath go-imports golint cargo php-mode web-mode fish-mode evil-tutor evil-numbers ruby-end ruby-extra-highlight ahk-mode molokai-theme opencl-mode glsl-mode elisp-lint flycheck-golangci-lint python-pylint pylint flycheck rust-playground rust-mode x-path-walker helm go-mode neotree auto-complete evil magit elpy)))
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

(setq load-path (append (list nil "~/.emacs.d/lisp" "~/.emacs.d/packages" "~/.emacs.d/config") load-path))

;; (require 'column-marker)
(require 'fill-column-indicator)
(require 'nlinum)
(require 'hl-todo)
(require 'xahk-mode)
(require 'escreen)
(escreen-install)
(require 'sql-upcase)

(require 'project-start)

(load "defaults.el")
(load "hotkeys.el")
(load "prog-config.el")
(load "c-config.el")
(load "c++-config.el")
(load "ruby-config.el")
(load "term-config.el")
(load "web-config.el")
(load "php-config.el")
(load "sql-config.el")
(load "go-config.el")
(load "slime-config.el")

;; Set modes
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

;; If this fails run "autoconf && ./configure && make" up in that directory
(if (and (file-exists-p "~/.emacs.d/tramp/lisp/tramp.el") (file-exists-p "~/.emacs.d/tramp/lisp/Makefile"))
	(progn
	  (setq load-path (append (list nil "~/.emacs.d/tramp/lisp") load-path))
	  (require 'tramp)
	  (require 'tramp-compat)
	  (setq tramp-default-method "ssh"))
  (print "Loading tramp failed.  Make sure ~/.emacs.d/tramp/lisp/tramp.el and ~/.emacs.d/tramp/lisp/Makefile exist.  Perhaps run 'autoconf && ./configure && make'"))

(provide 'init)
;;; init.el ends here
