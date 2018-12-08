;******************************************************************************;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    init.el                                            :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    by: thor <thor@42.fr>                           +#+  +:+       +#+        ;
;                                                  +#+#+#+#+#+   +#+           ;
;    Created: 2013/06/18 14:01:14 by thor               #+#    #+#             ;
;    Updated: 2018/12/08 15:41:35 by acarlson         ###   ########.fr        ;
;                                                                              ;
;******************************************************************************;
; Load general features files
;; (setq config_files "/usr/share/emacs/site-lisp/")
;; (setq load-path (append (list nil config_files) load-path))

;; Load general features files
(setq config_files "~/.emacs.d/dump/")
(setq load-path (append (list nil config_files) load-path))

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

;; Load user configuration
;; (if (file-exists-p "~/.myemacs") (load-file "~/.myemacs"))



;*******************************************************************************;

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
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(package-selected-packages (quote (magit evil elpy auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq config_files "~/.emacs.d/srcs")
(setq load-path (append (list nil config_files) load-path))

(require 'column-marker)
(require 'hl-todo)
(require 'evil)
(evil-mode 1)
(require 'escreen)
(escreen-install)

(load "my-defaults.el")
(load "my-hotkeys.el")
(load "my-prog-config.el")
(load "my-c-config.el")
(load "my-term-config.el")

(defun lipsum-load (arg)
  "Load lorem-ipsum.el"
  (interactive "p")
  (require 'lorem-ipsum)
  )

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("marmalade" . "http://marmalade-repo.org/packages/")
						 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; auto-complete-mode settings
(ac-config-default)
(setq ac-use-menu-map t)
(add-to-list 'ac-modes 'prog-mode)

;; Set modes
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-2 80)))
(add-hook 'prog-mode-hook 'my-prog-config)
(add-hook 'c-mode-hook 'my-c-config)
(add-hook 'org-mode-hook 'font-lock-mode)
(add-hook 'term-mode-hook 'my-term-config)
