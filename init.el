;*******************************************************************************;
;                                                                               ;
;                                                          :::      ::::::::    ;
;    dotemacs                                            :+:      :+:    :+:    ;
;                                                      +:+ +:+         +:+      ;
;    by: thor <thor@42.fr>                           +#+  +:+       +#+         ;
;                                                  +#+#+#+#+#+   +#+            ;
;    Created: 2013/06/18 14:01:14 by thor               #+#    #+#              ;
;    Updated: 2018/11/04 22:13:06 by acarlson         ###   ########.fr        ;
;                                                                               ;
;*******************************************************************************;

; Load general features files
;; (setq config_files "/usr/share/emacs/site-lisp/")
;; (setq load-path (append (list nil config_files) load-path))

(setq config_files "~/.emacs.d/dump/")
(setq load-path (append (list nil config_files) load-path))

(load "list.el")
(load "string.el")
(load "comments.el")
(load "header.el")

;; (autoload 'php-mode "php-mode" "Major mode for editing PHP code" t)
;; (add-to-list 'auto-mode-alist '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))

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

(setq config_files "~/.emacs.d/files")
(setq load-path (append (list nil config_files) load-path))

(load "move_text.el")
(load "backup_redirect.el")
(load "window_movement.el")
(load "tabbar.el")


;; Version specific
(if (version< emacs-version "25")
	()
  (load-theme 'manoj-dark t)
  (global-linum-mode 1)
  (load "highlighting.el")
  (add-hook 'prog-mode-hook 'hl-todo-mode))

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Set line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Set hotkeys for...
;; moving text
(global-set-key (kbd "C-x C-p") 'move-text-up)
(global-set-key (kbd "C-x C-n") 'move-text-down)
;; whitespace cleanup
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
;; changing panes in GUI
(global-set-key (kbd "s-[") 'back-window)
(global-set-key (kbd "s-]") 'other-window)

;; Turns on font lock mode, abbrev mode, and show paren mode! YES!
(add-hook 'prog-mode-hook 'abbrev-mode)
(add-hook 'prog-mode-hook 'font-lock-mode)
(add-hook 'text-mode-hook 'font-lock-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
