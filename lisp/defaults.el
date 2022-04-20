;;; defaults --- Summary:
;; default settings
;;; Commentary:
;; settings
;;; Code:

;;; My stuff
;; Set line and column numbers
(setq-default line-number-mode t)
(setq-default column-number-mode t)

;; Line numbers
(global-nlinum-mode 1)

;; Font lock mode
(setq-default font-lock-mode t)

;; Make search case insensetive
;; NOTE: can be toggled with `M-c` in isearch mode
;; see https://www.gnu.org/software/emacs/manual/html_node/efaq/Controlling-case-sensitivity.html
(setq-default case-fold-search t)
(setq-default case-replace t)

;; Set default Emacs configuration
(set-language-environment "UTF-8")
;; (setq-default font-lock-global-modes nil)
;; (setq-default line-number-mode nil)
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)
(global-set-key (kbd "DEL") 'backward-delete-char)
(global-set-key (kbd "<end>") 'end-of-visual-line)
(global-set-key (kbd "<home>") 'beginning-of-visual-line)
(setq-default c-backspace-function 'backward-delete-char)
(setq-default c-basic-offset 4)
(setq-default c-default-style "linux")
(setq-default tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
								64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

(provide 'defaults)
;;; defaults.el ends here
