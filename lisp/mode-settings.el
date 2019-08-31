;; Turn on evil mode if it is installed
(when (require 'evil nil 'noerror)
  (evil-mode 1)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'neotree-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'comint-mode 'emacs)
  (evil-set-initial-state 'slime-repl-mode 'emacs)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)

  (define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-visual-line)

  (when (require 'evil-numbers nil 'noerror)
	(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
	(define-key evil-normal-state-map (kbd "C-q") 'evil-numbers/dec-at-pt)))

;; Set auto-complete-mode settings if installed
(when (require 'auto-complete nil 'noerror)
  (ac-config-default)
  (setq ac-use-menu-map t)
  (add-hook 'go-mode-hook 'auto-complete-for-go)
  (add-to-list 'ac-modes 'prog-mode)
  (add-to-list 'ac-modes 'makefile-bsdmake-mode)
  (add-to-list 'ac-modes 'makefile-gmake-mode)
  (add-to-list 'ac-modes 'makefile-mode)
  (add-to-list 'ac-modes 'nasm-mode))

;; Set global undo tree
;; C-x u is amazing
(when (require 'undo-tree nil 'noerror)
  (global-undo-tree-mode))

;; Set global flycheck
(when (require 'flycheck nil 'noerror)
  (global-flycheck-mode))

;; projectile project management
(when (require 'projectile nil 'noerror)
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/projects/"))
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; load slime stuff properly
(when (require 'slime nil 'noerror)
  (progn
	(setq inferior-lisp-program (or (executable-find "sbcl") (executable-find "clisp") (executable-find "sbcl.exe") (executable-find "clisp.exe")))
	(setq slime-contribs '(slime-fancy))))

;; Preset `nlinum-format' for minimum width.
(when (require 'nlinum nil 'noerror)
  (defun my-nlinum-mode-hook ()
	"Recommended nlinum-mode function for nlinum-mode."
	(when nlinum-mode
	  (setq-local nlinum-format
				  (concat "%" (number-to-string
							   ;; Guesstimate number of buffer lines.
							   (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
						  "d"))))
  (add-hook 'nlinum-mode-hook #'my-nlinum-mode-hook))

;; If this fails run "autoconf && ./configure && make" up in that directory
(if (and (file-exists-p "~/.emacs.d/tramp/lisp/tramp.el") (file-exists-p "~/.emacs.d/tramp/lisp/Makefile") (require 'tramp nil 'noerror) (require 'tramp-compat nil 'noerror))
	(progn
	  (setq load-path (append (list nil "~/.emacs.d/tramp/lisp") load-path))
	  (setq tramp-default-method "ssh"))
  (message "Loading tramp failed.  Make sure ~/.emacs.d/tramp/lisp/tramp.el and ~/.emacs.d/tramp/lisp/Makefile exist and that tramp and tramp-compat are available.  Perhaps run 'autoconf && ./configure && make'"))

(provide 'mode-settings)
