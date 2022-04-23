(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
	(if (> (point) (mark))
		(exchange-point-and-mark))
	(let ((column (current-column))
		  (text (delete-and-extract-region (point) (mark))))
	  (forward-line arg)
	  (move-to-column column t)
	  (set-mark (point))
	  (insert text)
	  (exchange-point-and-mark)
	  (setq deactivate-mark nil)))
   (t
	(beginning-of-line)
	(when (or (> arg 0) (not (bobp)))
	  (forward-line)
	  (when (or (< arg 0) (not (eobp)))
		(transpose-lines arg))
	  (forward-line -1)))))

;; Move text up and down
(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line ARG lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun back-window ()
  "Switch to previous window."
  (interactive)
  (other-window -1))

;; Change panes in GUI
(global-set-key (kbd "C-{") 'back-window)
(global-set-key (kbd "C-}") 'other-window)

;; Escreen settings
(add-hook 'escreen-goto-screen-hook
		  'escreen-enable-number-mode-if-more-than-one-screen)
(global-set-key (kbd "C-c 0") 'escreen-goto-screen-0)
(global-set-key (kbd "C-c 1") 'escreen-goto-screen-1)
(global-set-key (kbd "C-c 2") 'escreen-goto-screen-2)
(global-set-key (kbd "C-c 3") 'escreen-goto-screen-3)
(global-set-key (kbd "C-c 4") 'escreen-goto-screen-4)
(global-set-key (kbd "C-c 5") 'escreen-goto-screen-5)
(global-set-key (kbd "C-c 6") 'escreen-goto-screen-6)
(global-set-key (kbd "C-c 7") 'escreen-goto-screen-7)
(global-set-key (kbd "C-c 8") 'escreen-goto-screen-8)
(global-set-key (kbd "C-c 9") 'escreen-goto-screen-9)
(global-set-key (kbd "C-c >") 'escreen-goto-next-screen)
(global-set-key (kbd "C-c <") 'escreen-goto-prev-screen)

;; My god this is amazing! Man pages on command! Com-man-d pages
(global-set-key [(f5)] (lambda () (interactive) (manual-entry (current-word))))

;; Set neotree settings if installed
(when (require 'neotree nil 'noerror)
  (global-set-key [f8] 'neotree-toggle))

;; set S-x to helm-M-x
(when (require 'helm nil 'noerror)
  (global-set-key (kbd "s-x") 'helm-M-x))

;; Set global undo tree
;; C-x u is amazing
(require 'undo-tree nil 'noerror)
(global-undo-tree-mode)
(setq evil-undo-system 'undo-tree)

;; Turn on evil mode if it is installed
(require 'evil nil 'noerror)
(evil-mode 1)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'eshell-mode 'emacs)
(evil-set-initial-state 'neotree-mode 'emacs)
(evil-set-initial-state 'help-mode 'emacs)
(evil-set-initial-state 'comint-mode 'emacs)
(evil-set-initial-state 'slime-repl-mode 'emacs)
(evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
(evil-set-initial-state 'smudge-mode 'emacs)
(evil-set-initial-state 'smudge-device-select-mode 'emacs)
(evil-set-initial-state 'smudge-playlist-search-mode 'emacs)
(evil-set-initial-state 'smudge-track-search-mode 'emacs)

(define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-visual-line)

(require 'evil-numbers nil 'noerror)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-q") 'evil-numbers/dec-at-pt)

;; Set auto-complete-mode settings if installed
(require 'auto-complete)
(ac-config-default)
(setq ac-use-menu-map t)
;; (add-hook 'go-mode-hook 'auto-complete-for-go)
(add-to-list 'ac-modes 'prog-mode)
(add-to-list 'ac-modes 'makefile-bsdmake-mode)
(add-to-list 'ac-modes 'makefile-gmake-mode)
(add-to-list 'ac-modes 'makefile-mode)
(add-to-list 'ac-modes 'nasm-mode)

;; Set global flycheck
(require 'flycheck)
(global-flycheck-mode)

;; projectile project management
(require 'projectile)
(projectile-mode +1)
(setq projectile-project-search-path '("~/projects/"))
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; load slime stuff properly
(when (require 'slime nil 'noerror)
  (progn
	(setq inferior-lisp-program (or (executable-find "sbcl") (executable-find "clisp") (executable-find "sbcl.exe") (executable-find "clisp.exe")))
	(setq slime-contribs '(slime-fancy))))

;; Preset `nlinum-format' for minimum width.
(require 'nlinum)
(defun my-nlinum-mode-hook ()
  "Recommended nlinum-mode function for nlinum-mode."
  (when nlinum-mode
	(setq-local nlinum-format
				(concat "%" (number-to-string
							 ;; Guesstimate number of buffer lines.
							 (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
						"d"))))
(add-hook 'nlinum-mode-hook #'my-nlinum-mode-hook)

;; If this fails run "autoconf && ./configure && make" up in that directory
(if (and (file-exists-p "~/.emacs.d/tramp/lisp/tramp.el") (file-exists-p "~/.emacs.d/tramp/lisp/Makefile") (require 'tramp nil 'noerror) (require 'tramp-compat nil 'noerror))
	(progn
	  (message "tramp loaded")
	  (setq load-path (append (list nil "~/.emacs.d/tramp/lisp") load-path))
	  (setq tramp-default-method "ssh"))
  (message "Loading tramp failed.  Make sure ~/.emacs.d/tramp/lisp/tramp.el and ~/.emacs.d/tramp/lisp/Makefile exist and that tramp and tramp-compat are available.  Perhaps run 'autoconf && ./configure && make'"))

(provide 'globals)
