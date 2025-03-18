(require 'el-log)

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

;; Turn on evil mode if it is installed
(require 'evil nil 'noerror)
;; commented out because this is bad.  Just turn on flycheck in relevant buffers
;; (evil-mode 1)
(setq evil-undo-system 'undo-tree)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'emacs-lisp-mode 'emacs)
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
(evil-set-initial-state 'agda2-mode 'emacs)
(evil-set-initial-state 'fundamental-mode 'emacs)
(evil-set-initial-state 'grep-mode 'emacs)

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
(with-eval-after-load 'flycheck
  ;; https://github.com/flycheck/flycheck/issues/1559
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
(global-flycheck-mode)

;; projectile project management
(require 'projectile)
(projectile-mode +1)
(setq projectile-project-search-path '("~/projects/" "~/p/"))
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(when (require 'which-key nil 'noerror)
  (which-key-mode))

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
	  (el-log "tramp loaded")
	  (setq load-path (append (list nil "~/.emacs.d/tramp/lisp") load-path))
	  (setq tramp-default-method "ssh"))
  (el-log-lvl 'WARN "Loading tramp failed.  Make sure ~/.emacs.d/tramp/lisp/tramp.el and ~/.emacs.d/tramp/lisp/Makefile exist and that tramp and tramp-compat are available.  Perhaps run 'autoconf && ./configure && make'"))

;; hs-minor-mode
(with-eval-after-load 'hideshow
  (progn
	(define-key hs-minor-mode-map (kbd "C-c -") 'hs-hide-block)
	(define-key hs-minor-mode-map (kbd "C-c =") 'hs-show-block)
	(define-key hs-minor-mode-map (kbd "C-c C--") 'hs-hide-all)
	(define-key hs-minor-mode-map (kbd "C-c C-=") 'hs-show-all)
	(define-key hs-minor-mode-map (kbd "C-'") 'hs-toggle-hiding)
	(define-key hs-minor-mode-map (kbd "C-c w") 'my/org-copy-visible)
	(define-key hs-minor-mode-map (kbd "C-c C-w") 'my/org-copy-visible)))

(defvar agda-locate-command "agda-mode locate")
(let ((exitCode (shell-command agda-locate-command)))
  (cond
   ((equal exitCode 127) (el-log-lvl 'WARN "agda-mode not found with command '%s'; exit code %d" agda-locate-command exitCode))
   ((equal exitCode 0)
	(load-file (let ((coding-system-for-read 'utf-8))
				 (shell-command-to-string agda-locate-command))))
   (t (el-log-lvl 'WARN "agda-mode unexpected error with command '%s' code %d; please check installation" agda-locate-command exitCode))))

(provide 'package-conf)
