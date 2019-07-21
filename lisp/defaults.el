(let ((l '("\
;;                              /~~~~~~~~~~~~\\_
;;          _+=+_             _[~  /~~~~~~~~~~~~\\_
;;         {\"\"|\"\"}         [~~~    [~   /~~~~~~~~~\\_
;;          \"\"\":-'~[~[~\"~[~  ((++     [~  _/~~~~~~~~\\_
;;               '=_   [    ,==, ((++    [    /~~~~~~~\\-~~~-.
;;                  ~-_ _=+-(   )/   ((++  .~~~.[~~~~(  {@} \\`.
;;                          /   }\\ /     (     }     (   .   ''}
;;                         (  .+   \\ /  //     )    / .,  \"\"\"\"/
;;                         \\\\  \\     \\ (   .+~~\\_  /.= /'\"\"\"\"
;;        -r.millward-     <\"_V_\">      \\\\  \\    ~~~~~~\\\\  \\
;;                                       \\\\  \\          \\\\  \\
;;                                       <\"_V_\">        <\"_V_\">
;; Ankylosaur
;;
;;
;;              \" Clankity, clankity, clankity clank!
;;                Ankylosaurus was built like a tank.
;;                Its hide was a fortress sturdy as steel,
;;                It tended to be an inedible meal. \"
;;                                       -- Jack Prelutsky
"
		   "\
;;                        (
;;                          )     (
;;                   ___...(-------)-....___
;;               .-\"\"       )    (          \"\"-.
;;         .-'``'|-._             )         _.-|
;;        /  .--.|   `\"\"---...........---\"\"`   |
;;       /  /    |                             |
;;       |  |    |                             |
;;        \\  \\   |                             |
;;         `\\ `\\ |                             |
;;           `\\ `|                             |
;;           _/ /\\                             /
;;          (__/  \\                           /
;;       _..---\"\"` \\                         /`\"\"---.._
;;    .-'           \\                       /          '-.
;;   :               `-.__             __.-'              :
;;   :                  ) \"\"---...---\"\" (                 :
;;    '._               `\"--...___...--\"`              _.'
;;      \\\"\"--..__                              __..--\"\"/
;;       '._     \"\"\"----.....______.....----\"\"\"     _.'
;;          `\"\"--..,,_____            _____,,..--\"\"`
;;                        `\"\"\"----\"\"\"`
"
		   "\
;;                                                        ..       :
;;                     .                  .               .   .  .
;;       .           .                .               .. .  .  *
;;              *          .                    ..        .
;;                            .             .     . :  .   .    .  .
;;             .                         .   .  .  .   .
;;                                          . .  *:. . .
;; .                                 .  .   . .. .         .
;;                          .     . .  . ...    .    .
;;        .              .  .  . .    . .  . .
;;                         .    .     . ...   ..   .       .               .
;;                  .  .    . *.   . .
;;     .                   :.  .           .
;;                  .   .    .    .
;;              .  .  .    ./|\\
;;             .  .. :.    . |             .               .
;;      .   ... .            |
;;  .    :.  . .   *.        |     .               .
;;    .  *.             You are here.
;;  . .    .               .             *.                         .
")))
  (setq initial-scratch-message (nth (random (length l)) l)))

;; Turn on evil mode if it is installed
(when (require 'evil nil 'noerror)
  (evil-mode 1)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'neotree-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'comint-mode 'emacs)
  (evil-set-initial-state 'slime-repl-mode 'emacs)

  (when (require 'evil-numbers nil 'noerror)
	(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
	(define-key evil-normal-state-map (kbd "C-q") 'evil-numbers/dec-at-pt)))

;; Set auto-complete-mode settings if installed
(when (require 'auto-complete nil 'noerror)
  (ac-config-default)
  (setq ac-use-menu-map t)
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

;; Make sure backup directory exists
(if (not (file-directory-p "~/.emacs.d/backups"))
	(make-directory "~/.emacs.d/backups"))

;; Redirects emacs backups
(setq backup-directory-alist
	  `((".*" . , "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
	  `((".*" , "~/.emacs.d/backups" t)))

;; Purges files not accessed in a week
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
	  (current (float-time (current-time))))
  (dolist (file (directory-files "~/.emacs.d/backups" t))
	(when (and (backup-file-name-p file)
			   (> (- current (float-time (nth 5 (file-attributes file))))
				  week))
	  (message "%s" file)
	  (delete-file file))))

;; Theme
(load-theme 'manoj-dark t)

;; Line numbers
(global-nlinum-mode 1)

;; Preset `nlinum-format' for minimum width.
(defun my-nlinum-mode-hook ()
  "Recommended nlinum-mode function for nlinum-mode."
  (when nlinum-mode
	(setq-local nlinum-format
				(concat "%" (number-to-string
							 ;; Guesstimate number of buffer lines.
							 (ceiling (log (max 1 (/ (buffer-size) 80)) 10)))
						"d"))))
(add-hook 'nlinum-mode-hook #'my-nlinum-mode-hook)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable scroll bar and toolbar in GUI
(if (window-system)
	(progn
	  (tool-bar-mode -1)
	  (scroll-bar-mode -1)))

;; Set line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Make search case sensetive
(setq-default case-fold-search nil)

;; Highlight current line
(global-hl-line-mode 1)
(set-face-foreground 'hl-line nil)

;; Line wrap
(global-visual-line-mode 1)

;; Disable annoying bell
(setq ring-bell-function 'ignore)

;; auto-mode-alist file extension stuff

;; glsl
(when (require 'glsl-mode nil 'noerror)
  (autoload 'glsl-mode "glsl-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode)))

;; Assembly
(when (require 'nasm-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode))
  (add-to-list 'auto-mode-alist '("\\.s\\'" . nasm-mode)))

;; Web stuff
(when (require 'web-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

;; projectile project management
(when (require 'projectile nil 'noerror)
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/projects/"))
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; append shell (SHELL) path to path and exec-path
(let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
  (setenv "PATH" (concat (getenv "PATH") ":" path-from-shell))
  (setq exec-path (append exec-path (split-string path-from-shell path-separator))))

;; end
