;;; package --- Summary
;;; Commentary:
;;; Code:

(defun my/fix-inline-images ()
  (when org-inline-image-overlays
	(org-redisplay-inline-images)))

(defun my/org-unsafe-babel ()
  (setq org-confirm-babel-evaluate nil))

(defun org-config ()
  "For use in `org-mode-hook'."
  (hl-todo-mode t)
  (setq-local indent-tabs-mode nil)
  (font-lock-mode t)
  ;; do NOT uncomment; this exists as a reminder
  ;; (setq org-confirm-babel-evaluate nil)
  (add-hook 'org-babel-after-execute-hook 'my/fix-inline-images)

  (use-package htmlize
	:defer t
	:config
	;; Disable fci-mode around htmlize so its indicator chars don't get exported
	(with-eval-after-load 'fill-column-indicator
      (defvar my/htmlize-was-fci nil
		"Whether `fci-mode` was on before `htmlize-buffer` ran.")
      (defun my/htmlize-before-disable-fci ()
		(setq my/htmlize-was-fci fci-mode)
		(when fci-mode (fci-mode -1)))
      (defun my/htmlize-after-restore-fci ()
		(when my/htmlize-was-fci
          (fci-mode 1)))

      (add-hook 'htmlize-before-hook #'my/htmlize-before-disable-fci)
      (add-hook 'htmlize-after-hook  #'my/htmlize-after-restore-fci))

	;; Similarly, disable flyspell to avoid markup errors in the export
	(with-eval-after-load 'flyspell
      (defvar my/htmlize-was-flyspell nil
		"Whether `flyspell-mode` was on before `htmlize-buffer` ran.")
      (defun my/htmlize-before-disable-flyspell ()
		(setq my/htmlize-was-flyspell flyspell-mode)
		(when flyspell-mode (flyspell-mode -1)))
      (defun my/htmlize-after-restore-flyspell ()
		(when my/htmlize-was-flyspell
          (flyspell-mode 1)))

      (add-hook 'htmlize-before-hook #'my/htmlize-before-disable-flyspell)
      (add-hook 'htmlize-after-hook  #'my/htmlize-after-restore-flyspell)))
  )

(provide 'org-config)
