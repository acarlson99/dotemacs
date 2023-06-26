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
  )

(provide 'org-config)
