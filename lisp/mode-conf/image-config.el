;;; image-config.el ---                              -*- lexical-binding: t; -*-

(defun my/reload-image ()
  (interactive "")
  (find-file (el-current-file)))

(defun my/image-mode-hook ()
  (local-set-key (kbd "r") 'my/reload-image))

(provide 'image-config)
;;; image-config.el ends here
