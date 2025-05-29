;;; mod-init.el ---                                  -*- lexical-binding: t; -*-

;; initializes modules

(require 'el-log)
(require 'my-defs)

(let ((mod "ob-glsl-module")
	  (repo-link "https://github.com/finalpatch/ob-glsl/"))
  (unless (load (concat (file-name-directory (el-current-file)) mod) t)
	(el-log-lvl 'WARN "unable to load module %s; perhaps visit %s" mod repo-link)))

(provide 'mod-init)
;;; mod-init.el ends here
