(when (require 'slime nil 'noerror)
  (progn
	(cond
	 ((file-exists-p "/usr/bin/sbcl")
	  (setq inferior-lisp-program "/usr/bin/sbcl"))
	 ((file-exists-p "~/.brew/bin/sbcl")
	  (setq inferior-lisp-program "~/.brew/bin/sbcl")))

	(setq slime-contribs '(slime-fancy))))
