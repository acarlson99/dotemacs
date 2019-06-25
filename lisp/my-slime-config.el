(cond
 ((file-exists-p "/usr/bin/sbcl")
  (setq inferior-lisp-program "/usr/bin/sbcl"))
 ((file-exists-p "~/.brew/bin/sbcl")
  (setq inferior-lisp-program "~/.brew/bin/sbcl")))
