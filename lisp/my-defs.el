;;; my-defs.el --- little funcs
;;; Commentary:
;;; Code:

(defmacro curry (fun &rest args)
  "Curry curries FUN with ARGS."
  `(lambda (&rest args2)
    (apply ,fun (append '(,@args) args2))))

(provide 'my-defs)
;;; my-defs.el ends here
