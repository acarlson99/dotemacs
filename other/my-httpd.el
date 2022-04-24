;; λ

(require 'simple-httpd)
(httpd-start)
(setq httpd-host "localhost")

(cl-defmacro with-tag (div args &rest body)
  ""
  `(concat
	"<" (symbol-name ,div)
	,@(mapcar (lambda (x) (format " %s=\"%s\"" (car x) (cadr x))) args)
	">"
	,@body
	"</" (symbol-name ,div) ">"))

;; <div class="cum,on" id="me,mommy" href="./yee.html" src="/tmp/img.png">
;; </div>

(defvar counter 0)
(defservlet test-tag text/html ()
  (progn
	(setq counter (1+ counter))
	(insert
	 (with-tag 'div (("src" "./style.css")
					 ("style" "color: red; font-size: 20px;"))
			   (with-tag 'p nil "this is text")
			   (with-tag 'p nil (format "%d" counter))
			   "yeet"))))

(defservlet* test-smth text/plain (body)
  (if body
	  (insert body)
	(insert "no body")))

(defun foldl (fn start it)
  "Fold FN over IT starting at START."
  (cond ((equalp nil it) start)
		(t (foldl fn (funcall fn start (car it)) (cdr it)))))

(defun foldr (fn start it)
  (foldl fn start (reverse it)))

(defun split (lst)
  (foldl (lambda (acc x)
		   (let ((a (car acc))
				 (b (cadr acc)))
			 (cond ((= (length a) (length b)) (list (cons x a) b))
				   (t (list a (cons x b))))))
		 '(()())
		 lst))

(provide 'my/httpd)
