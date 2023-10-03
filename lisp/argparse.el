;;; argparse.el --- argparse                         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  misterbuttstuff

;; Author: misterbuttstuff <john@misterbuttstuff>
;; Keywords: internal, lisp, c, unix, wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'cl)

(cl-defstruct argparse-opt "" name longopt shortopt has-arg arg-type)
  ;; ((name :initarg name :accessor argparse-name :initform "")
  ;;  (longopt :initarg longopt :accessor argparse-longopt :initform nil)
  ;;  (shortopt :initarg shortopt :accessor argparse-shortopt :initform nil)
  ;;  (has-arg :initarg has-arg :accessor argparse-has-arg :initform nil)
  ;;  (arg-type :initarg arg-type :accessor argparse-arg-type :initform nil)
  ;;  ))

(defvar opt-1
  (make-argparse-opt :name "filename" :longopt "file" :shortopt "f" :has-arg t :arg-type :string))
(defvar opt-2
  (make-argparse-opt :name "argless-flag" :longopt "no-arg" :shortopt "a" :has-arg nil))
(defvar opt-3
  (make-argparse-opt :name "arg-flag" :longopt "arg" :shortopt "g" :has-arg t))
(defvar g-opts (list opt-1 opt-2 opt-3))

(defun argp-parse---foo\ bar (opt args &optional continuation rargs)
  (if (< (length args) 2)
	  rargs
	;; (let ((opt opt-1)
	;; 	  (args '("--file" "bar"))
	;; 	  (rargs nil)
	;; 	  (continuation nil))
	(let ((arg1 (car args))
		  (arg2 (cadr args))
		  (rest (cddr args)))
	  (if (and
		   (argparse-opt-has-arg opt)
		   (string= "--" (substring arg1 0 2))
		   (string= (concat "--" (argparse-opt-longopt opt)) arg1))
		  (let ((next-rargs
				 (append rargs
						 (list (append (list (argparse-opt-name opt)) arg2)))))
			(if continuation
				(funcall continuation opt rest continuation next-rargs)
			  next-rargs))
		rargs))))

(argp-parse---foo\ bar opt-1 '("--file" "thisisafilename") nil '(("a" . "b")))

(require 'cl-extra)

;; parser creation macro
;; create parser with continuation with which to continue parse
(defmacro arg-parser-with-continuation (args-f f next-args-f)
  `(lambda (opts args &optional continuation rargs)
	 (if (< (length args) ,(- (length args-f) 1))
		 (append (list rargs) (list args))
	   (let ((result (cl-some (lambda (opt) (apply ,f opt (mapcar (lambda (fn) (funcall fn args)) ,args-f))) opts)))
		 (if result
			 (let ((next-args (funcall ,next-args-f args)))
			   (if continuation
				   (funcall continuation opts next-args continuation (append rargs result))
				 (append (list (append rargs result)) (list next-args))))
		   (append (list rargs) (list args)))))))

;; (mapcar (lambda (op) (funcall op '(1 2 3))) '(car cadr))

(defun compose (funcs)
  "composes several funcitons into one"
  (lexical-let ((funcs funcs))
    (lambda (arg)
      (if funcs
          (funcall (car funcs) (funcall (compose (cdr funcs)) arg))
        arg))))

(let* (
										; '--flag' 'val'
	   (with-sep-arg (arg-parser-with-continuation
								   (list #'car #'cadr)
								   (lambda (opt arg1 arg2)
									 (if (and
										  (argparse-opt-has-arg opt)
										  (string= "--" (substring arg1 0 2))
										  (string= (concat "--" (argparse-opt-longopt opt)) arg1))
										 (list (append (list (argparse-opt-name opt)) arg2))))
								   #'cddr))
										; '--flag=val'
	   (with-eq (arg-parser-with-continuation
				 (list #'car)
				 (lambda (opt arg1)
				   (let ((arg-eq-str (concat "--" (argparse-opt-longopt opt) "=")))
					 (if (and
						  (<= (length arg-eq-str) (length arg1))
						  (string= arg-eq-str (substring arg1 0 (length arg-eq-str))))
						 (progn
						   (list (append (list (argparse-opt-name opt)) (substring arg1 (length arg-eq-str))))
						   ))))
				 #'cdr))
										; '--flag-with-no-arg'
	   (with-no-arg (arg-parser-with-continuation
					 (list #'car)
					 (lambda (opt arg1)
					   (if (and
							(not (argparse-opt-has-arg opt))
							(string= "--" (substring arg1 0 2))
							(string= (concat "--" (argparse-opt-longopt opt)) arg1))
						   (list (list (argparse-opt-name opt)))))
					 #'cdr))
	   (short-sep-arg (arg-parser-with-continuation
				   (list (car args) ;; TODO: remove `args' and just pass `(list car cadr)'
						 (cadr args))
				   (lambda (opt arg1 arg2)
					 nil
					 )
				   (cddr args))) ;; TODO: remove `args', pass only `cddr'.
	   (f (lambda (opts args &optional continuation rargs)
			(largest-list (mapcar (lambda (fn) (funcall fn opts args continuation rargs)) (list with-sep-arg with-eq with-no-arg))))))
  (funcall f g-opts '("--arg" "fdsa" "--arg=fadssdaf" "--file" "file-val" "--no-arg" "other-arg") f))

(defun argp-parse---foo=bar (opt args &optional continuation rargs)
  (if (< (length args) 1)
	  rargs
	(let ((arg1 (car args))
		  (rest (cdr args)))
	  (if (and
		   (argparse-opt-has-arg opt)
		   (string= "--" (substring arg1 0 2)))
		  (let ((arg-eq-str (concat "--" (argparse-opt-longopt opt) "=")))
			(if (and
				 (<= (length arg-eq-str) (length arg1))
				 (string= arg-eq-str (substring arg1 0 (length arg-eq-str))))
				(let ((next-rargs (append rargs (list (append (list (argparse-opt-name opt)) (substring arg1 (length arg-eq-str)))))))
				  (if continuation
					  (funcall continuation opt rest continuation next-rargs)
					next-rargs))
			  rargs))
		rargs))))

(argp-parse---foo=bar opt-1 '("--file=this is my file" "--file=another") #'argp-parse---foo=bar '(("a" . "b")))

(defun argp-parse---foo (opt args &optional continuation rargs)
  (if (< (length args) 1)
	  rargs
	(let ((arg1 (car args))
		  (rest (cdr args)))
	  (if (and
		   (not (argparse-opt-has-arg opt))
		   (string= "--" (substring arg1 0 2))
		   (string= (concat "--" (argparse-opt-longopt opt)) arg1))
		  (let ((next-rargs (append rargs (list (list (argparse-opt-name opt))))))
			(if continuation
				(funcall continuation opt rest continuation next-rargs)
			  next-rargs))
		rargs))))

(funcall #'argp-parse---foo opt-2 '("--no-arg"))

(defun largest-list (lss)
  (car (seq-sort (lambda (a b) (> (length (car a)) (length (car b)))) lss)))

(defun argp-parse-longopt (opts args &optional continuation rargs)
  (if (< (length args) 1)
	  rargs
	(let* ((longfs '(
					 argp-parse---foo\ bar
					 argp-parse---foo=bar
					 argp-parse---foo
					 ))
		   (parses (mapcar (lambda (fn)
							 (largest-list (mapcar (lambda (opt)
													 (funcall fn opt args
															  (lambda (_ args &optional continuation rargs)
																(funcall #'argp-parse-longopt opts args continuation rargs))
															  rargs))
												   opts)))
						   longfs)))
	  (largest-list parses))))

(argp-parse-longopt opts '("--file" "bar"))
(argp-parse-longopt opts '("--file=foobar" "--file=bar-foo" "--file" "bar" "--no-arg"))

(defun _argparse_rec (opt ls)
  (if (equal ls nil) nil)
  (let ((s (car ls))
		(rest (cdr ls)))
	(cond
	 ((string= "--" (substring s 0 2))
	  (progn
		(message "yesSSSS")
		(if (string= (concat "--" (argparse-opt-longopt opt)) s)
			(if (argparse-opt-has-arg opt)
				;; --abc def
				(append (list (append (list s) (car rest))) (_argparse_rec (cdr rest)))
			  ;; --abc
			  (append (list s) (_argparse_rec rest)))
		  (let ((sstr (concat (argparse-opt-longopt opt) "=")))
			(if (string= sstr (substring s 0 (length sstr)))
				;; --abc=def
				(append (list (append (list s) (substring s (length sstr)))) (_argparse_rec rest))
			  (append (list s) (_argparse_rec rest)))))))
	 ((string= "-" (substring s 0 1))
										; -s       ;; single
										; -it      ;; two single flas combined
										; -s=value
										; -s value
										; -svalue
	  )
	 (t (append nil s)))))

(defun argparse (ls)
  (_argparse_rec ls))

(provide 'argparse)
;;; argparse.el ends here

;; (_argparse_rec '("--file" "abc" "--file=abc"))
