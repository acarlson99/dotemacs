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

;; Overengineered unoptimized argparse library.
;; Likely powerful enough to be a full parser, but probably not a very fast one.

;;; Code:

(cl-defstruct argparse-opt "" name longopt shortopt has-arg arg-type)

;; (require 'cl-extra)

;; (defmacro num-lambda-args (l)
;;   `(let ((clj ,l))
;; 	 (print (car l))
;; 	 (cl-assert (equal (car clj) 'lambda))
;; 	 (cl-assert (listp (cadr clj)))
;; 	 (length (cadr clj))))

(defun num-lambda-args (lmb)
  (cl-assert (equal (car lmb) 'lambda))
  (length (cadr lmb)))

;; parser creation macro
;; create a parser which accepts a continuation
;; tail-call optimized even though it doesn't matter (not supported by elisp)
(defmacro arg-parser-with-continuation (f)
  `(lambda (opts args &optional continuation rargs)
	 (let ((n-consumed ,(- (num-lambda-args f) 1)))
	   (if (< (length args) n-consumed)
		   (append (list rargs) (list args))
		 (let ((result (cl-some
						(lambda (opt)
						  (apply ,f opt
								 (take n-consumed args)))
						opts)))
		   (if result
			   (let ((next-args (nthcdr n-consumed args)))
				 (if continuation
					 (funcall continuation opts next-args continuation (append rargs result))
				   (append (list (append rargs result)) (list next-args))))
			 (append (list rargs) (list args))))))))

(defun largest-list (lss)
  (car (seq-sort (lambda (a b) (> (length (car a)) (length (car b)))) lss)))

(defun argp-parse-longopt (opts args)
  (let* (
										; '--flag' 'val'
		 (with-sep-arg (arg-parser-with-continuation
						(lambda (opt arg1 arg2)
						  (if (and
							   (argparse-opt-has-arg opt)
							   (string= "--" (substring arg1 0 2))
							   (string= (concat "--" (argparse-opt-longopt opt)) arg1))
							  (list (append (list (argparse-opt-name opt)) arg2))))))
										; '--flag=val'
		 (with-eq (arg-parser-with-continuation
				   (lambda (opt arg1)
					 (let ((arg-eq-str (concat "--" (argparse-opt-longopt opt) "=")))
					   (if (and
							(argparse-opt-has-arg opt)
							(<= (length arg-eq-str) (length arg1))
							(string= arg-eq-str (substring arg1 0 (length arg-eq-str))))
						   (list (append (list (argparse-opt-name opt)) (substring arg1 (length arg-eq-str)))))))))
										; '--flag-with-no-arg'
		 (with-no-arg (arg-parser-with-continuation
					   (lambda (opt arg1)
						 (if (and
							  (not (argparse-opt-has-arg opt))
							  (string= "--" (substring arg1 0 2))
							  (string= (concat "--" (argparse-opt-longopt opt)) arg1))
							 (list (list (argparse-opt-name opt)))))))
		 (short-sep-arg (arg-parser-with-continuation
						 (lambda (opt arg1 arg2)
						   (if (and
								(argparse-opt-has-arg opt)
								(string= arg1 (concat "-" (argparse-opt-shortopt opt))))
							   (list (append (list (argparse-opt-name opt)) arg2))))))
		 (short-eq-arg (arg-parser-with-continuation
						(lambda (opt arg1)
						  (let ((arg-eq-str (concat "-" (argparse-opt-shortopt opt) "=")))
							(if (and
								 (argparse-opt-has-arg opt)
								 (<= (length arg-eq-str) (length arg1))
								 (string= arg-eq-str (substring arg1 0 (length arg-eq-str))))
								(list (append (list (argparse-opt-name opt)) (substring arg1 (length arg-eq-str)))))))))
		 (short-arg-no-sep (arg-parser-with-continuation
						(lambda (opt arg1)
						  (let ((arg-str (concat "-" (argparse-opt-shortopt opt))))
							(if (and
								 (argparse-opt-has-arg opt)
								 (<= (length arg-str) (length arg1))
								 (string= arg-str (substring arg1 0 (length arg-str))))
								(list (append (list (argparse-opt-name opt)) (substring arg1 (length arg-str)))))))))
		 (short-no-arg (arg-parser-with-continuation
						(lambda (opt arg1)
						  (if (and
							   (not (argparse-opt-has-arg opt))
							   (string= "-" (substring arg1 0 1))
							   (string= (concat "-" (argparse-opt-shortopt opt)) arg1))
							  (list (list (argparse-opt-name opt)))))))
		 (f (lambda (opts args &optional continuation rargs)
			  (largest-list (mapcar (lambda (fn) (funcall fn opts args continuation rargs))
									(list with-sep-arg with-eq with-no-arg
										  short-sep-arg short-eq-arg
										  short-arg-no-sep short-no-arg))))))
	(funcall f opts args f nil)))

(defvar opt-1
  (make-argparse-opt :name "filename" :longopt "file" :shortopt "f" :has-arg t :arg-type :string))
(defvar opt-2
  (make-argparse-opt :name "argless-flag" :longopt "no-arg" :shortopt "a" :has-arg nil))
(defvar opt-3
  (make-argparse-opt :name "arg-flag" :longopt "arg" :shortopt "g" :has-arg t))
(defvar g-opts (list opt-1 opt-2 opt-3))

(argp-parse-longopt g-opts '("--arg" "fdsa" "--arg=fadssdaf" "--file" "file-val" "--no-arg" "other-arg"))

(argp-parse-longopt g-opts '("--file" "bar"))
(argp-parse-longopt g-opts '("--file" "bar" "baz"))
(argp-parse-longopt g-opts '("--file=foobar" "--file=bar-foo" "--file" "bar" "--no-arg"))

(let ((my-opts (list
				(make-argparse-opt :name "filename" :longopt "file" :shortopt "f" :has-arg t :arg-type :string)
				(make-argparse-opt :name "use-static-naming" :longopt "static" :shortopt "s" :has-arg nil :arg-type :string)
				(make-argparse-opt :name "port" :longopt "port" :shortopt "p" :has-arg t :arg-type :string))))
  (argp-parse-longopt my-opts '("--static" "-p" "8080" "-p=8080" "-s" "-p8080" "--" "--static" "rest-args")))

;; (((use-static-naming) (port . 8080) (port . 8080) (use-static-naming) (port . 8080)) (-- --static rest-args))

(provide 'argparse)
;;; argparse.el ends here

;; (_argparse_rec '("--file" "abc" "--file=abc"))
