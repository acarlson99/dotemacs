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
;; Honstly it's more of a `getopt' but `argparse' is a good name so ¯\_(ツ)_/¯

;;; Code:

(require 'cl-macs)

(cl-defstruct argparse-opt
  "\
NAME: how your code will reference this option
LONGOPT: long `--flag' syntax; `nil' to unset
SHORTOPT: short `-f' syntax; `nil' to unset
HAS-ARG: whether this option should expect an argument (e.g. `--flag=arg')
ARG-TYPE: 'int 'str 'float"
  name longopt shortopt has-arg arg-type)

(defun argparse--num-lambda-args (lmb)
  (cl-assert (equal (car lmb) 'lambda))
  (length (cadr lmb)))

(defmacro argparse-parser-with-continuation (f)
  "\
Create a tail-call optimized (unsupported in elisp) parser which accepts a
continuation.
Returns a function of form (lambda (opts args &optional continuation rargs) ...)

F: lambda which acts as the body of the created parser.  This cannot be a
function, it MUST be a lambda.

Parser consumes as many tokens as the function takes arguments.
Specifically this uses the `argparse--num-lambda-args` function which does not
properly handle `&optional' or `&rest' arguments.  In cases where a function
with optional arguments is required please use the following form:
  (let ((parse-func (lambda (a b &optional c d)
					  ...)))
	(argparse-parser-with-continuation (lambda (a b) (parse-func a b))))
"
  `(lambda (opts args &optional continuation rargs)
	 (let ((n-consumed ,(- (argparse--num-lambda-args f) 1)))
	   (if (< (length args) n-consumed)
		   (append (list rargs) (list args))
		 (let* ((result (cl-some
						 (lambda (opt)
						   (let ((res (apply ,f opt
											 (take n-consumed args))))
							 (cl-case (and res (argparse-opt-has-arg opt) (argparse-opt-arg-type opt))
							   (str res)
							   (int (list (append (list (caar res)) (round (string-to-number (cdar res))))))
							   (float (list (append (list (caar res)) (string-to-number (cdar res)))))
							   (t res)
							   )))
						 opts)))
		   (if result
			   (let ((next-args (nthcdr n-consumed args)))
				 (if continuation
					 (funcall continuation opts next-args continuation (append rargs result))
				   (append (list (append rargs result)) (list next-args))))
			 (append (list rargs) (list args))))))))

(defun argparse--largest-list (lss)
  (car (seq-sort (lambda (a b) (> (length (car a)) (length (car b)))) lss)))

(defun argparse-getopt (opts args)
  "\
OPTS: list of `argparse-opt'
ARGS: list of strings, usually `argv'
"
  (let* (
										; '--flag' 'val'
		 (with-sep-arg (argparse-parser-with-continuation
						(lambda (opt arg1 arg2)
						  (if (and
							   (argparse-opt-longopt opt)
							   (argparse-opt-has-arg opt)
							   (string= "--" (substring arg1 0 2))
							   (string= (concat "--" (argparse-opt-longopt opt)) arg1))
							  (list (append (list (argparse-opt-name opt)) arg2))))))
										; '--flag=val'
		 (with-eq (argparse-parser-with-continuation
				   (lambda (opt arg1)
					 (let ((arg-eq-str (concat "--" (argparse-opt-longopt opt) "=")))
					   (if (and
							(argparse-opt-longopt opt)
							(argparse-opt-has-arg opt)
							(<= (length arg-eq-str) (length arg1))
							(string= arg-eq-str (substring arg1 0 (length arg-eq-str))))
						   (list (append (list (argparse-opt-name opt)) (substring arg1 (length arg-eq-str)))))))))
										; '--flag-with-no-arg'
		 (with-no-arg (argparse-parser-with-continuation
					   (lambda (opt arg1)
						 (if (and
							  (argparse-opt-longopt opt)
							  (not (argparse-opt-has-arg opt))
							  (string= "--" (substring arg1 0 2))
							  (string= (concat "--" (argparse-opt-longopt opt)) arg1))
							 (list (list (argparse-opt-name opt)))))))
										; '-f' 'filename'
		 (short-sep-arg (argparse-parser-with-continuation
						 (lambda (opt arg1 arg2)
						   (if (and
								(argparse-opt-shortopt opt)
								(argparse-opt-has-arg opt)
								(string= arg1 (concat "-" (argparse-opt-shortopt opt))))
							   (list (append (list (argparse-opt-name opt)) arg2))))))
										; '-f=filename'
		 (short-eq-arg (argparse-parser-with-continuation
						(lambda (opt arg1)
						  (let ((arg-eq-str (concat "-" (argparse-opt-shortopt opt) "=")))
							(if (and
								 (argparse-opt-shortopt opt)
								 (argparse-opt-has-arg opt)
								 (<= (length arg-eq-str) (length arg1))
								 (string= arg-eq-str (substring arg1 0 (length arg-eq-str))))
								(list (append (list (argparse-opt-name opt)) (substring arg1 (length arg-eq-str)))))))))
										; '-ffilename'
		 (short-arg-no-sep (argparse-parser-with-continuation
							(lambda (opt arg1)
							  (let ((arg-str (concat "-" (argparse-opt-shortopt opt))))
								(if (and
									 (argparse-opt-shortopt opt)
									 (argparse-opt-has-arg opt)
									 (<= (length arg-str) (length arg1))
									 (string= arg-str (substring arg1 0 (length arg-str))))
									(list (append (list (argparse-opt-name opt)) (substring arg1 (length arg-str)))))))))
										; '-l'
		 (short-no-arg (argparse-parser-with-continuation
						(lambda (opt arg1)
						  (if (and
							   (argparse-opt-shortopt opt)
							   (not (argparse-opt-has-arg opt))
							   (string= "-" (substring arg1 0 1))
							   (string= (concat "-" (argparse-opt-shortopt opt)) arg1))
							  (list (list (argparse-opt-name opt)))))))
		 (f (lambda (opts args &optional continuation rargs)
			  (argparse--largest-list (mapcar (lambda (fn) (funcall fn opts args continuation rargs))
											  (list with-sep-arg with-eq with-no-arg
													short-sep-arg short-eq-arg
													short-arg-no-sep short-no-arg))))))
	(funcall f opts args f nil)))

(let ((g-opts
	   (list (make-argparse-opt :name "filename" :longopt "file" :shortopt "f" :has-arg t :arg-type 'str)
			 (make-argparse-opt :name "argless-flag" :longopt "no-arg" :shortopt "a" :has-arg nil)
			 (make-argparse-opt :name "arg-flag" :longopt "arg" :has-arg t)
			 (make-argparse-opt :name "nolong" :shortopt "s" :has-arg t)
			 (make-argparse-opt :name "mini" :shortopt "m")
			 )))

  (cl-assert (equal
			  (argparse-getopt g-opts '("--arg" "fdsa" "--arg=fadssdaf" "--file" "file-val" "--no-arg" "other-arg"))
			  '((("arg-flag" . "fdsa") ("arg-flag" . "fadssdaf") ("filename" . "file-val") ("argless-flag")) ("other-arg"))))
  (cl-assert (equal
			  (argparse-getopt g-opts '("--file" "bar"))
			  '((("filename" . "bar")) nil)))
  (cl-assert (equal
			  (argparse-getopt g-opts '("--file" "bar" "baz"))
			  '((("filename" . "bar")) ("baz"))))
  (cl-assert (equal
			  (argparse-getopt g-opts '("--file=foobar" "--file=bar-foo" "--file" "bar" "--no-arg"))
			  '((("filename" . "foobar") ("filename" . "bar-foo") ("filename" . "bar") ("argless-flag")) nil)))
  (cl-assert (equal
			  (argparse-getopt g-opts '("-m" "-s" "arg" "-s--arg-2" "--=3"))
			  '((("mini") ("nolong" . "arg") ("nolong" . "--arg-2")) ("--=3")))))

(let ((my-opts (list
				(make-argparse-opt :name "filename" :longopt "file" :shortopt "f" :has-arg t :arg-type 'str)
				(make-argparse-opt :name "use-static-naming" :longopt "static" :shortopt "s" :has-arg nil :arg-type 'str)
				(make-argparse-opt :name "float" :longopt "flt" :shortopt "n" :has-arg t :arg-type 'float)
				(make-argparse-opt :name "floatless" :longopt "fls" :shortopt "i" :has-arg nil :arg-type 'float)
				(make-argparse-opt :name "port" :longopt "port" :shortopt "p" :has-arg t :arg-type 'int))))
  (cl-assert (equal
			  (argparse-getopt my-opts '("--static" "-p" "8080" "-p=8080" "-s" "-p8080" "--" "--static" "rest-args"))
			  '((("use-static-naming") ("port" . 8080) ("port" . 8080) ("use-static-naming") ("port" . 8080)) ("--" "--static" "rest-args"))))
  (cl-assert (equal
			  (argparse-getopt my-opts '("--flt" "22.22" "-p22.22" "--fls" "12.3"))
			  '((("float" . 22.22) ("port" . 22) ("floatless")) ("12.3"))))
  )

(defun argparse--index-of (e l &optional cmp_)
  (letrec ((cmp (or cmp_ #'eq))
		   (rf (lambda (l n)
				 (if l
					 (if (funcall (or cmp #'eq) e (car l))
						 n
					   (funcall
						rf
						(cdr l)
						(+ (or n 0) 1)))
				   nil))))
	(funcall rf l 0)))

(defun argparse-scan-argv (element args)
  "\
ELEMENT: token to search for
ARGS: args to search through
RETURNS: list after `ELEMENT' or `nil'

e.g.
(argparse-scan-argv '--
					'(-f main -- serve -p 8080 -d /tmp/files))
=> (-- serve -p 8080 -d /tmp/files)
"
  (let* ((n (argparse--index-of element args #'string=)))
	(if n
		(nthcdr n args))))

(defun argparse-get-arg (k arglist)
  "\
K: key to search for
ARGLIST: list of key/value pairs of form ((A . B) (C . D))

(argparse-get-arg 'A '((C . D) (A . B)))
=> (A . B)
"
  (assoc k arglist #'string=))

(let ((parsed-args (car (argparse-getopt
						 (list
						  (make-argparse-opt :name "hostname" :longopt "host" :shortopt "h" :has-arg t :arg-type 'str)
						  (make-argparse-opt :name "port" :longopt "port" :shortopt "p" :has-arg t :arg-type 'int)
						  (make-argparse-opt :name "dir" :longopt "dir" :shortopt "d" :has-arg t :arg-type 'str))
						 '("--port=8080" "-d" "/tmp")))))
  (cl-assert (equal
			  (argparse-get-arg "port" parsed-args)
			  '("port" . 8080)))
  (cl-assert (equal
			  (argparse-get-arg "other" parsed-args)
			  nil))
  (cl-assert (equal
			  (argparse-get-arg "dir" parsed-args)
			  '("dir" . "/tmp"))))

(provide 'argparse)
;;; argparse.el ends here
