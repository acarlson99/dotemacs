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

(defclass argparse-opt ()
  ((name :initarg name :accessor argparse-name :initform "")
   (longopt :initarg longopt :accessor argparse-longopt :initform nil)
   (shortopt :initarg shortopt :accessor argparse-shortopt :initform nil)
   (has-arg :initarg has-arg :accessor argparse-has-arg :initform nil)
   (arg-type :initarg arg-type :accessor argparse-arg-type :initform nil)
   ))

(defun _argparse_rec (ls)
  (if (equal ls nil) nil)
  (let ((s (car ls))
		(rest (cdr ls)))
	(cond
	 ((string= "--" (substring s 0 2))
	  (progn
		(if (string= (argparse-longopt opt) s)
			(if (argparse-has-arg opt)
				;; --abc def
				(append (list (append (list s) (car rest))) (_argparse_rec (cdr rest)))
			  ;; --abc
			  (append (list s) (_argparse_rec rest)))
		  (let ((sstr (concat (argparse-longopt opt) "=")))
			(if (string= sstr (substring s 0 (length sstr)))
				;; --abc=def
				(append (list (append (list s) (substring s (length sstr)))) (_argparse_rec rest)))))
		nil))
	  ((string= "-" (substring s 0 1))
										; -s       ;; single
										; -it      ;; two single flas combined
										; -s=value
										; -s value
										; -svalue
	   )
	  (t (nil . s)))))

(defun argparse (ls)
  (_argparse_rec ls))

(provide 'argparse)
;;; argparse.el ends here

(defvar opt
  (make-argparse-opt :name "filename" :longopt "--file" :shortopt "-f" :has-arg t :arg-type :string))
(defvar opts (list opt))

(_argparse_rec '("--file" "abc" "-fabc"))
