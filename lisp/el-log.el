;;; el-log.el --- simple logging                     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Alexander Carlson

;; Author: Alexander Carlson
;; Keywords: convenience, maint

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

(defvar el-log-levels '(INFO WARN ERROR FATAL)
  "Levels to display.
Logging to a loglvl not in this list produces no output")

(defvar el-log-level-default 'INFO "Default value assigned to el-log calls if not specified.")

(defmacro el-log-msg-prefix (loglvl)
  "Prefix for log messages.  This can be customized"
  `(format "%s [%s] %s:%d -- "
		   (current-time-string)
		   (symbol-name ,loglvl)
		   ;; TODO: FIX when run on init this produces "[INFO] *scratch*:1 -- "
		   (or load-file-name (buffer-file-name) (buffer-name))
		   (line-number-at-pos)))

(defmacro el-log-lvl (loglvl format-string &rest args)
  `(let ((loglvl (or ,loglvl el-log-level-default)))
	 (if (member loglvl el-log-levels)
		 (message "%s"
				  (string-join (list (el-log-msg-prefix loglvl)
							   (cl-reduce
								(lambda (s fn)
								  (funcall fn s))
								el-log-middleware
								:initial-value (format ,format-string ,@args))))))))
								   ;; (string-join (list (el-log-msg-prefix loglvl) ,format-string))
								   ;; ,@args))))))

(defmacro el-log (format-string &rest args)
  `(el-log-lvl el-log-level-default ,format-string ,@args))

(defvar el-log-middleware '()
  "intermediary string functions to run after message is formatted.
Usually just silly.")

;; (require 'OwOify)
;; (add-to-list 'el-log-middleware #'OwOify)

(provide 'el-log)
;;; el-log.el ends here
