;;; el-keystore.el --- store API keys                -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar el-keystore-key-storage-file (mapconcat 'identity `(,user-emacs-directory ".el-keystored-keys.el") ""))

(defvar el-keystore-keylist nil
  "Database of completion history.")

(defun el-keystore-make-keylist ()
  (make-hash-table :test 'equal))

(defun el-keystore-reset-keylist ()
  (setq el-keystore-keylist (make-hash-table :test 'equal)))

(defun el-keystore-store-key (name val)
  (interactive "sName: \nsVal: ")
  (puthash name val el-keystore-keylist)
  )

(defun el-keystore-read-key (name)
  "Read stored key NAME."
  (interactive "sName: ")
  (gethash name el-keystore-keylist))

(defun el-keystore-serialize (db)
  "No documentation, DB."
  (let (alist)
	(maphash (lambda (k v) (push (cons k v) alist)) db)
	alist))

(defun el-keystore-save-keys (&optional fname) ;; dump to file
  "Save keys in FNAME or default to el-keystore-key-storage-file."
  (interactive)
  (require 'pp)
  (ignore-errors
	(with-temp-buffer
	  (insert ";; DO NOT ADD TO SOURCE CONTROL\n")
	  (pp (el-keystore-serialize el-keystore-keylist) (current-buffer))
	  (write-region (point-min) (point-max) (or fname el-keystore-key-storage-file)))))

(defun el-keystore-deserialize (sexp)
  "No documentation, SEXP."
  (condition-case nil
	  (or (let ((tab (el-keystore-make-keylist)))
			(mapc (lambda (cons)
					(puthash (car cons) (cdr cons) tab))
				  sexp)
			tab)
		  (el-keystore-make-keylist))
	(error (message "Invalid comphist db.") nil)))

(defun el-keystore-load-keys (&optional fname) ;; dump from file
  "Read keys from FNAME or default to el-keystore-key-storage-file."
  (interactive)
  (let* ((file (or fname el-keystore-key-storage-file))
		(db (if (file-exists-p file)
				(ignore-errors
				  (with-temp-buffer
					(insert-file-contents file)
					(goto-char (point-min))
					(el-keystore-deserialize (read (current-buffer))))))))
	(setq el-keystore-keylist (or db (make-hash-table :test 'equal)))))

;; (progn
;;   (el-keystore-reset-keylist)
;;   (el-keystore-store-key "cum" "sock")
;;   (el-keystore-store-key "munchkin" "giggity")
;;   (el-keystore-save-keys)
;;   (el-keystore-load-keys)
;;   (el-keystore-read-key "cum")
;;   (el-keystore-read-key "munchkin")
;;   )

(provide 'el-keystore)
;;; el-keystore.el ends here
