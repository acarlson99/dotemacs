;;; my-defs.el --- little funcs                      -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; for emacs versions <29
(unless (fboundp 'take)
  (defun take (n l)
    (cond
     ((> n 0) (cons (car l) (take (- n 1) (cdr l))))
     ((= n 0) nil)
     (t (error "invalid argument for take: %d" n)))))

;; Text

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
	(if (> (point) (mark))
		(exchange-point-and-mark))
	(let ((column (current-column))
		  (text (delete-and-extract-region (point) (mark))))
	  (forward-line arg)
	  (move-to-column column t)
	  (set-mark (point))
	  (insert text)
	  (exchange-point-and-mark)
	  (setq deactivate-mark nil)))
   (t
	(beginning-of-line)
	(when (or (> arg 0) (not (bobp)))
	  (forward-line)
	  (when (or (< arg 0) (not (eobp)))
		(transpose-lines arg))
	  (forward-line -1)))))

;; Move text up and down
(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line ARG lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line ARG lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun back-window ()
  "Switch to previous window."
  (interactive)
  (other-window -1))

;; Functional

(defmacro curry (fun &rest args)
  "Curry curries FUN with ARGS."
  `(lambda (&rest args2)
	 (apply ,fun (append '(,@args) args2))))

(defun compose (funcs)
  "composes several funcitons into one"
  (let ((funcs_ funcs))
    (lambda (arg)
      (if funcs_
          (funcall (car funcs_) (funcall (compose (cdr funcs_)) arg))
        arg))))

(mapc
 (lambda (funcs)
   (let ((compose-funcs (car funcs))
		 (test-func (cdr funcs)))
	 (cl-assert (equal
				 (funcall (compose compose-funcs) '(1 2 3 4 5))
				 (funcall test-func '(1 2 3 4 5))))))
 '(((car cdr cdr) . caddr)
   ((identity identity cdr identity identity) . cdr)))

;; (defun drop (N LIST)
;;   (if (eq N 0)
;; 	  LIST
;; 	(drop (- N 1) (cdr LIST))))

(defun drop (N LIST)
  (nthcdr N LIST))

(cl-assert (not (drop 3 '(1 2 3))))
(cl-assert (equal (drop 3 '(1 2 3 4)) '(4)))
(let ((ls '(0 1 2 3 4 5)))
  (mapc (lambda (n)
		  (cl-assert (equal (append (take n ls) (drop n ls)) ls)))
		ls))

;; NOTE: lambdas expand to closures which have different syntax
;; Generally use `num-closure-args' unless you are using macros
;; (macroexpand (lambda (x) x))
;; => (closure (t) (x) x)
(defun num-lambda-args (lmb)
  "You may want `num-closure-args'."
  (cl-assert (equal (car lmb) 'lambda))
  (length (cadr lmb)))

(defun num-closure-args (clj)
  "You may want `num-lambda-args'."
  (cl-assert (equal (car clj) 'closure))
  (length (caddr clj)))

;; (num-lambda-args '(lambda (x)))
;; => 1
;; (defmacro test-macro-1 (l)
;;   `(+ 0 ,(num-lambda-args l)))
;; (test-macro-1 (lambda (x)))
;; => 1
;; (defmacro test-macro-2 (l)
;;   `(+ 0 (num-closure-args ,l)))
;; (test-macro (lambda (x)))
;; => 1
;; (num-closure-args (lambda (x)))
;; => 1

;; Misc.

(require 'subr-x)
(require 'el-log)

(defun prune-backups ()
  "Set backup directory to `~/.emacs.d/backups' and clear directory weekly"
  (progn
	(let ((backupdir (string-join (list user-emacs-directory "backups") ""))
		  (wipefile (string-join (list user-emacs-directory ".lastIwiped") "")))

	  ;; Make sure backup directory exists
	  (if (not (file-directory-p backupdir))
		  (make-directory backupdir))

	  ;; Redirects emacs backups
	  (setq backup-directory-alist
			`((".*" . , backupdir)))
	  (setq auto-save-file-name-transforms
			`((".*" , backupdir)))

	  (let ((last-run-time-file wipefile)
			(week (* 60 60 24 7)))
		(if (not (file-exists-p last-run-time-file))
			(write-region (number-to-string (float-time (current-time))) nil last-run-time-file))
		(let ((last-wipe-time (with-temp-buffer
								(insert-file-contents last-run-time-file)
								(string-to-number (buffer-string))))
			  (current (float-time (current-time))))
		  (if (> current (+ last-wipe-time week))
			  (progn
				;; Purges files not accessed in a week
				(el-log "Deleting old backup files...")
				(dolist (file
						 (mapcar
						  (lambda (f) (string-join (list backupdir f) "/"))
						  (directory-files backupdir)))
				  (when (and (backup-file-name-p file)
							 (> (- current (float-time (nth 5 (file-attributes file))))
								week))
					(el-log "Delete %s" file)
					(delete-file file)))
				(write-region (number-to-string (float-time (current-time))) nil last-run-time-file)
				t)
			(el-log "No wipe; waiting %d seconds" (- (+ last-wipe-time week) current))))))))

(defun url-to-file (filepath url)
  "Save content at URL to FILEPATH."
  (with-current-buffer
	  (url-retrieve-synchronously url)
	(write-file filepath)))

(require 'org)

(defvar my/org-copy-visible-trunc-replacement " ... ")

;; yoinked from org-copy-visible
;; purpose is to replace invisible sections with truncated text e.g. "<...>"
(defun my/org-copy-visible (beg end)
  "Copy the visible parts of the region."
  (interactive "r")
  (let ((result ""))
    (while (/= beg end)
      (if (eq org-fold-core-style 'text-properties)
          (progn
            (while (org-invisible-p beg)
			  (setq beg (org-fold-next-visibility-change beg end)))
            (let ((next (org-fold-next-visibility-change beg end)))
			  (setq result (concat result (buffer-substring beg next)))
			  (unless (= next end)
				(setq result (concat result my/org-copy-visible-trunc-replacement)))
			  (setq beg next)))
        (when (invisible-p beg)
		  (setq beg (next-single-char-property-change beg 'invisible nil end)))
        (let ((next (next-single-char-property-change beg 'invisible nil end)))
		  (setq result (concat result (buffer-substring beg next)))
		  (setq beg next))))
    ;; Prevent Emacs from adding full selected text to `kill-ring'
    ;; when `select-enable-primary' is non-nil.  This special value of
    ;; `deactivate-mark' only works since Emacs 29.
    (setq deactivate-mark 'dont-save)
    (kill-new result)
    (message "Visible strings have been copied to the kill ring.")))

(provide 'my-defs)
;;; my-defs.el ends here
