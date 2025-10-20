;;; vj.el --- VJ a rave or something with emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2025  chumshack

;; Author: chumshack <john@chumshack>
;; Keywords: games, multimedia

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

(defvar vj-buf-name "*pattern*")

(defun vj-init ()
  (setq show-trailing-whitespace nil)
  (nlinum-mode 0)
  (scroll-bar-mode 0)
  (setq gc-cons-threshold (* 100 1024 1024)))

(defun vj-cls ()
  (message "brother just use (erase-buffer)")
  (erase-buffer))

(defun vj-write (&rest args)
  (apply #'insert args))

(defun vj-resolution ()
  ;; TODO: this is the resolution of the current active buffer, not the vj buffer
  (with-current-buffer (get-buffer-create vj-buf-name)
	`(
	  ,(- (window-total-width) 10)		; nlinum, scrollbar margins
;;	  ,(- (window-total-width) 3)		; nlinum, scrollbar margins
	  .
	  ,(- (window-total-height) 3)
	  )))

(defun vj-strs-to-str (strl)
  (string-join (mapcar #'string-join strl) "\n"))

(defun vj-blit (&rest ss)
  (vj-blit-2 (vj-strs-to-str ss)))

(defun vj-blit-2 (s)
  (progn
	(erase-buffer)
	(vj-write s)))

(defvar vj-chars
  '())

(defun drop-first-and-last (l)
  (drop 1 (reverse (drop 1 (reverse l)))))

(defun stol (s)
  (let ((l (split-string s "")))
	;; drop first and last
	(drop-first-and-last l)))

(defun ltos (l)
  (string-join l))

(defun mirror (l &optional dfal)
  (if dfal
	  (append l (drop-first-and-last (reverse l)))
	(append l (reverse l))))

(defun list-repeat (l &optional n)
  (cond
   ((eq nil n) (append l l))
   ((< n 1) l)
   ('t (append l (list-repeat l (- n 1))))))

(defvar vj--timer nil)

(defvar float-pi 3.1415722653589)

(defun fract (n) (- n (floor n)))

;; (defvar vj-shader
;;   (lambda (x y w h time)
;; 	(let ((x (float x))
;; 		  (y (float y))
;; 		  (w (float w))
;; 		  (h (float h))
;; 		  (time (float time)))
;; 	  ;;	  (let ((chars (stol "EMACS"))
;; 	  (let* ((chars (stol "iloveemacs"))
;; 			 ;; (uvx (/ x w))
;; 			 ;; (uvy (/ y h))
;; 			 (uvzx (/ (- x (* w .5)) h))
;; 			 (uvzy (/ (- y (* h .5)) h))
;; 			 (wav (+ .5 (* .5 (* (sin (* float-pi uvzx 2)) (sin (* float-pi uvzy 4))))))
;; 			 (wav2 (+ .5 (* .5 (* (sin (* float-pi uvzx 2)))) uvzy)))
;; 		(let* ((letter (car (nthcdr (mod (round
;; 										 (+ time (* (length chars)
;; 										;							  (+ 0.5 (* 0.5 (sin (+ (* (max (abs uvzx) (abs uvzy)) float-pi) (- time)))))
;; 													wav2
;; 													)))
;; 										(length chars))
;; 								   chars)))
;; 			  (txt (propertize letter 'face '(:foreground "red" :background "white"))))
;; 		  (if (< .5 (+ .5 (* .5 (sin (+ time (* wav 4 float-pi))))))
;; 			  txt
;; 			(upcase txt)))))))

(defun triangle-wave (x)
  (abs (- (* (fract x) 2) 1)))

;; this one is pretty good
;; (defvar vj-shader
;;   (lambda (x y w h time)
;; 	(let ((x (float x))
;; 		  (y (float y))
;; 		  (w (float w))
;; 		  (h (float h))
;; 		  (time (float time)))
;; 	  ;;	  (let ((chars (stol "EMACS"))
;; 	  (let* ((chars (stol "i love emacs"))
;; 			 ;; (uvx (/ x w))
;; 			 ;; (uvy (/ y h))
;; 			 (uvzx (/ (- x (* w .5)) h))
;; 			 (uvzy (/ (- y (* h .5)) h))
;; 			 (wav (+ .5 (* .5 (+
;; 							   (sin
;; 								(+ (* -1 time) (* (sin (+ (* -2 (abs uvzy)))) float-pi -3)
;; 								   (* float-pi (abs uvzx) 1)))))))
;; 			 (wav2 (+ .5 (* .5 (* (sin (+ (* float-pi (+ (* 1 (abs uvzx)) (* 2 (abs uvzy))) -1)))))))
;; 			 (wav3 (* (- 1 wav) wav2))
;; 			 (wav4 (* wav wav2)))
;; 		(let* ((letter (car (nthcdr (mod (round
;; 										  (+ time (* (length chars)
;; 										;							  (+ 0.5 (* 0.5 (sin (+ (* (max (abs uvzx) (abs uvzy)) float-pi) (- time)))))
;; 													 wav3
;; 													 )))
;; 										 (length chars))
;; 									chars)))
;; 			   (txt (cond
;; 					 ((< .5 wav) (propertize letter 'face '(:foreground "maroon")))
;; 					 (t
;; 					  (propertize letter 'face '(:foreground "cyan" :background "maroon"))))))
;; 		  (if (< .5 wav4)
;; 			  (propertize txt 'face '(:foreground "cyan"))
;; 			(upcase txt)))))))

(defun vec-length (x y)
  (sqrt (+ (expt x 2) (expt y 2))))

(defun dot2 (v)
  "Compute the dot product of a 2D vector."
  (+ (* (car v) (car v)) (* (cadr v) (cadr v))))

(defun sign (x)
  (if (< 0 x) -1 1))

;; this one is a little wrong idk
;; (defun sd-heart (p)
;;   "Compute the signed distance function for a heart shape."
;;   (let ((x (abs (car p)))
;;         (y (cadr p)))
;;     (if (> (+ x y) 1.0)
;;         (- (sqrt (dot2 (list (- x 0.25) (- y 0.75)))) (/ (sqrt 2.0) 4.0))
;;       (* (sqrt (min (dot2 (list (- x 0.0) (- y 1.0)))
;; 					(dot2 (list (- x (* 0.5 (max (+ x y) 0.)))
;; 								(- y (* 0.5 (max (+ x y) 0.)))))))
;; 		 (sign (- x y))))))

;; good enough
(defun sd-heart (p)
  (let* ((x (car p))
		(y (+ (abs x) (cadr p))))
	(- (vec-length x y) 1.)))

;;(sd-heart '(0 0))

(defun sin-zero-one (p)
  (+ .5 (* .5 (sin p))))

;; WIP hearts
(defvar vj-shader
  (lambda (x y w h time)
    (let ((x (float x))
		  (y (float y))
		  (w (float w))
		  (h (float h))
		  (time (float time)))
      (let* ((s "peace and love and ")
			 (chars (reverse (stol s)))
			 (uvzx (/ (- x (* w .5)) h))
			 (uvzy (/ (- (* y 1.5) (* h .65)) h))
			 (wav (fract (+
						  (sd-heart (list uvzx (- uvzy)))
						  (* time -.3))))
			 (wav2 (fract (+
						   (sd-heart (list uvzx (- uvzy)))
						   (* time -.2)
						   (* .2 (sign uvzx) (sign uvzy)))))
			 (wav3 (fract (+
						   (sd-heart
							(vec-rotate (list uvzx uvzy) (* float-pi (* .15 (- time (vec-length uvzx uvzy))))))
						   (* time -.25))))
			 (letter (car (nthcdr (mod (round (* (length chars) wav3)) (length chars)) chars)))
			 (fg (if
					 (< (sin (- (* float-pi 10. wav) (/ float-pi 2))) 0.)
					 "magenta"
				   "cyan"))
			 (bg (if
					 (< (sin (- (* float-pi 1. wav2) (/ float-pi 2))) 0.)
					 "red"
				   "white")))
		(propertize letter 'face `(:foreground ,fg :background ,bg))))))

(defun vec-rotate (p th)
  "rotate vector (x,y) by theta"
  (let ((c (cos th))
        (s (sin th))
		(x (car p))
		(y (cadr p)))
    (list
     (- (* x c) (* y s))
     (+ (* x s) (* y c)))))


;; WIP high-res with braille
;; (defvar vj-shader
;;   (lambda (x y w h time)
;; 	(let* ((empty "⠀")
;; 		  (top "⠁⠈⠉")
;; 		  (mid1  "⠂⠃⠊⠋⠐⠑⠒⠓⠘⠙⠚⠛")
;; 		  (mid2 (concat "⠄⠅⠆⠇⠌⠍⠎⠏⠔⠕⠖⠗⠜⠝⠞⠟"
;;               "⠠⠡⠢⠣⠤⠥⠦⠧⠨⠩⠪⠫⠬⠭⠮⠯"
;;               "⠰⠱⠲⠳⠴⠵⠶⠷⠸⠹⠺⠻⠼⠽⠾⠿"))
;; 		  (bot "⣗⣿")
;; 		  (charlist (list empty top mid1 mid2 bot))
;; 		  (x (float x))
;; 		  (y (float y))
;; 		  (w (float w))
;; 		  (h (float h))
;; 		  (time (float time)))
;; 	  (car (reverse (stol (car (nthcdr
;; 					   (mod (floor (+ time (sin y) (sin x))) (length charlist))
;; 					   charlist))))))))
;;	  (car (stol bot)))))

(defun vj--shader-render (f time)
  (let* ((res (vj-resolution))
		 (width (car res))
		 (height (cdr res))
		 (ll '()))
	(cl-loop for y from height downto 0 do
			 (let ((l '()))
			   (cl-loop for x from 0 to width do
						(setq l (append l (list (funcall f x y width height time)))))
			   (setq ll (append ll (list l)))))
	ll))

(let ((l (vj--shader-render vj-shader 0)))
  l)
;  (string-join (mapcar #'ltos l) "\n"))

(defvar vj-tick-rate 0.05)

(defvar vj--msg-i 0)
(defvar vj--msg-mode 0.0)

(defun vj--message (s)
  (let* ((width 80)
		 (longS (list-repeat (stol s) (+ 1 (/ width (length s)))))
		 (i (mod vj--msg-i (length s)))
		 (rs (take width (drop i longS)))
		 (rsh (take (/ width 2) (drop i longS)))
		 (hs (append (reverse rsh) rsh))
		 (rhs (append rsh (reverse rsh)))
		 (msg-i (* vj--msg-i 0.25)))
    (setq vj--msg-i (+ vj--msg-i (+ 1)))
	(cond
	 ((< (mod msg-i 4) 1) (ltos rs))
	 ((< (mod msg-i 4) 2) (ltos (reverse rs)))
	 ;; ((< (mod msg-i 4) 3) (ltos hs))
	 ((< (mod msg-i 4) 4) (ltos rhs)))))

(defvar vj--inhibit-message nil)

(defun vj--tick (time)
  (let* ((res (vj-resolution))
		 (width (car res))
		 (height (cdr res))
		 (strs (vj--shader-render vj-shader time)))
	(with-current-buffer (get-buffer-create vj-buf-name)
	  (with-silent-modifications
		(let ((inhibit-modification-hooks t)
			  (inhibit-read-only t)
			  (inhibit-redisplay t))
		  (erase-buffer)
		  (apply #'vj-blit strs)
		  (redisplay)))
	  (setq vj--timer
			(run-with-timer vj-tick-rate nil #'vj--tick (+ time vj-tick-rate)))
	  ;; TODO: check if M-x menu is active.
	  ;; Until then this will remain super annoying.
	  (unless vj--inhibit-message (message "%s" (vj--message "❤❤♥"))))))

(defun vj--tick-old ()
  (let* ((res (vj-resolution))
		 (width (car res))
		 (height (cdr res)))
	(setq vj-strs (append (drop 1 vj-strs) (take 1 vj-strs)))
	(with-current-buffer (get-buffer-create vj-buf-name)
	  (let ((inhibit-read-only t))
		(erase-buffer)
		(apply #'vj-blit vj-strs)))
	(setq vj--timer
		  (run-with-timer 0.5 nil #'vj--tick-old))))

(defun vj-start ()
  (interactive)
  (when vj--timer (cancel-timer vj--timer))
  (let ((switch-to-buffer-p (eq nil (get-buffer vj-buf-name))))
	(with-current-buffer (get-buffer-create vj-buf-name)
      (read-only-mode 1)
	  (if switch-to-buffer-p (switch-to-buffer (current-buffer)))
	  )
	(vj--tick 0)))

(defun vj-stop ()
  (interactive)
  (when vj--timer
	(cancel-timer vj--timer)
	(setq vj--timer nil)
	(message "alright, that's enough")))

(defun pattern-start () (interactive) (vj-start))
(defun pattern-stop () (interactive) (vj-stop))

(provide 'vj)
;;; vj.el ends here

;; minimum nylon opinion
