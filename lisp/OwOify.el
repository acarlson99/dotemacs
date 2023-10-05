;; ported (badly) from https://github.com/aqua-lzma/OwOify/blob/master/OwOify.js

(defvar OwOify-replaceWords t)
(defvar OwOify-wordMap '(("love" . "wuv")
						 ("dog" . "doggo")
						 ("cat" . "kitteh")
						 ("hello" . "henwo")
						 ("hell" . "heck")
						 ("fuck" . "fwick")
						 ("fuk" . "fwick")
						 ("shit" . "shoot")
						 ("friend" . "fwend")
						 ("stop" . "stawp")
						 ("god" . "gosh")
						 ("dick" . "peepee")
						 ("penis" . "peepee")
						 ("damn" . "darn")
						 ))
(defvar OwOify-rltow t)
(defvar OwOify-yaftern t)
(defvar OwOify-repeaty t)
(defvar OwOify-doStutter t)
(defvar OwOify-stutterChance 0.5)
(defvar OwOify-doPrefixes t)
(defvar OwOify-prefixChance 0.5)
(defvar OwOify-prefixes '("OwO" ;; TODO: literally what are these numbers
						  "OwO whats this?"
						  "*unbuttons shirt*"
						  "*nuzzles*"
						  "*waises paw*"
						  "*notices bulge*"
						  "*blushes*"
						  "*giggles*"
						  "hehe"))
(defvar OwOify-doSuffixes t)
(defvar OwOify-suffixChance 0.8)
(defvar OwOify-suffixes '("(ﾉ´ з `)ノ"
						  "( ´ ▽ ` ).｡ｏ♡"
						  "(´,,•ω•,,)♡"
						  "(*≧▽≦)"
						  "ɾ⚈▿⚈ɹ"
						  "( ﾟ∀ ﾟ)"
						  "( ・ ̫・)"
						  "( •́ .̫ •̀ )"
						  "(▰˘v˘▰)"
						  "(・ω・)"
						  "✾(〜 ☌ω☌)〜✾"
						  "(ᗒᗨᗕ)"
						  "(・`ω´・)"
						  ":3"
						  ">:3"
						  "hehe"
						  "xox"
						  ">3<"
						  "murr~"
						  "UwU"
						  "*gwomps*"
						  ))

;; TODO: rewrite this using `cl-reduce' and macros generating a lambda or `identity'
(defun OwOify (text)
  "OwOify the given text."
  (let ((text text))

	(if OwOify-replaceWords
		(setq text
			  (OwOify-replace-all text OwOify-wordMap)))
	(if OwOify-rltow
		(setq text
			  (replace-regexp-in-string
			   "[rl]"
			   (lambda (match)
				 (if (< (string-to-char match) 97)
					 "W"
				   "w"))
			   text)))
	(if OwOify-yaftern
		(setq text
			  (replace-regexp-in-string
			   "n[aeiou]"
			   (lambda (match)
				 (concat (substring match 0 1)
						 (if (< (string-to-char (substring match 1 2)) 97)
							 "Y"
						   "y")
						 (substring match 1)))
			   text)))
	(if OwOify-repeaty
		(setq text
			  (replace-regexp-in-string
			   "\\b(?=.*[aeiou])(?=[a-vx-z])[a-z]\\{4,\\}y\\b"
			   (lambda (match)
				 (concat match
						 (if (< (string-to-char (substring match 0 1)) 97)
							 "W"
						   "w")
						 (substring (string-match "[aeiouy]" match) 0)))
			   text)))
	(if OwOify-doStutter
		(setq text
			  (mapconcat
			   (lambda (word)
				 (if (or (= (length word) 0) (not (string-match "[a-z]" (substring word 0 1))))
					 word
				   (while (< (/ (random 100) 100.0) OwOify-stutterChance)
					 (setq word (concat (substring word 0 1) "-" word)))
				   word))
			   (split-string text " ")
			   " ")))
	(if OwOify-doPrefixes
		(if (< (/ (random 100) 100.0) OwOify-prefixChance)
			(setq text (concat (OwOify-weighted-random OwOify-prefixes) " " text))))
	(if OwOify-doSuffixes
		(if (< (/ (random 100) 100.0) OwOify-suffixChance)
			(setq text (concat text " " (OwOify-weighted-random OwOify-suffixes)))))
	text))

(defun OwOify-replace-all (text OwOify-wordMap)
  "Replace words in TEXT based on WORDMAP."
  (dolist (entry OwOify-wordMap)
    (setq text (replace-regexp-in-string (car entry) (cdr entry) text)))
  text)

(defun OwOify-weighted-random (list)
  (let* ((maxN (apply #'max (mapcar #'length list)))
		 (acc 0)
		 (dotted-str-weights (mapcar
							  (lambda (s)
								(cons s (setq acc (+ acc maxN (length s)))))
							  list))
		 (random (* acc (/ (random 100) 100.0))))
	(cl-some (lambda (dsw)
			   (and (< random (cdr dsw)) (car dsw)))
			 dotted-str-weights)))

;; (OwOify-weighted-random '((1 . 1) (2 . 2) (3 . 3) (4 . 4) (5 . 5)))

;; ;; Example usage:
;; (setq inputText "Hello, world! This is a test.")
;; (setq outputText (OwOify inputText))
;; (print outputText)

(provide 'OwOify)
