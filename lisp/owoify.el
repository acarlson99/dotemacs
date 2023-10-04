;; ported (badly) from https://github.com/aqua-lzma/OwOify/blob/master/owoify.js

(defvar owoify-replaceWords t)
(defvar owoify-wordMap '(("love" . "wuv")
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
(defvar owoify-rltow t)
(defvar owoify-yaftern t)
(defvar owoify-repeaty t)
(defvar owoify-doStutter t)
(defvar owoify-stutterChance 0.5)
(defvar owoify-doPrefixes t)
(defvar owoify-prefixChance 0.5)
(defvar owoify-prefixes '("OwO" ;; TODO: literally what are these numbers
						  "OwO whats this?"
						  "*unbuttons shirt*"
						  "*nuzzles*"
						  "*waises paw*"
						  "*notices bulge*"
						  "*blushes*"
						  "*giggles*"
						  "hehe"))
(defvar owoify-doSuffixes t)
(defvar owoify-suffixChance 0.8)
(defvar owoify-suffixes '("(ﾉ´ з `)ノ"
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
(defun owoify (text)
  "OwOify the given text."
  (let ((text text))

	(if owoify-replaceWords
		(setq text
			  (owo-replace-all text owoify-wordMap)))
	(if owoify-rltow
		(setq text
			  (replace-regexp-in-string
			   "[rl]"
			   (lambda (match)
				 (if (< (string-to-char match) 97)
					 "W"
				   "w"))
			   text)))
	(if owoify-yaftern
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
	(if owoify-repeaty
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
	(if owoify-doStutter
		(setq text
			  (mapconcat
			   (lambda (word)
				 (if (or (= (length word) 0) (not (string-match "[a-z]" (substring word 0 1))))
					 word
				   (while (< (/ (random 100) 100.0) owoify-stutterChance)
					 (setq word (concat (substring word 0 1) "-" word)))
				   word))
			   (split-string text " ")
			   " ")))
	(if owoify-doPrefixes
		(if (< (/ (random 100) 100.0) owoify-prefixChance)
			(setq text (concat (owo-weighted-random owoify-prefixes) " " text))))
	(if owoify-doSuffixes
		(if (< (/ (random 100) 100.0) owoify-suffixChance)
			(setq text (concat text " " (owo-weighted-random owoify-suffixes)))))
	text))

(defun owo-replace-all (text owoify-wordMap)
  "Replace words in TEXT based on WORDMAP."
  (dolist (entry owoify-wordMap)
    (setq text (replace-regexp-in-string (car entry) (cdr entry) text)))
  text)

(defun owo-weighted-random (list)
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

;; (owo-weighted-random '((1 . 1) (2 . 2) (3 . 3) (4 . 4) (5 . 5)))

;; ;; Example usage:
;; (setq inputText "Hello, world! This is a test.")
;; (setq outputText (owoify inputText))
;; (print outputText)

(provide 'owoify)
