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
(defvar owoify-prefixChance 0.2)
(defvar owoify-prefixes '(("OwO" . 9) ;; TODO: literally what are these numbers
						  ("OwO whats this?" . 1)
						  ("*unbuttons shirt*" . 2)
						  ("*nuzzles*" . 3)
						  ("*waises paw*" . 4)
						  ("*notices bulge*" . 5)
						  ("*blushes*" . 6)
						  ("*giggles*" . 7)
						  ("hehe" . 8)))
(defvar owoify-doSuffixes t)
(defvar owoify-suffixChance 0.1)
(defvar owoify-suffixes '(("(ﾉ´ з `)ノ" . 1)
						  ("( ´ ▽ ` ).｡ｏ♡" . 1)
						  ("(´,,•ω•,,)♡" . 1)
						  ("(*≧▽≦)" . 1)
						  ("ɾ⚈▿⚈ɹ" . 1)
						  ("( ﾟ∀ ﾟ)" . 1)
						  ("( ・ ̫・)" . 1)
						  ("( •́ .̫ •̀ )" . 1)
						  ("(▰˘v˘▰)" . 1)
						  ("(・ω・)" . 1)
						  ("✾(〜 ☌ω☌)〜✾" . 1)
						  ("(ᗒᗨᗕ)" . 1)
						  ("(・`ω´・)" . 1)
						  (":3" . 1)
						  (">:3" . 1)
						  ("hehe" . 1)
						  ("xox" . 1)
						  (">3<" . 1)
						  ("murr~" . 1)
						  ("UwU" . 1)
						  ("*gwomps*" . 1)
						  ))

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
				   (while (< (random 1.0) owoify-stutterChance)
					 (setq word (concat (substring word 0 1) "-" word)))
				   word))
			   (split-string text " ")
			   " ")))
	(if owoify-doPrefixes
		(if (< (random 1.0) owoify-prefixChance)
			(setq text (concat (owo-weighted-random owoify-prefixes) " " text))))
	(if owoify-doSuffixes
		(if (< (random 1.0) owoify-suffixChance)
			(setq text (concat text " " (owo-weighted-random owoify-suffixes)))))
	text))

(defun owo-replace-all (text owoify-wordMap)
  "Replace words in TEXT based on WORDMAP."
  (dolist (entry owoify-wordMap)
    (setq text (replace-regexp-in-string (car entry) (cdr entry) text)))
  text)

(defun owo-weighted-random (list)
  "Return a randomly selected element from LIST with weighted probability."
  (let ((total-weight (apply '+ (mapcar 'cdr list))))
    (let ((rand-weight (random total-weight))
		  (current-weight 0))
	  ;; eval to first expr such that `(>= current-weight rand-weight)`
	  (cl-some
	   (lambda (item)
		 (setq current-weight (+ current-weight (cdr item)))
		 (if (>= current-weight rand-weight)
			 (car item)))
	   list))))

;; (owo-weighted-random '((1 . 1) (2 . 2) (3 . 3) (4 . 4) (5 . 5)))

;; ;; Example usage:
;; (setq inputText "Hello, world! This is a test.")
;; (setq outputText (owoify inputText))
;; (print outputText)

(provide 'owoify)
