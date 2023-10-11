;; ported (badly) from https://github.com/aqua-lzma/OwOify/blob/master/OwOify.js

(defcustom OwOify-replaceWords t "Whether or not to replace words in wordMap."
  :type 'boolean
  :group 'OwOify)
(defcustom OwOify-wordMap '(("love" . "wuv")
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
						 )
  "Words to replace."
  :type 'list
  :group 'OwOify)
(defcustom OwOify-rltow t
  "Replace `r' and `l' with `w'."
  :type 'boolean
  :group 'OwOify)
(defcustom OwOify-yaftern t
  "Insert `y' after `n' with vowel e.g. `no' -> `nyo'."
  :type 'boolean
  :group 'OwOify)
(defcustom OwOify-repeaty t
  "broken"
  :type 'boolean
  :group 'OwOify)
(defcustom OwOify-doStutter t
  "S-S-Stutter."
  :type 'boolean
  :group 'OwOify)
(defcustom OwOify-stutterChance 0.2
  "Chance to stutter."
  :type 'float
  :group 'OwOify)
(defcustom OwOify-doPrefixes t
  "Add prefix to string."
  :type 'boolean
  :group 'OwOify)
(defcustom OwOify-prefixChance 0.2
  "Chance to add prefix to string."
  :type 'float
  :group 'OwOify)
(defcustom OwOify-prefixes '("OwO"
						  "OwO whats this?"
						  "*unbuttons shirt*"
						  "*nuzzles*"
						  "*waises paw*"
						  "*notices bulge*"
						  "*blushes*"
						  "*giggles*"
						  "hehe")
  "Prefixes to prepend to string."
  :type 'boolean
  :group 'OwOify)
(defcustom OwOify-doSuffixes t
  "Add suffix to end of string."
  :type 'boolean
  :group 'OwOify)
(defcustom OwOify-suffixChance 0.3
  "Chance to add suffix."
  :type 'boolean
  :group 'OwOify)
(defcustom OwOify-suffixes '("(ﾉ´ з `)ノ"
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
						  "(ᗒᗨᗕ)"		; cute kitty whiskers
						  "(・`ω´・)"
						  ":3"
						  ">:3"
						  "hehe"
						  "xox"
						  ">3<"
						  "murr~"
						  "UwU"
						  "*gwomps*"
						  )
  "Suffixes to add randomly."
  :type 'boolean
  :group 'OwOify)

(defun OwOify (text)
  "OwOify the given text."
  (cl-reduce (lambda (result fn)
               (let ((v (funcall fn result)))
				 (if (> (length v) 0)
					 v
				   result)))
             '(OwOify-replace-all-fn
               OwOify-rltow-fn
               OwOify-yaftern-fn
               OwOify-repeaty-fn
               OwOify-doStutter-fn
               OwOify-doPrefixes-fn
               OwOify-doSuffixes-fn
			   )
			 :initial-value text))

(defun OwOify-replace-all-fn (text)
  (if OwOify-replaceWords
      (OwOify-replace-all text OwOify-wordMap)))

(defun OwOify-rltow-fn (text)
  (if OwOify-rltow
      (replace-regexp-in-string
       "[rl]"
       (lambda (match)
         (if (< (string-to-char match) 97)
             "W"
           "w"))
       text)))

(defun OwOify-yaftern-fn (text)
  (if OwOify-yaftern
      (replace-regexp-in-string
       "n[aeiou]"
       (lambda (match)
         (concat (substring match 0 1)
                 (if (< (string-to-char (substring match 1 2)) 97)
                     "Y"
                   "y")
                 (substring match 1)))
       text)))

(defun OwOify-repeaty-fn (text)
  (if OwOify-repeaty
      (replace-regexp-in-string
       "\\b(?=.*[aeiou])(?=[a-vx-z])[a-z]\\{4,\\}y\\b"
       (lambda (match)
         (concat match
                 (if (< (string-to-char (substring match 0 1)) 97)
                     "W"
                   "w")
                 (substring (string-match "[aeiouy]" match) 0)))
       text)))

(defun OwOify-doStutter-fn (text)
  (if OwOify-doStutter
      (mapconcat
       (lambda (word)
         (if (or (= (length word) 0) (not (string-match "[a-z]" (substring word 0 1))))
             word
           (while (< (/ (random 100) 100.0) OwOify-stutterChance)
             (setq word (concat (substring word 0 1) "-" word)))
           word))
       (split-string text " ")
       " ")))

(defun OwOify-doPrefixes-fn (text)
  (if OwOify-doPrefixes
      (if (< (/ (random 100) 100.0) OwOify-prefixChance)
          (concat (OwOify-weighted-random OwOify-prefixes) " " text))))

(defun OwOify-doSuffixes-fn (text)
  (if OwOify-doSuffixes
      (if (< (/ (random 100) 100.0) OwOify-suffixChance)
          (concat text " " (OwOify-weighted-random OwOify-suffixes)))))

(require 'regexp-opt)

;; Helper function to replace OwOify-replace-all
(defun OwOify-replace-all (text wordMap)
  (replace-regexp-in-string
   (regexp-opt (mapcar 'car wordMap) t)
   (lambda (match)
	 (replace-regexp-in-string match
							   (cdr (assoc (downcase match) wordMap))
							   match))
   text))

;; Helper function to implement OwOify-weighted-random
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
