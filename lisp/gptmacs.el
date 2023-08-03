;;; gptmacs -- GPT emacs entanglement
;;; Commentary:
;;; for use
;;; (kbd "C-M-|") -- send highlighted code along with a message
;;; (kbd "C-|")   -- send highlighted code to gpt-3 in the following format
;;;
;;; Please analyze the following code
;;;
;;; ### TEXT TO ANALYZE
;;;
;;; int main() {
;;;    // printf() displays the string inside quotation
;;;    printf("Hello\n, World!");
;;;    return 0;
;;; }

(require 'url)
(require 'el-keystore)

(el-keystore-load-keys)

(defun gptmacs-switch-to-url-buffer (status)
  "Switch to the buffer returned by `url-retreive'.
	The buffer contains the raw HTTP response sent by the server."
  (help-mode)
  (pop-to-buffer (current-buffer)))

(defvar openai-api-key (el-keystore-read-key "openai-key"))

(defun gptmacs-query-str2 (msgContent)
  "deprecated."
  (concat
   "{"
   "\"model\":\"gpt-3.5-turbo\","
   "\"messages\":[{\"role\":\"user\", \"content\":\""
   (gptmacs-escape-json-string msgContent)
   "\"}],"
   "\"temperature\":\"0.7\""
   "}"))

;; (type-of (gethash "a" (json-parse-string "{\"a\": [{\"b\": 3}]}")))
(defun gptmacs-query-str (msgContent)
  (let ((msgMap #s(hash-table size 5 test equal data ("role" "user")))
		(postMap #s(hash-table size 30 test equal data ("model" "gpt-3.5-turbo"
														"temperature" "0.7"))))
	(puthash "content" msgContent msgMap)
	(puthash "messages" (vector msgMap) postMap)
	(json-serialize postMap)))

(defun gptmacs-url-http-post (url args)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
		(url-request-extra-headers
		 `(("Content-Type" . "application/json")
		   ("Authorization" . ,(concat "Bearer " openai-api-key))))
		(url-request-data args))
	(url-retrieve url
				  (lambda (status)
					(progn
					  (help-mode)
					  (pop-to-buffer (current-buffer)))))))

(defun gptmacs-query-gpt (msgContent)
  ;; (gptmacs-url-http-post "http://localhost:8080"
  (gptmacs-url-http-post "https://api.openai.com/v1/chat/completions"
						 (gptmacs-query-str msgContent)))

;; (gptmacs-query-gpt (concat "cum all over my code" "#include <stdio.h>
;; int main() {
;;    // printf() displays the string inside quotation
;;    printf(\"Hello\\n, World!\");
;;    return 0;
;; }"))

(defun gptmacs-escape-json-string (s)
  "Escapes JSON string `S` containing code/text to be sent to GPT-3."
  ;; NOTE: this is broken; does not always produce proper JSON
  (replace-regexp-in-string "\n" "\\\\n" (replace-regexp-in-string "\\([\"\\]\\)" "\\\\\\1" s)))

(defun gptmacs-execute-on-region (start end msg)
  "Sends marked code to gpt with a message from the user.  `START` and `END` represent the region, `MSG` is the message."
  (interactive "r\nsWhat is your message?? ")
  (if (region-active-p)
	  (gptmacs-query-gpt
	   (concat msg "\n#### TEXT TO ANALYZE\n\n"
			   (buffer-substring start end)))
	(gptmacs-query-gpt msg)
	)
  )

;; TODO: don't set this globally, use a keymap like a normal person
(global-set-key (kbd "C-|") (lambda (start end) (interactive "r") (gptmacs-execute-on-region start end "Hello! Can you tell me what the following code does?")))
(global-set-key (kbd "C-M-|") 'gptmacs-execute-on-region)

(provide 'gptmacs)

;;; gptmacs.el ends here
