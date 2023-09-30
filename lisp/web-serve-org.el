;; chumshack -- Fri Sep 29 18:53:41 2023
;;           ,’          __  ‘.
;;          /|          " __   \
;;         , |           / |.   .
;;        |,’          !_.’|   |
;;       ,’             ’   |   |
;;      /              |’--’|   |
;;    |                ’---’   |
;;      .   ,                   |                       ,".
;;       ._     ’           _’  |                    , ’ \ ‘
;;   ‘.. ‘.‘-...___,...---""    |       __,.        ,‘"   L,|
;;  |, ‘- .‘._        _,-,.’   .  __.-’-. /        .   ,    \
;; -:..     ‘. ‘-..--_.,.<       ‘"      / ‘.        ‘-/ |   .
;;   ‘,         """"’     ‘.              ,’         |   |  ’,,
;;     ‘.      ’            ’            /          ’    |’. |/
;;       ‘.   |              \       _,-’           |       ’’
;;         ’._’               \   ’"\                .      |
;;           |                ’     \                ‘._  ,’
;;           |                 ’     \                 .’|
;;           |                 .      \                | |
;;           |                 |       L              ,’ |
;;            ‘                 |       |             /   ’
;;             \                |       |           ,’   /
;;           ,’ \               |  _.._ ,-..___,..-’    ,’
;;          /     .             .      ‘!             ,j’
;;         /       ‘.          /        .           .’/
;;        .          ‘.       /         |        _.’.’
;;         ‘.          7‘’---’          |------"’_.’
;;        _,.‘,_     _’                ,’’-----"’
;;    _,-_    ’       ‘.     .’      ,\
;;    -" /‘.         _,’     | _  _  _.|
;;     ""--’---"""""’        ‘’ ’! |! /

(require 'subr-x)

;; from https://hungyi.net/posts/dead-simple-emacs-lisp-templating/
(defun charge-html (&rest template)
  "Turns a list of TEMPLATE s-exps (tag :attr value ...content) into HTML."
  (let (tag attr-name (content (list)) (attrs (list)))
    (mapc
     (lambda (x)
       (cond ((and x (listp x))
              (push (apply #'charge-html x) content))
             ((and (not tag) x (symbolp x))
              (setq tag x))
             ((keywordp x)
              (setq attr-name x))
             (attr-name
              (push (cons attr-name x) attrs)
              (setq attr-name nil))
             (t
              (unless (null x) (push (format "%s" x) content)))))
     template)
    (let ((tag-is-void
           (memq tag
                 '(area base br col embed
                        hr img input link meta
                        param track wbr))))
      (concat
       (when tag
         (thread-last
           attrs
           (nreverse)
           (mapcar
            (lambda (attr)
              (format
               (if (cdr attr) " %s=\"%s\"" " %s")
               (substring (symbol-name (car attr)) 1) (cdr attr))))
           (apply #'concat)
           (format
            (if tag-is-void "<%s%s/>" "<%s%s>")
            tag)))
       (unless tag-is-void
         (thread-last
           content
           (nreverse)
           (apply #'concat)))
       (when (and tag (not tag-is-void))
         (format "</%s>" tag))))))

;; TODO: add UTF-8 charset enforcement
(defvar js-embed-string "
function sendContent() {
  // Get the content from the textarea
  const content = document.getElementById('editable-content').innerText;

  // Create a FormData object
  const formData = new FormData();
  
  // Append the content to the FormData object
  formData.append('content', content);

  // Create a new XMLHttpRequest object
  const xhr = new XMLHttpRequest();

  // Define the POST request to 'localhost:9002'
  xhr.open('POST', 'http://localhost:9002'+filepath, true);

  // Send the FormData object
  xhr.send(formData);

  // Handle the response (you can add your own logic here)
  xhr.onreadystatechange = function () {
    if (xhr.readyState === 4) {
      if (xhr.status === 200) {
        alert('Content sent successfully.');
      } else {
        alert('Error sending content; code '+String(xhr.status));
      }
    }
  };
}
")

(defun make-html (filepath content)
  (charge-html
   `(html
	 (head
	  (title ,(format "Edit %S" filepath))
	  (meta :charset "ISO-8859-1") ;; TODO: verify that this enforces UTF-8-only
	  (script ,(format "var filepath = %S;" filepath))
	  (script ,(format "var content = `%s`;" content))
	  (script "window.onload = function(){ document.getElementById('editable-content').innerText = content; };")
	  (script ,js-embed-string))
	 (body
	  (h1 ,(format "Edit %S" filepath))
	  (p :contenteditable nil :id editable-content)
	  ;; (p :contenteditable nil :id editable-content ,content)
	  (button :onclick "sendContent()" "Upload")
	  ))))


(require 'url)
(require 'web-server)
(require 'org)
(setq org-confirm-babel-evaluate nil)

;; (setq docroot "/home/john/p/dota-draml")
(setq docroot "/tmp/test")

;; (ws-start
;;  '(((:POST . ".*") .
;;     (lambda (request)
;;       (with-slots (process headers) request
;;         (let ((message (cdr (assoc "message" headers))))
;;           (ws-response-header process 200 '("Content-type" . "text/plain"))
;;           (process-send-string process
;;             (if message
;;                 (format "you said %S\n" (cdr (assoc 'content message)))
;;               "This is a POST request, but it has no \"message\".\n"))))))
;;    ((:GET . ".*") .
;;     (lambda (request)
;;       (with-slots (process) request
;;         (ws-response-header process 200 '("Content-type" . "text/plain"))
;;         (process-send-string process
;;           "This is a GET request not a POST request.\n")))))
;;  9005)

(defun org-server (request)
  (with-slots (process headers) request
	(let* ((path (ws-in-directory-p ; check if path is in docroot
				  docroot (substring (cdr (assoc :GET headers)) 1)))
		   (base (file-name-sans-extension path))
		   (orig (concat base ".org")))
	  (unless path (ws-send-404 process)) ; send 404 if not in docroot
	  (cond
	   ((file-directory-p path)
		(progn ;; send directory listing, convert org files to html/tex/txt
		  (print (concat "PATH IS" path))
		  (ws-response-header process 200 (cons "Content-type" "text/html"))
		  (process-send-string process
							   (concat "<ul>"
									   (mapconcat
										(lambda (f)
										  (let* ((full (expand-file-name (concat path "/" f))) ;; concat to fix `~` errors
												 (end (if (file-directory-p full) "/" ""))
												 (url (url-encode-url (concat (filename-in-docroot full) end))))
											;; (print (format "f %S path %S full %S end %S url %S" f path full end url))
											;; (print (format "docroot %S" (filename-in-docroot full)))
											(format "<li><a href=%s>%s</li>" url (concat f end))))
										(sort (append (apply #'append
															 (mapcar
															  (lambda (f)
																(list
																 ;; (concat f ".txt")
																 ;; (concat f ".tex")
																 (concat f ".org")
																 (concat f ".html")))
															  (mapcar #'file-name-sans-extension
																	  (directory-files path nil ;; "^[^\.]"))))
																					   "^[^\.].*org$"))))
													  (cl-remove-if-not
													   (lambda (dirname)
														 (and
														  (file-directory-p (expand-file-name dirname path))
														  (not (and (string= path docroot) (string= dirname "..")))
														  ))
													   (directory-files path nil)) ;; TODO: list directories
													  ) 'string<)
										"\n") "</ul>"))))
	   ;; TODO: serve as editable file
	   ((string-equal "org" (file-name-extension path))
		(progn
		  (ws-response-header process 200 (cons "Content-type" "text/html"))
		  ;; (print (format "THIS IS PATH %S" path))
		  (let ((filename (filename-in-docroot path))
				(content (with-temp-buffer
						   (insert-file-contents orig)
						   (buffer-string))))
			(process-send-string process
								 (make-html filename content))
			)))
	   ;; Export the file as requested and return the result
	   (t (let* ((type (case (intern (downcase (file-name-extension path)))
						 (html 'html)
						 (tex  'latex)
						 (txt  'ascii)
						 (t (ws-error process "%S export not supported"
									  (file-name-extension path))))))
			(unless (file-exists-p orig) (ws-send-404 process))
			(save-window-excursion (find-file orig)
								   (org-export-to-file type path))
			(ws-send-file process path)))))))

(defmacro assert (test-form)
  `(when (not ,test-form)
     (error "Assertion failed: %s" (format "%s" ',test-form))))

(defun filename-in-docroot (full)
  (let ((filename (substring full (length docroot))))
	;; (print (format "root %S base %S" (concat docroot filename) full))
	(assert (string-equal (concat docroot filename) full)) ;; no escapes
	filename))

;; read posted file and save to disk
;; POST localhost:9002/abc/def/test -F 'content=* Header'
(defun org-poster (request)
  ;; TODO: add authentication
  (with-slots (process headers) request
	(ws-response-header process 200 '("Content-type" . "text/plain"))
	;; (print request)
	;; (print headers)
	(let* ((path (ws-in-directory-p ; check if path is in docroot
				  docroot (substring (cdr (assoc :POST headers)) 1)))
		   (base (file-name-sans-extension path))
		   (orig (concat base ".org")) ;; full filepath of doc to write
		   (message-assoc (cdr (assoc "content" headers)))
		   (message
			(if (not (equal nil message-assoc))
				(cdr (assoc 'content message-assoc))
			  nil)))
	  (print (format "root %S base %S" (concat docroot (filename-in-docroot orig)) orig))
	  (if (not (equal nil message))
		  (progn
			(if (not (file-directory-p (file-name-directory orig)))
				(make-directory (file-name-directory orig) t))
			(with-temp-buffer
			  (insert message)
			  (write-region nil nil orig))
			(process-send-string process (format "Wrote %S.\n" orig)))
		(process-send-string process
							 (format "This is a POST request with no content.\n"))
		))))

;; (ws-start 'org-server 9014)

(setq my-server (ws-start   '(((:POST . ".*") . org-poster)
							  ((:GET . ".*") . org-server))
							9002))

;; (ws-stop-all)
