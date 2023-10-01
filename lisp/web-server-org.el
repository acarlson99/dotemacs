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

;; (add-to-list 'load-path "~/p/emacs-org-server/")
;; (add-to-list 'load-path "./")
;; (add-to-list 'load-path "~/.emacs.d/elpa/")

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'el-log)

;; dangerous :)
(defun ask-user-about-supersession-threat (fn)
  "blatantly ignore files that changed on disk"
  )

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

;; JS stuff

(defvar js-embed-string "
function sendContent() {
  // Get the content from the textarea
  // const content = document.getElementById('editable-content').innerText;
  const content = document.getElementById('editable-content').value;

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

(defvar js-onload-func "
window.onload = function(){
// document.getElementById('editable-content').innerText = content;
document.getElementById('editable-content').value = content;
};
// document.onload = function(){
// document.getElementById('editable-content').editableContent = 'true';
// if (navigator.userAgent.search('Firefox') === -1)
//   document.getElementById('editable-content').editableContent = 'plaintext-only';
// };
")

(defun make-html-dual-view (path-a path-b)
  (charge-html
   `(html
	 (head
	  ;; (source "document.getElementByID() ; document.getElementById("view-a").contentDocument.activeElement.getElementsByTagName('button')[0].onclick
	  (script "
window.onload = function(){
var viewA = document.getElementById('view-a');
var viewB = document.getElementById('view-b');
var b = viewA.contentDocument.getElementsByTagName('button')[0];
var oldF = b.onclick;
var refreshF = () => { viewB.contentWindow.location.reload(); };
b.onclick = () => { oldF(); refreshF(); };
}")
	  (style "iframe { width: 50vw; height: 100vh; }")
	  (style "body { margin: 0; }"))
   (body
	(iframe :id view-a :src ,path-a :frameBorder 0)
	(iframe :id view-b :src ,path-b :frameBorder 0)
	))))

(defun escape-js-special-characters (str)
  "Escape common special characters in a JavaScript string."
  (setq str (replace-regexp-in-string "\\\\" "\\\\\\\\" str)) ; Escape backslashes
  (setq str (replace-regexp-in-string "\n"
									  "\\\\n" str)) ; Escape newlines
  (setq str (replace-regexp-in-string "\r"
									  "\\\\r" str)) ; Escape carriage returns
  (setq str (replace-regexp-in-string "\t"
									  "\\\\t" str)) ; Escape tabs
  (setq str (replace-regexp-in-string "\b"
									  "\\\\b" str)) ; Escape backspace
  (setq str (replace-regexp-in-string "\f"
									  "\\\\f" str)) ; Escape form feed
  (setq str (replace-regexp-in-string "\v"
									  "\\\\v" str)) ; Escape vertical tab
  (setq str (replace-regexp-in-string "\0"
									  "\\\\0" str)) ; Escape null character
  (setq str (replace-regexp-in-string "\\u" ;; TODO: fix this; adds an extra '\'
									  "\\\\\\\\u" str)) ; Escape Unicode escapes like \uXXXX
  str)

;; Example usage:
(escape-js-special-characters  "This is a string with special characters: \n\r\t\"'")

(defun make-html (filepath content)
  (charge-html
   `(html
	 (head
	  (title ,(format "Edit %S" filepath))
	  (meta :charset "UTF-8")
	  (style "body { height: 100%; background-color: darkblue; color: red; }")
	  (style "textarea { width: 100%; height: 90vh; background-color: darkblue; color: red; }")
	  (style "button { margin: auto; display: block; }")
	  (style "h1 { text-align: center; margin: auto; }")
	  (script ,(format "var filepath = %S;" (escape-js-special-characters filepath)))
	  (script ,(format "var content = `%s`; console.log(content);" (escape-js-special-characters content)))
	  (script ,js-onload-func)
	  (script ,js-embed-string))
	 (body
	  (h1 ,(format "Edit %S" filepath))
	  (br)
	  ;; (p :contenteditable "true" :id editable-content)
	  (textarea :type text :id editable-content)
	  ;; (p :contenteditable nil :id editable-content ,content)
	  (br)
	  (button :onclick "sendContent()" "Upload")
	  ))))

;; BEGIN SERVER

(require 'url)
(require 'web-server)
(require 'org)
(setq org-confirm-babel-evaluate nil)

;; (setq docroot "/home/john/p/dota-draml")
(setq docroot "/tmp/test")

;; serve GET requests for:
;; * FILE.html -- compile FILE.org to HTML and serve
;; * FILE.org -- serve org file in editable interface and allow updates
;; * path/dir/folder/ -- serve ORG/HTML files
(defun org-server (request)
  (with-slots (process headers) request
	(let* ((path (ws-in-directory-p ; check if path is in docroot
				  docroot (substring (cdr (assoc :GET headers)) 1)))
		   (base (file-name-sans-extension path))
		   (orig (concat base ".org"))
		   (extension (downcase (or (file-name-extension path) ""))))
	  (unless path (ws-send-404 process)) ; send 404 if not in docroot
	  (cond
	   ;; /path/to/dir/ -- require trailing slash
	   ((and (string= (substring (cdr (assoc :GET headers)) -1) "/") (file-directory-p path))
		(progn ;; send directory listing, convert org files to html/tex/txt
		  (el-log (format "Serving directory %S." path))
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
																 (concat f ".edit")
																 (concat f ".html")
																 (concat f ".org")))
															  (mapcar #'file-name-sans-extension
																	  (directory-files path nil ;; "^[^\.]"))))
																					   "^[^\.].*org$"))))
													  (cl-remove-if-not
													   (lambda (dirname)
														 (and
														  (file-directory-p (expand-file-name dirname path))
														  (not (and (string= path docroot) (string= dirname "..")))
														  ))
													   (directory-files path nil))
													  ) 'string<)
										"\n") "</ul>"))))
	   ;; serve dual-view to edit
	   ((string= extension "")
	   ;; ((string-equal "edit" (file-name-extension path))
		(progn
		  (el-log (format "dual editing file %S." orig))
		  (ws-response-header process 200 (cons "Content-type" "text/html"))
		  ;; (process-send-string process (make-html-dual-view (print (format "orig %S" orig)) (print (format "html %S" (concat base ".html")))))
		  (process-send-string process (make-html-dual-view (filename-in-docroot orig) (filename-in-docroot (concat base ".html"))))
		  ))
	   ;; serve as editable file
	   ((string= extension "org")
		;; ((string-equal "org" (or (file-name-extension path) ""))
		(progn
		  (el-log (format "editing file %S." orig))
		  (ws-response-header process 200 (cons "Content-type" "text/html"))
		  (let ((filename (filename-in-docroot path))
				(content (with-temp-buffer
						   (if (file-exists-p orig)
							   (insert-file-contents orig))
						   (buffer-string))))
			(process-send-string process
								 (make-html filename content))
			)))
	   ;; Export the file as requested and return the result
	   ((file-exists-p orig)
		(el-log (format "exporting with extension %S." extension)
		(let* ((type (cl-case (intern extension)
						 (html 'html)
						 (tex  'latex)
						 (latex  'latex)
						 (txt  'ascii)
						 (t (ws-error process "%S export not supported"
									  (file-name-extension path))))))
			(unless (file-exists-p orig) (ws-send-404 process))
			(save-window-excursion (find-file orig)
								   (org-export-to-file type path))
			(ws-send-file process path))))
	   (t (ws-send-404 process))
		))))

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
	  (el-log (format "root %S base %S" (concat docroot (filename-in-docroot orig)) orig))
	  (if (not (equal nil message))
		  (progn
			(el-log (format "WRITING TEXT TO %S" orig))
			(el-log (format "TEXT: %S" message))
			(if (not (file-directory-p (file-name-directory orig)))
				(make-directory (file-name-directory orig) t))
			(with-temp-buffer
			  (insert message)
			  (write-region nil nil orig nil t))
			(process-send-string process (format "Wrote %S.\n" orig)))
		(process-send-string process
							 (format "This is a POST request with no content.\n"))
		))))

;; (ws-start 'org-server 9014)

(defvar my-server (ws-start   '(((:POST . ".*") . org-poster)
								((:GET . ".*") . org-server))
							  9002))

;; (ws-stop-all)

(defvar block (while t (sleep-for 99999999)))
