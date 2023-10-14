:; emacs --batch -l "$0" -f web-server-org-main -- "$@" && exit

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "/opt/web-server-org/") ;; in case of systemctl
(require 'package)
(unless package--initialized (package-initialize))
(unless (require 'web-server nil :noerror)
  (package-install 'web-server)
  (require 'web-server))
(require 'el-log)
(require 'argparse)
(require 'url)
(require 'org)

;; TODO: disable 'would you like to reread file? (yes/no)' message
;; dangerous :)
(defun ask-user-about-supersession-threat (filename)
  "blatantly ignore files that changed on disk"
  nil)

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

;; JS HTML stuff

(defvar host-address "http://localhost"
  "HOST-ADDRESS is the location to which frontend JS should send POST requests")
(defvar host-port (int-to-string 8080))

(defun js-embed-string ()
  (concat "
function sendContent() {
  const content = document.getElementById('editable-content').value;

  // Create a FormData object
  const formData = new FormData();
  
  // Append the content to the FormData object
  formData.append('content', content);

  // Create a new XMLHttpRequest object
  const xhr = new XMLHttpRequest();

  // Define the POST request to 'localhost:8080'
  xhr.open('POST', '"
		  host-address
		  (if (not (or (equal nil host-port) (string-empty-p host-port)))
			  (concat ":" host-port)
			"")
		  "'+filepath, true);

  // Send the FormData object
  xhr.send(formData);

  xhr.onreadystatechange = function () {
    if (xhr.readyState === 4) {
      if (xhr.status === 200) {
        alert('Content sent successfully.'); // TODO: this alert here is bad
      } else {
        alert('Error sending content; code '+String(xhr.status));
      }
    }
  };
}
"))

(defvar js-onload-func "
window.onload = function(){
document.getElementById('editable-content').value = content;
};
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

;; from github.com/magnars/s.el
(defvar ucs-normalize-combining-chars)  ; Defined in `ucs-normalize'
(autoload 'slot-value "eieio")
(defun s-replace (old new s)
  "Replaces OLD with NEW in S."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (regexp-quote old) new s t t))
;; end s.el

(defun escape-js-special-characters (str)
  (s-replace "\\" "\\\\" str))

(defun make-html (filepath content)
  (charge-html
   `(html
	 (head
	  (title ,(format "Edit %S" filepath))
	  (meta :charset "UTF-8")
	  (style "body { height: 100%; background-color: darkblue; color: red; margin: 0; }")
	  (style ,(concat
	  		   "textarea { "
			   "font-family: \"Lucida Console\", \"Courier New\", monospace; "
			   "width: 100%; height: 90vh; "
			   "background-color: darkblue; color: red; "
			   "}")) ;; "Lucida Console", "Courier New", monospace;
	  (style "button { margin: auto; display: block; }")
	  (style "h1 { text-align: center; margin: auto; }")
	  (script ,(format "var filepath = %S;" (escape-js-special-characters filepath)))
	  (script ,(format "var content = `%s`; console.log(content);" (escape-js-special-characters content)))
	  (script ,js-onload-func)
	  (script ,(js-embed-string)))
	 (body
	  (h1 ,(format "Edit %S" filepath))
	  (br)
	  (textarea :type text :id editable-content)
	  (br)
	  (button :onclick "sendContent()" "Upload")
	  ))))

;; BEGIN SERVER

(setq org-confirm-babel-evaluate nil)

(defvar docroot "/tmp/test/") ;; TODO: add prefix e.g. `org-server-docroot'

(defun org-server-string-starts-with (prefix str)
  (and
   (>= (length str) (length prefix))
   (string= (substring str 0 (length prefix)) prefix)))

;; serve GET requests for:
;; * FILE.html -- compile FILE.org to HTML and serve
;; * FILE.org -- serve org file in editable interface and allow updates
;; * path/dir/folder/ -- serve ORG/HTML files
(defun org-server (request)
  (condition-case err
	  (with-slots (process headers) request
		(let ((path (ws-in-directory-p	; check if path is in docroot
					 docroot (substring (cdr (assoc :GET headers)) 1))))
		  (unless path (ws-send-404 process "invalid path")) ; send 404 if not in docroot
		  (let* ((base (file-name-sans-extension path))
				 (orig (concat base ".org"))
				 (extension (downcase (or (file-name-extension path) ""))))
			(unless path (ws-send-404 process))
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
													   ;; (end (if (file-directory-p full) "/" ""))
													   (url (url-encode-url (filename-in-docroot full))))
												  ;; (print (format "f %S path %S full %S end %S url %S" f path full end url))
												  ;; (print (format "docroot %S" (filename-in-docroot full)))
												  (format "<li><a href=%s>%s</li>" url f)))
											  (sort (append (apply #'append
																   (mapcar
																	(lambda (f)
																	  (list
																	   (concat f)
																	   ;; (concat f ".tex")
																	   ;; (concat f ".txt")
																	   ;; (concat f ".html")
																	   ;; (concat f ".org")))
																	   ))
																	(mapcar #'file-name-sans-extension
																			(directory-files path nil ;; "^[^\.]"))))
																							 "^[^\.].*org$"))))
															(mapcar (lambda (s) (concat s "/"))
																	(cl-remove-if-not
																	 (lambda (dirname)
																	   (and
																		(file-directory-p (expand-file-name dirname path))
																		(not (and (string= path docroot) (string= dirname "..")))
																		))
																	 (directory-files path nil)))
															) 'string<)
											  "\n") "</ul>"))))
			 ((string= (substring (cdr (assoc :GET headers)) -1) "/")
			  (ws-send-404 process ))
			 ;; serve dual-view to edit
			 ((string= extension "")
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
	(error (el-log "caught error %s" (error-message-string err)))))

(defun filename-in-docroot (full)
  (let ((filename (substring full (length docroot))))
	;; (print (format "root %S base %S" (concat docroot filename) full))
	(cl-assert (string-equal (concat docroot filename) full)) ;; no escapes
	(if (and (> (length filename) 0) (string= (substring filename 0 1) "/"))
		filename
	  (concat "/" filename))))

;; read posted file and save to disk
;; POST localhost:8080/abc/def/test -F 'content=* Header'
(defun org-poster (request)
  ;; TODO: add authentication for poasting
  ;; TODO: add exception catch/error handling
  (condition-case err
	  (with-slots (process headers) request
		(ws-response-header process 200 '("Content-type" . "text/plain"))
		;; (print request)
		;; (print headers)
		(let* ((post-header (let ((s (substring (cdr (assoc :POST headers)) 1))
								  (host-header (cdr (assoc :HOST headers))))
							  ;; POST header sometimes looks like `HOST:PORT/path'
							  ;; correction to remove prefix hostname
							  (if (org-server-string-starts-with host-header s)
								  (substring s (+ 1 (length host-header)))
								s)))
			   (path (ws-in-directory-p ; check if path is in docroot
					  docroot post-header))
			   (base (file-name-sans-extension path))
			   (orig (concat base ".org")) ;; full filepath of doc to write
			   (message-assoc (cdr (assoc "content" headers)))
			   (message
				(if (not (equal nil message-assoc))
					(cdr (assoc 'content message-assoc))
				  nil)))
		  (el-log "root %S base %S" (concat docroot (filename-in-docroot orig)) orig)
		  (if (not (equal nil message))
			  (progn
				(el-log "writing text to %S len %d" orig (length message))
				;; (el-log "msg %S" message)
				(if (not (file-directory-p (file-name-directory orig)))
					(make-directory (file-name-directory orig) t))
				(with-temp-buffer
				  (insert message)
				  (write-region nil nil orig nil t))
				(process-send-string process (format "Wrote %S.\n" orig)))
			(progn
			  (ws-send-404)
			  (process-send-string process
								   (format "This is a POST request with no content.\n"))))))
	(error (el-log "caught error %s" (error-message-string err)))))

;; (ws-start 'org-server 9014)

(when (require 'OwOify nil :noerror)
  (add-to-list 'el-log-middleware #'OwOify))

(defmacro setq-if (SYM VAL)
  `(let ((v ,VAL))
	 (if v
		 (setq ,SYM v))))

(defun web-server-org-main ()
  ;; scan for '--' to skip emacs args
  (let* ((argv-2 (cdr (argparse-scan-argv "--" argv)))
		 (argp-opts
		  (list
		   (make-argparse-opt :name "help" :longopt "help" :shortopt "h" :description "Help msg")
		   (make-argparse-opt :name "host-address" :longopt "host" :has-arg t :arg-type 'str
							  :default "http://localhost"
							  :description "Address for frontend to contact host")
		   (make-argparse-opt :name "port" :longopt "port" :shortopt "p" :has-arg t :arg-type 'int
							  :default 8080
							  :description "Port to bind and listen to")
		   (make-argparse-opt :name "short-log-prefix" :longopt "short-log-prefix"
							  :description "Shorten server prefix")
		   (make-argparse-opt :name "docroot" :longopt "dir" :shortopt "d" :has-arg t :arg-type 'str
							  :default "/tmp/org-docroot/"
							  :description "Fileserver root directory")))
		 (argp-v
		  (argparse-getopt
		   argp-opts
		   argv-2))
		 (argv (cadr argp-v))
		 (args (car argp-v)))
	(if (argparse-get-arg argp-opts "help" args)
		(progn
		  (message "Usage: ./web-server-org.el [OPTION]")
		  (message "")
		  (message "%s" (argparse-help-msg argp-opts)))
	  (progn
		(if (argparse-get-arg argp-opts "short-log-prefix" args)
			(setq el-log-msg-prefix-fn
				  (lambda (lvl)
					(format "[%s] " lvl))))
		(setq-if host-address (cdr (argparse-get-arg argp-opts "host-address" args)))
		(let ((v (cdr (argparse-get-arg argp-opts "port" args))))
		  (if v
			  (setq host-port (int-to-string v))))
		(setq-if docroot (cdr (argparse-get-arg argp-opts "docroot" args)))
		(if (> (length argv) 0)
			(el-log-lvl 'WARN "Unconsumed args %S" argv))
		(el-log "preparing")
		(global-auto-revert-mode t)
		(setq auto-revert-verbose nil)
		(defvar my-server (ws-start '(((:POST . ".*") . org-poster)
									  ((:GET . ".*") . org-server))
									(string-to-number host-port)
									nil
									:host "0.0.0.0" ;; critical fix for Docker
									))
		;; TODO: ^ add network args
		;; TODO: https support
		;; TODO: (require 'ox) org mode export theme customization
		;; TODO: css
		(el-log "Hello, server is running at %s:%s" host-address host-port)
		(el-log "Serving HTML and ORG files in directory %S" docroot)
		(el-log "... ~nyaa")
		(defvar block (while t (sleep-for 99999999))) ;; TODO: add prefix
		(ws-stop-all)))))
