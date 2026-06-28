;; from https://github.com/finalpatch/ob-glsl/
;; requires ~/.emacs.d/module/ob-glsl-module.so module to exist and be loaded

(require 'ob)
(if (require 'ob-glsl-module nil t)
    (progn

      (defvar org-babel-default-header-args:glsl
		'((:results . "raw html") (:exports . "results"))
		"Default arguments to use when evaluating a glsl source block.")

      (defun org-babel-expand-body:glsl (body params)
		"Expand BODY according to PARAMS, return the expanded body."
		(concat
		 "#version 300 es
#ifdef GL_ES
precision mediump float;
#endif
#ifdef GL_FRAGMENT_PRECISION_HIGH
precision highp float;
#else
precision mediump float;
#endif

uniform float iTime;
"
		 ;; "#version 330 core\n"
		 "out vec4 fragColor;\n"
		 "uniform vec2 iResolution;\n"
		 body))

	  (defun org-babel-execute:glsl (body params)
		"Execute a block of GLSL code with org-babel.
	  If exporting to HTML, embed a WebGL viewer instead of running ob-glsl-run."
		(let* (
			   (glsl-code (org-babel-expand-body:glsl body params))
			   (render-width (or (cdr (assq :width params)) 400))
			   (render-height (or (cdr (assq :height params)) 300)))
		  (setq glsl-body-whooo body)
		  (setq glsl-params-whooo params)
		  (if (eq org-export-current-backend 'html)
			  ;; Instead of producing an image, return HTML snippet
			  (format "<canvas class='shader-canvas' width='%d' height='%d'
	                   >%s</canvas>"
					  render-width render-height
					  (org-html-encode-plain-text glsl-code))
			;; Normal local rendering
			(let ((out-file (org-babel-process-file-name
							 (cdr (assq :file params)) t)))
			  (ob-glsl-run glsl-code render-width render-height out-file))
			nil)))

      (defun org-babel-prep-session:shady (_session _params)
		"Return an error because glsl does not support sessions."
		(error "glsl does not support sessions"))))

(provide 'ob-glsl)
