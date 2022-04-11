;; Coplien form for c++ coding ease
(defun c++-insert-class-header (name)
  "Insert Coplien form class to .hpp.  Insert class NAME."
  (interactive "sClass name: ")
  (insert "#ifndef "(upcase name)"_HPP\n# define "(upcase name)"_HPP\n")
  (insert "\n")
  ;; (insert "# include <iostream>\n# include <string>\n# include <stdexcept>\n\n")
  (insert "class " name " {\n\npublic:\n")
  ;; (insert "\t" name "(std::string);\n")
  (insert "\t" name "(void);\n")
  (insert "\t" name "(" name " const &);\n")
  (insert "\t~" name "(void);\n")
  (insert "\t" name "& operator=(" name " const &);\n")
  (insert "\nprivate:\n\n};\n\n#endif\n"))

(defun c++-insert-class-src (name)
  "Insert Coplien form class to .cpp.  Insert class NAME."
  (interactive "sClass name: ")
  (insert "#include \"" name ".hpp\"\n\n")
  ;; (insert name"::" name "(std::string) : { }\n")
  (insert name"::" name "(void) { }\n")
  (insert name"::" name "(" name " const &cp) { *this = cp; }\n")
  (insert name"::~" name "(void) { }\n")
  (insert name"& " name "::operator=("
		  name " const &) { return *this; }\t// TODO: implement\n"))

(defun c++-insert-class (name)
  "Class Coplien form caller.  Insert class NAME to buffer."
  (interactive "sClass name: ")
  (if (equal (file-name-extension(buffer-file-name)) "hpp")
	  (c++-insert-class-header name))
  (if (equal (file-name-extension(buffer-file-name)) "cpp")
	  (c++-insert-class-src name)))

(defun c++-insert-exception-header (name nest)
  "Insert Coplien form exceptions to .hpp.  Insert exception NAME in class NEST."
  (interactive "sClass name: \nsNested in: ")
  (insert "\tclass " name "Exception : public std::exception {\n\tpublic:\n")
  (insert "\t\t" name "Exception(void);\n")
  (insert "\t\t" name "Exception(" name "Exception const &);\n")
  (insert "\t\t~" name "Exception(void) throw();\n")
  (insert "\t\t" name "Exception& operator=(" name "Exception const &);\n")
  (insert "\t\tvirtual const char*\twhat() const throw();\n")
  (insert "\t};\n"))

(defun c++-insert-exception-src (name nest)
  "Insert Coplien form exceptions to .hpp.  Insert exception NAME in class NEST."
  (interactive "sClass name: \nsNested in: ")
  (let ((newNest (if (string= nest "")
					 ""
				   (concat nest "::"))))
	(insert newNest name"Exception::" name "Exception(void) { }\n")
	(insert newNest name"Exception::" name "Exception(" name "Exception const &cp) { *this = cp; }\n")
	(insert newNest name"Exception::~" name "Exception(void) throw() { }\n")
	(insert newNest name"Exception& "newNest name"Exception::operator=(" name "Exception const &) { return *this; }\n")
	(insert "const char\t*"newNest name"Exception::what(void) const throw() {\n\treturn \"" name "Exception\";\n}\n")))

(defun c++-insert-exception (name nest)
  "Exception Coplien form caller.  Insert exception NAME in class NEST."
  (interactive "sException Name: \nsNested in class: ")
  (if (equal (file-name-extension(buffer-file-name)) "hpp")
	  (c++-insert-exception-header name nest))
  (if (equal (file-name-extension(buffer-file-name)) "cpp")
	  (c++-insert-exception-src name nest)))

(defun c++-config ()
  "For use in 'c++-mode-hook'."
  (when (require 'c-config nil 'noerror)
	(c-config))
  ;; template exception insertion stuff from Steven
  (local-set-key (kbd "C-c i i") 'c++-insert-class)
  (local-set-key (kbd "C-c i e") 'c++-insert-exception))

(provide 'c++-config)
