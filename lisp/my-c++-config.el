;; Coplien form for c++ coding ease
(defun c++-insert-class-header (name)
  "Add Coplien form to .hpp for C++ Classes painlessly"
  (interactive "sClass name: ")
  (insert "\n#ifndef "(upcase name)"_HPP\n# define "(upcase name)"_HPP\n")
  (insert "\n")
  ;; (insert "# include <iostream>\n# include <string>\n# include <stdexcept>\n\n")
  (insert "class "name" {\n\npublic:\n")
  ;; (insert "\t"name"( std::string );\n")
  (insert "\t"name"( void );\n")
  (insert "\t"name"( "name" const & cp);\n")
  (insert "\t~"name"( void );\n")
  (insert "\t"name"& operator=( "name" const &);\n")
  (insert "\nprivate:\n\n};\n\n#endif\n")
  )

(defun c++-insert-class-src (name)
  "Add Coplien form to .cpp for C++ Classes painlessly"
  (interactive "sClass name: ")
  (insert "\n#include \""name".hpp\"\n\n")
  ;; (insert name"::"name"( std::string ) : { }\n")
  (insert name"::"name"( void ) { }\n")
  (insert name"::"name"( "name" const & cp) { *this = cp; }\n")
  (insert name"::~"name"( void ) { }\n")
  (insert name"& "name"::operator=( "name" const &) { return *this; }\n")
  )

(defun c++-insert-class (name)
  "Coplien Form Caller"
  (interactive "sClass name: ")
  (if (equal (file-name-extension(buffer-file-name)) "hpp")
	  (c++-insert-class-header name))
  (if (equal (file-name-extension(buffer-file-name)) "cpp")
	  (c++-insert-class-src name))
  )

(defun c++-insert-exception-header (name nest)
  "Add Coplien form to .hpp for C++ Exceptions painlessly"
  (interactive "sClass name: \nsNested in: ")
  (insert "\tclass "name"Exception : public std::exception {\n\tpublic:\n")
  (insert "\t\t"name"Exception( void );\n")
  (insert "\t\t"name"Exception( "name"Exception const & cp);\n")
  (insert "\t\t~"name"Exception( void ) throw();\n")
  (insert "\t\t"name"Exception& operator=( "name"Exception const & e);\n")
  (insert "\t\tvirtual const char* what() const throw();\n")
  (insert "\t};\n")
  )

(defun c++-insert-exception-src (name nest)
  "Add Coplien form to .cpp for C++ Exceptions painlessly"
  (interactive "sClass name: \nsNested in: ")
  (if (string= nest "")
	  (setq newNest "")
	(setq newNest (concat nest "::")))
  (insert newNest name"Exception::"name"Exception( void ) { }\n")
  (insert newNest name"Exception::"name"Exception( "name"Exception const & cp) { *this = cp; }\n")
  (insert newNest name"Exception::~"name"Exception( void ) throw() { }\n")
  (insert newNest name"Exception& "newNest name"Exception::operator=( "name"Exception const &) { return *this; }\n")
  (insert "const char* "newNest name"Exception::what( void ) const throw() {\n\treturn \""name"\";\n}\n")
  )

(defun c++-insert-exception (name nest)
  "Exception Coplien Form Caller"
  (interactive "sException Name: \nsNested in class: ")
  (if (equal (file-name-extension(buffer-file-name)) "hpp")
	  (c++-insert-exception-header name nest))
  (if (equal (file-name-extension(buffer-file-name)) "cpp")
	  (c++-insert-exception-src name nest))
  )

(defun my-c++-config()
  "For use in 'c++-mode-hook'."
  ;; template exception insertion stuff from Steven
  (local-set-key (kbd "C-c i i") 'c++-insert-class)
  (local-set-key (kbd "C-c i e") 'c++-insert-exception)
  ;; better comments
  (local-set-key (kbd "C-c C-c") 'comment-norminette)
  ;; whitespace cleanup
  (local-set-key (kbd "C-c w") 'whitespace-cleanup)
  ;; uncommenting
  (local-set-key (kbd "C-c c") 'uncomment-region))
