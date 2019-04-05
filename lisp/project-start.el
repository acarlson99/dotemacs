;;; project-start.el --- Start project of a specific type
;; Author: Alexander Carlson <alexandercarlson99@gmail.com>
;; Maintainer: Alexander Carlson <alexandercarlson99@gmail.com>
;; Created: 31 Mar 2019
;; Keywords: project
;; This file is not part of GNU Emacs.

;;; Commentary:
;; Hercules project for 42 Silicon Valley
;; The goal is to make a simple script to make starting projects easier/faster

;;; Code:

(defun clone-gitignore ()
  "Clone gitignore template."
  (if (y-or-n-p "Clone gitignore? ")
	  (progn
		(if (file-exists-p ".gitignore")
			(message ".gitignore file exists.  Skipping clone-gitignore")
		  (progn
			(message "cloning gitignore")
			(shell-command "git clone https://github.com/bombblob/gitignore_template.git gitignore_dir")
			(rename-file "gitignore_dir/.gitignore" "./.gitignore")
			(shell-command "rm -rf gitignore_dir"))))
	(message "skipping clone-gitignore")))

(defun clone-libft ()
  "Clone libft."
  (if (y-or-n-p "Clone libft? ")
	  (progn
		(if (file-exists-p "libft")
			(message "libft exists.  Skipping clone-libft")
		  (progn
			(message "cloning libft")
			(shell-command "git clone https://github.com/bombblob/libft.git libft")
			(shell-command "rm -rf libft/.git"))))
	(message "skipping clone-libft")))

(defun make-author ()
  "Create author file."
  (if (y-or-n-p "Create author file? ")
	  (progn
		(if (file-exists-p "author")
			(message "author file exists.  Skipping make-author")
		  (progn
			(message "creating author file")
			(append-to-file user-login-name nil "author")
			)))
	(message "skipping make-author")))

(defun git-init-project (repo-url)
  "Initialize git repo and maybe push to REPO-URL."
  (interactive "sRepo url(blank to skip) ")
  (if (string= repo-url "")
	  (message "Skipping git-init-project")
	(progn
	  (if (file-exists-p ".git")
		  (message ".git file exists.  Skipping git-init-project")
		(progn
		  (if (shell-command (format "git init && git add . && git commit -m \"First commit\" && git remote add origin %s && git push -u origin master" repo-url))
			  (message "PUSH FAILED")
			(message "push successful")))))))

(defun project-start-c ()
  "Start a c project."
  (message "Starting c project")
  (make-author)
  (clone-gitignore)
  (clone-libft)
  (if (file-exists-p "src")
	  (message "src dir exists.  Skipping creation")
	(make-directory "src"))
  (if (file-exists-p "inc")
	  (message "inc dir exists.  Skipping creation")
	(make-directory "inc"))
  (if (file-exists-p "lib")
	  (message "lib dir exists.  Skipping creation")
	(make-directory "lib"))
  (if (file-exists-p "Makefile")
	  (message "Makefile exists.  Skipping creation")
	(with-temp-file "Makefile"
	  (insert "")))
  (git-init-project (read-from-minibuffer "Repo url(blank to skip) ")))

(defun project-start-c++ ()
  "Start a c++ project."
  (message "Starting c++ project")
  (make-author)
  (clone-gitignore)
  (clone-libft)
  (if (file-exists-p "src")
	  (message "src dir exists.  Skipping creation")
	(make-directory "src"))
  (if (file-exists-p "inc")
	  (message "inc dir exists.  Skipping creation")
	(make-directory "inc"))
  (if (file-exists-p "lib")
	  (message "lib dir exists.  Skipping creation")
	(make-directory "lib"))
  (if (file-exists-p "Makefile")
	  (message "Makefile exists.  Skipping creation")
	(with-temp-file "Makefile"
	  (insert "")))
  (git-init-project (read-from-minibuffer "Repo url(blank to skip) ")))

(defun project-start-rust ()
  "Start a rust project."
  (message "Starting rust project")
  (make-author)
  (clone-gitignore)
  (if (file-exists-p "src")
	  (message "src dir exists.  Skipping creation")
	(progn
	  (make-directory "src")
	  (with-temp-file "src/main.rs"
		(insert "fn main() {\n\tprintln!(\"Hello, world!\");\n}"))
	  ))
  (if (file-exists-p "Cargo.toml")
	  (message "Cargo.toml exists.  Skipping creation")
	(progn
	  (with-temp-file "Cargo.toml"
		(insert (format "[package]\nname = \"NAME\"\nversion = \"0.1.0\"\nauthors = [\"%s\"]\nedition = \"2018\"\n\n[dependencies]\n" user-login-name)))
	  ))
  (git-init-project (read-from-minibuffer "Repo url(blank to skip) ")))

(defun project-start-help ()
  "Help option for project-start."
  (message "This simply creates a series of files/directories depending on the type of project specified")
  (message "supported options: c c++ rust help"))

(defun project-start-generic ()
  "Start other type of project."
  (if (y-or-n-p "Invalid project type.  Create generic project? ")
	  (progn
		(make-author)
		(clone-gitignore)
		(git-init-project (read-from-minibuffer "Repo url(blank to skip) ")))))

(defun project-start (project-type)
  "Clone gitignore, libft(?), first push(?)\
and start project of type PROJECT-TYPE."
  (interactive "sWhat type of project?(help to show help message) ")
  (cond
   ((string= project-type "c")
	(project-start-c))
   ((string= project-type "c++")
	(project-start-c++))
   ((string= project-type "rust")
	(project-start-rust))
   ((string= project-type "help")
	(project-start-help))
   ((= 1 1)
	(project-start-generic))))

(provide 'project-start)
;;; project-start.el ends here
