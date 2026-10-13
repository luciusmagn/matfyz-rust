#!/usr/bin/env -S sbcl --script
(require :asdf)

(defun package-public-site (destination)
  "Copy the explicit publication manifest into a new directory."
  (let* ((root (truename (merge-pathnames
                          "../" (uiop:pathname-directory-pathname *load-truename*))))
         (output (uiop:ensure-directory-pathname
                  (merge-pathnames destination (uiop:getcwd))))
         (files (uiop:read-file-lines
                 (merge-pathnames "deployment/public-files.txt" root))))
    (when (probe-file output)
      (error "Publication directory already exists: ~A" output))
    (labels ((copy-public-file (source target)
               (let ((input (truename (merge-pathnames source root)))
                     (result (merge-pathnames target output)))
                 (unless (uiop:subpathp input root)
                   (error "Publication source is outside the repository: ~A" source))
                 (ensure-directories-exist result)
                 (uiop:copy-file input result))))
      (copy-public-file "deployment/index.html" "index.html")
      (dolist (file files)
        (unless (and (uiop:string-prefix-p "materials/" file)
                     (not (search ".." file))
                     (not (find #\\ file)))
          (error "Invalid publication path: ~S" file))
        (copy-public-file file file)))
    (format t "Published ~D files to ~A~%" (1+ (length files)) output)))

(let ((arguments (uiop:command-line-arguments)))
  (unless (= 1 (length arguments))
    (error "Usage: sbcl --script deployment/package.lisp NEW-DIRECTORY"))
  (package-public-site (first arguments)))
