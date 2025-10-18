(in-package :matfyz)

(-> directory-list-subdirs (pathname) (list-of pathname))
(defun directory-list-subdirs (directory)
  "List all subdirectories in DIRECTORY."
  (remove-if-not #'cl-fad:directory-pathname-p
                 (cl-fad:list-directory directory)))

(-> directory-get-name (pathname) string)
(defun directory-get-name (directory)
  "Extract the name component from a directory pathname."
  (car (reverse (pathname-directory directory))))

(-> directory-list-files (pathname) (list-of pathname))
(defun directory-list-files (directory)
  "List all regular files in DIRECTORY."
  (remove-if #'cl-fad:directory-pathname-p
             (cl-fad:list-directory directory)))

(-> file-get-extension (pathname) string)
(defun file-get-extension (path)
  "Extract the file extension from PATH."
  (let ((name (file-namestring path)))
    (aif (position #\. name :from-end t)
         (subseq name it)
         "")))

(-> file-get-basename (pathname) string)
(defun file-get-basename (path)
  "Extract the base name (without extension) from PATH."
  (let ((name (file-namestring path)))
    (aif (position #\. name :from-end t)
         (subseq name 0 it)
         name)))

(-> name-humanize (string) string)
(defun name-humanize (name)
  "Convert kebab-case NAME to human-readable format."
  (string-capitalize (substitute #\Space #\- name)))
