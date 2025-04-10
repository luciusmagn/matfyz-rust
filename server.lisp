;; Yeehaw
(ql:quickload '(:hunchentoot
                :spinneret
                :cl-fad
                :cl-ppcre
                :uiop
                :split-sequence))

(defpackage :rust-server
  (:use :cl :hunchentoot :cl-ppcre)
  (:import-from :uiop :getcwd)
  (:import-from :spinneret :with-html-string))
(in-package :rust-server)

;; config - he scoped down the stairs very dynamically
(defparameter *base-directory* (getcwd))
(defparameter *port* 27777)
(defparameter *server* nil)
(defparameter *author* "Lukáš Hozda (LHO)")
(defparameter *contacts*
  '(("Discord" "@magnusi")
    ("Email" "lukas.hozda@braiins.cz, luk.hozda@gmail.com")))
(defparameter *department-url* "https://d3s.mff.cuni.cz/")
(defparameter *rust-resources*
  '(("Rust Book"       "https://doc.rust-lang.org/book/")
    ("Rust By Example" "https://doc.rust-lang.org/rust-by-example/")
    ("Rustlings"       "https://github.com/rust-lang/rustlings")
    ("Rust Reference"  "https://doc.rust-lang.org/reference/")))

;;there must be a better way to do these fs functions
(defun list-subdirs (directory)
  ;; I want my question marks back :C
  (remove-if-not #'cl-fad:directory-pathname-p
                 (cl-fad:list-directory directory)))

(defun dir-name (directory)
  (let* ((dirs     (pathname-directory directory))
         (reversed (reverse dirs))
         (name     (car reversed)))
    name))

(defun get-files (directory)
  (remove-if #'cl-fad:directory-pathname-p
             (cl-fad:list-directory directory)))

(defun file-ext (path)
  (let ((name (file-namestring path)))
    (subseq name
            (or (position #\. name :from-end t)
                (length name)))))

(defun file-basename (path)
  (let* ((name    (file-namestring path))
         (dot-pos (position #\. name :from-end t)))
    (if dot-pos
        (subseq name 0 dot-pos)
        name)))

(defun humanize (name) ; the design is very human
  (string-capitalize (substitute #\Space #\- name)))

;; page template
(defmacro with-page ((&key title) &body body)
  `(with-html-string
       (:doctype)
     (:html
      (:head
       (:meta :charset "UTF-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
       (:title ,title)
       (:style
        (:raw "
         @font-face {
           font-family: \"TT Livret\";
           src: url(\"/static/TTLivretTextRegular.woff2\") format(\"woff2\");
           font-weight: normal;
           font-style: normal;
         }

         * {
           font-family: \"TT Livret\";
         }

         body {
           font-family :serif;
           margin: 0;
           padding: 20px;
           background: black;
           color: white;
         }

         a {
           color: #DDD;
         }

         .container {
           max-width: 800px;
           margin: 0 auto;
         }

         .footer {
           margin-top: 40px;
           font-size: 0.9em;
         }
       ")))
      (:body
       (:div :class "container"
             ,@body
             (:div :class "footer"
                   (:p "Created by " (:strong ,*author*))
                   (:p "Contacts: "
                       (dolist (contact *contacts*)
                         (:span (:strong (car contact)) ": " (cadr contact) " ")))
                   (:p "Department: "
                       (:a :href *department-url* "Department of Distributed and Dependable Systems"))))))))

;; pages
(defun generate-index ()
  (with-page (:title "Rust @ MATFYZ CUNI")
    (:h1 "Rust @ MatFyz CUNI")
    (:p "It is a good language, you should learn it. Here are the course materials and some other resources.")
    (:h2 "Course Materials")
    (:ul
     (dolist (dir (list-subdirs *base-directory*))
       (let ((dir-name (dir-name dir)))
         (when (or (string= dir-name "advanced-rust")
                   (string= dir-name "intro-rust"))
           (:li (:a :href (format nil "/~A/" dir-name)
                    (humanize dir-name)))))))

    (:h2 "Rust Learning Resources")
    (:ul (dolist (resource *rust-resources*)
           (:li (:a :href (cadr resource) (car resource)))))))

(defun generate-course (course-name)
  (let ((course-path (merge-pathnames (make-pathname :directory (list :relative course-name))
                                      *base-directory*)))
    (with-page (:title (format nil "~A" (humanize course-name)))
      (:h1 (humanize course-name))
      (:p (:a :href "/" "← Back"))
      (:h2 "Labs")
      (:ul
       (dolist (lab-dir  (list-subdirs course-path))
         (let ((lab-name (dir-name lab-dir)))
           (:li (:a :href (format nil "/~A/~A/" course-name lab-name)
                    (humanize lab-name)))))))))

(defun generate-lab (course-name lab-name)
  (let ((lab-path (merge-pathnames
                   (make-pathname :directory (list :relative course-name lab-name))
                   *base-directory*)))
    (with-page (:title (format nil "~A - ~A" (humanize course-name) (humanize lab-name)))
      (:h1 (format nil "~A - ~A" (humanize course-name) (humanize lab-name)))
      (:p (:a :href (format nil "/~A/" course-name) "← Back"))
      (:h2 "Materials")
      (:ul
       (dolist (file (get-files lab-path))

         (let ((file-name (file-namestring file))
               (file-ext  (string-downcase (subseq (file-ext file) 1))))
           (:li (:a :href (format nil "/~A/~A/~A"
                                  course-name
                                  lab-name
                                  file-name)
                    (format nil "~A (~A)" (humanize (file-basename file)) file-ext)))))))))

;;handlers
(define-easy-handler (root :uri "/") ()
  (setf (content-type*) "text/html")
  (generate-index))

(defun course-paths (req)
  (scan "^/(advanced|intro)-rust/?$" (script-name* req)))
(define-easy-handler (course :uri #'course-paths)
    ()
  (setf (content-type*) "text/html")
  (let* ((uri         (script-name*))
         (course-name (car (split-sequence:split-sequence #\/ (string-trim '(#\/) uri)))))
    (generate-course course-name)))

(defun lab-paths (req)
  (scan "^/(advanced|intro)-rust/.+/?$" (script-name* req)))
(define-easy-handler (lab :uri #'lab-paths)
    ()
  (setf (content-type*) "text/html")
  (let* ((uri         (string-trim '(#\/) (script-name*)))
         (parts       (split-sequence:split-sequence #\/ uri))
         (course-name (first parts))
         (lab-name    (second parts)))
    (generate-lab course-name lab-name)))

(defun file-paths (req)
  (scan "\\.(pdf|org)$" (script-name* req)))
(define-easy-handler (file :uri #'file-paths)
    ()
  (let* ((uri       (script-name*))
         (file-path (merge-pathnames (subseq uri 1) *base-directory*)))
    (when (probe-file file-path)
      ;; TODO: Find out if it can be automatic
      (handle-static-file file-path))))

(defun static-paths (req)
  (scan "^/static/.+" (script-name* req)))
(define-easy-handler (static-files :uri #'static-paths)
    ()
  (let* ((uri           (script-name*))
         (relative-path (subseq uri 1)))
    (if (search ".." relative-path) ; security
        (setf (return-code*) 403)
        (let ((file-path (merge-pathnames relative-path *base-directory*)))
          (when (probe-file file-path)
            (handle-static-file file-path))))))

(defun start-server ()
  (setf *server*
        (make-instance 'easy-acceptor
                       :port *port*
                       :address "0.0.0.0"
                       :document-root *base-directory*))
  (start *server*)
  (format t "Server started on port ~A~%" *port*))

(defun stop-server ()
  (when *server*
    (stop *server*)
    (setf *server* nil)))

;;(start-server)
