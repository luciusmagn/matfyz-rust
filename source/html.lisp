(in-package :matfyz)

(defmacro with-page ((&key title) &body body)
  "Generate a complete HTML page with standard layout."
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:meta :charset "UTF-8")
       (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
       (:title ,title)
       (:style (:raw (page--get-css))))
      (:body
       (:div :class "container"
             ,@body
             ,(page--render-footer))))))

(-> page--get-css () string)
(defun page--get-css ()
  "Return the CSS styles for the page."
  "@font-face {
     font-family: \"TT Livret\";
     src: url(\"/static/TTLivretTextRegular.woff2\") format(\"woff2\");
     font-weight: normal;
     font-style: normal;
   }
   * { font-family: \"TT Livret\"; }
   body {
     font-family: serif;
     margin: 0;
     padding: 20px;
     background: black;
     color: white;
   }
   a { color: #DDD; }
   .container { max-width: 800px; margin: 0 auto; }
   .footer { margin-top: 40px; }")

(defun page--render-footer ()
  "Render the page footer."
  `(:div :class "footer"
         (:p "Maintained by " (:strong *author*))
         (:p "Contacts: "
             (dolist (contact *contacts*)
               (:span (:strong (car contact)) ": " (cadr contact) " ")))
         (:p (:a :href *department-url* 
                 "Department of Distributed and Dependable Systems"))))

(-> page-generate-index () string)
(defun page-generate-index ()
  "Generate the main index page."
  (with-page (:title "Rust @ MATFYZ CUNI")
    (:h1 "Rust @ MatFyz CUNI")
    (:p "It is a good language, you should learn it. "
        "Here are the course materials and some other resources.")
    (:h2 "Course Materials")
    (:ul
     (dolist (dir (directory-list-subdirs *base-directory*))
       (let ((dir-name (directory-get-name dir)))
         (when (or (string= dir-name "advanced-rust")
                   (string= dir-name "intro-rust"))
           (:li (:a :href (format nil "/~A/" dir-name)
                    (name-humanize dir-name)))))))
    (:h2 "Rust Learning Resources")
    (:ul 
     (dolist (resource *rust-resources*)
       (:li (:a :href (cadr resource) (car resource)))))))

(-> page-generate-course (string) string)
(defun page-generate-course (course-name)
  "Generate the page for a specific course."
  (let ((course-path (merge-pathnames 
                      (make-pathname :directory (list :relative course-name))
                      *base-directory*)))
    (with-page (:title (name-humanize course-name))
      (:h1 (name-humanize course-name))
      (:p (:a :href "/" "← Back"))
      (:h2 "Labs")
      (:ul
       (dolist (lab-dir (directory-list-subdirs course-path))
         (let ((lab-name (directory-get-name lab-dir)))
           (:li (:a :href (format nil "/~A/~A/" course-name lab-name)
                    (name-humanize lab-name)))))))))

(-> page-generate-lab (string string) string)
(defun page-generate-lab (course-name lab-name)
  "Generate the page for a specific lab."
  (let ((lab-path (merge-pathnames
                   (make-pathname :directory (list :relative course-name lab-name))
                   *base-directory*)))
    (with-page (:title (format nil "~A - ~A" 
                               (name-humanize course-name) 
                               (name-humanize lab-name)))
      (:h1 (format nil "~A - ~A" 
                   (name-humanize course-name) 
                   (name-humanize lab-name)))
      (:p (:a :href (format nil "/~A/" course-name) "← Back"))
      (:h2 "Materials")
      (:ul
       (dolist (file (directory-list-files lab-path))
         (let ((file-name (file-namestring file))
               (extension (string-downcase (subseq (file-get-extension file) 1))))
           (:li (:a :href (format nil "/~A/~A/~A"
                                  course-name
                                  lab-name
                                  file-name)
                    (format nil "~A (~A)" 
                            (name-humanize (file-get-basename file)) 
                            extension)))))))))
