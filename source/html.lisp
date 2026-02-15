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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (-> course-display-name (string) string)
  (defun course-display-name (course-name)
    "Return human-facing name for known course directories."
    (cond
      ((string= course-name "basic-rust") "Introductory Rust (old)")
      ((string= course-name "advanced-rust") "Advanced Rust (old)")
      (t (name-humanize course-name))))

  (-> course-unit-title (string) string)
  (defun course-unit-title (unit-name)
    "Return label for a course unit (lecture/lab directory)."
    (if (scan "^lab-" unit-name)
        (format nil "Lab: ~A" (name-humanize unit-name))
        (format nil "Lecture: ~A" (name-humanize unit-name))))

  (-> sort-pathnames-by-name ((list-of pathname)) (list-of pathname))
  (defun sort-pathnames-by-name (paths)
    "Sort PATHS by file/directory name."
    (sort (copy-list paths)
          #'string<
          :key #'file-namestring))

  (defun page--render-footer ()
    "Render the page footer."
    `(:div :class "footer"
           (:p "Maintained by " (:strong *author*))
           (:h3 "Contacts")
           (dolist (contact *contacts*)
             (:p (:strong (car contact)) ": " (cadr contact) " "))
           (:p (:a :href *department-url* 
                   "Department of Distributed and Dependable Systems"))))

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
     .footer { margin-top: 40px; }"))

(-> page-generate-index () string)
(defun page-generate-index ()
  "Generate the main index page."
  (with-page (:title "Rust @ MATFYZ CUNI")
    (:h1 "Rust @ MatFyz CUNI")
    (:p "It is a good language, you should learn it. "
        "Here are the course materials and some other resources.")
    (:h2 "Legacy Course Materials")
    (:ul
     (dolist (dir (directory-list-subdirs *base-directory*))
       (let ((dir-name (directory-get-name dir)))
         (when (or (string= dir-name "advanced-rust")
                   (string= dir-name "basic-rust"))
           (:li (:a :href (format nil "/~A/" dir-name)
                    (course-display-name dir-name)))))))
    (:h2 "2026 Course Resources")
    (:ul
     (:li (:a :href "https://d3s.mff.cuni.cz/teaching/rust"
              "Introductory Rust (2026)"))
     (:li (:a :href "https://d3s.mff.cuni.cz/teaching/morerust"
              "Advanced Rust (2026)"))
     (:li (:a :href "https://matfyz.lho.sh"
              "Legacy materials mirror (matfyz.lho.sh)")))
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
    (with-page (:title (course-display-name course-name))
      (:h1 (course-display-name course-name))
      (:p (:a :href "/" "← Back"))
      (:h2 "Material Units")
      (:ul
       (dolist (unit-dir (sort (copy-list (directory-list-subdirs course-path))
                               #'string<
                               :key #'directory-get-name))
         (let ((unit-name (directory-get-name unit-dir)))
           (:li (:a :href (format nil "/~A/~A/" course-name unit-name)
                    (course-unit-title unit-name))))))
      (let ((root-files (sort-pathnames-by-name (directory-list-files course-path))))
        (when root-files
          (:h2 "General Files")
          (:ul
           (dolist (file root-files)
             (let ((file-name (file-namestring file))
                   (extension (string-downcase (subseq (file-get-extension file) 1))))
               (:li (:a :href (format nil "/~A/~A" course-name file-name)
                        (format nil "~A (~A)"
                                (name-humanize (file-get-basename file))
                                extension)))))))))))

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
