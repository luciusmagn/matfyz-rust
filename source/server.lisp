(in-package :matfyz)

;;;; -- Request Handlers --
(define-easy-handler (root :uri "/") ()
  (setf (content-type*) "text/html")
  (page-generate-index))

(defun handler--course-paths (request)
  "Check if request matches course paths."
  (scan "^/(advanced|basic)-rust/?$" (script-name* request)))

(define-easy-handler (course :uri #'handler--course-paths) ()
  (setf (content-type*) "text/html")
  (let* ((uri         (script-name*))
         (course-name (car (split-sequence #\/ (string-trim '(#\/) uri)))))
    (page-generate-course course-name)))

(defun handler--lab-paths (request)
  "Check if request matches lab paths."
  (scan "^/(advanced|basic)-rust/.+/?$" (script-name* request)))

(define-easy-handler (lab :uri #'handler--lab-paths) ()
  (setf (content-type*) "text/html")
  (let* ((uri         (string-trim '(#\/) (script-name*)))
         (parts       (split-sequence #\/ uri))
         (course-name (first parts))
         (lab-name    (second parts)))
    (page-generate-lab course-name lab-name)))

(defun handler--file-paths (request)
  "Check if request matches file paths."
  (scan "\\.(pdf|org|typ)$" (script-name* request)))

(define-easy-handler (file :uri #'handler--file-paths) ()
  (let* ((uri       (script-name*))
         (file-path (merge-pathnames (subseq uri 1) *base-directory*)))
    (when (probe-file file-path)
      (handle-static-file file-path))))

(defun handler--static-paths (request)
  "Check if request matches static file paths."
  (scan "^/static/.+" (script-name* request)))

(define-easy-handler (static-files :uri #'handler--static-paths) ()
  (let* ((uri           (script-name*))
         (relative-path (subseq uri 1)))
    (if (search ".." relative-path) ; I don't trust hunchentoot that much on security
        (setf (return-code*) 403)
        (let ((file-path (merge-pathnames relative-path *base-directory*)))
          (when (probe-file file-path)
            (handle-static-file file-path))))))

;;;; -- Server Management --

(-> start-server () t)
(defun start-server ()
  "Start the web server on configured port."
  (setf *server*
        (make-instance 'easy-acceptor
                       :port           *port*
                       :address        "0.0.0.0"
                       :document-root  *base-directory*))
  (start *server*)
  (format t "Server started on port ~A~%" *port*)
  t)

(-> stop-server () t)
(defun stop-server ()
  "Stop the running web server."
  (aif *server*
       (progn
         (stop it)
         (setf *server* nil)
         (format t "Server stopped~%"))
       (format t "No server running~%"))
  t)
