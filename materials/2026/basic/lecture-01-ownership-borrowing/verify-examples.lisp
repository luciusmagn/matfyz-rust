#!/usr/bin/env -S sbcl --script
(require :asdf)

(define-condition example-verification-error (error)
  ((message :initarg :message :reader verification-message))
  (:report (lambda (condition stream)
             (write-string (verification-message condition) stream))))

(defun fail (control &rest arguments)
  (error 'example-verification-error
         :message (apply #'format nil control arguments)))

(defun attribute (line name)
  (let ((start (search (format nil "~A=\"" name) line)))
    (when start
      (let* ((begin (+ start (length name) 2))
             (end (position #\" line :start begin)))
        (unless end (fail "Unclosed ~A attribute: ~A" name line))
        (subseq line begin end)))))

(defun command (arguments directory)
  (multiple-value-bind (output diagnostic status)
      (uiop:run-program arguments :directory directory
                        :output :string :error-output :string
                        :ignore-error-status t)
    (values status output diagnostic)))

(defun verify-example (opening code number line-number lesson heading toolchain root)
  (let* ((directory (merge-pathnames (format nil "example-~D/" number) root))
         (source (merge-pathnames "main.rs" directory))
         (binary (merge-pathnames "example" directory))
         (expected-error (attribute opening "data-error"))
         (expected-output (attribute opening "data-output")))
    (ensure-directories-exist source)
    (with-open-file (stream source :direction :output :if-exists :supersede)
      (write-string code stream))
    (multiple-value-bind (status output diagnostic)
        (command (list "rustup" "run" toolchain "rustc" "--edition=2021"
                       "--crate-name" "example" "main.rs" "-o" "example")
                 directory)
      (if expected-error
          (unless (and (not (zerop status)) (search expected-error diagnostic))
            (fail "~A:~D (~A): expected ~A, got compiler exit ~D.~%~A~A"
                  lesson line-number heading expected-error status output diagnostic))
          (progn
            (unless (zerop status)
              (fail "~A:~D (~A): compilation failed.~%~A~A"
                    lesson line-number heading output diagnostic))
            (multiple-value-bind (run-status run-output run-diagnostic)
                (command (list (namestring binary)) directory)
              (unless (zerop run-status)
                (fail "~A:~D (~A): execution failed.~%~A~A"
                      lesson line-number heading run-output run-diagnostic))
              (let ((actual (string-right-trim '(#\Return #\Linefeed) run-output)))
                (when (and expected-output (not (string= expected-output actual)))
                  (fail "~A:~D (~A): expected output ~S, got ~S."
                        lesson line-number heading expected-output actual)))))))))

(defun main ()
  (let* ((arguments (uiop:command-line-arguments))
         (toolchain (or (uiop:getenv "RUSTUP_TOOLCHAIN") "1.88.0"))
         (script-directory (uiop:pathname-directory-pathname *load-truename*))
         (path (find-if (lambda (argument) (not (uiop:string-prefix-p "--" argument)))
                        arguments))
         (lesson (if path (truename path) (merge-pathnames "lesson.md" script-directory)))
         (lines (coerce (uiop:read-file-lines lesson) 'vector))
         (root (uiop:ensure-directory-pathname
                (uiop:run-program '("mktemp" "-d" "-t" "ownership-examples.XXXXXX")
                                  :output '(:string :stripped t))))
         (heading "document")
         (examples 0))
    (unwind-protect
         (progn
           (dolist (argument arguments)
             (when (uiop:string-prefix-p "--toolchain=" argument)
               (setf toolchain (subseq argument 12))))
           (loop for index from 0 below (length lines)
                 for line = (aref lines index)
                 do (when (uiop:string-prefix-p "## " line)
                      (setf heading (subseq line 3)))
                    (when (and (uiop:string-prefix-p "```{.rust" line)
                               (search ".runnable" line))
                      (let ((closing (position "```" lines :start (1+ index) :test #'string=)))
                        (unless closing
                          (fail "~A:~D (~A): unclosed Rust example." lesson (1+ index) heading))
                        (incf examples)
                        (verify-example
                         line (format nil "~{~A~%~}" (coerce (subseq lines (1+ index) closing) 'list))
                         examples (1+ index) lesson heading toolchain root)
                        (setf index closing))))
           (when (zerop examples) (fail "No runnable Rust examples in ~A." lesson))
           (format t "Verified ~D Rust examples with toolchain ~A.~%" examples toolchain))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(handler-case (main)
  (error (condition)
    (format *error-output* "verify-examples: ~A~%" condition)
    (uiop:quit 1)))
