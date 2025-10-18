;;;; src/package.lisp
(defpackage :matfyz
  (:use #:cl #:spinneret :hunchentoot)
  (:import-from :cl-ppcre
   :scan)
  (:import-from :uiop
   :getcwd)
  (:import-from :split-sequence
   :split-sequence)
  (:import-from :anaphora
   :aif
                :it)
  (:import-from :serapeum
   :->)
  (:export :start-server
   :stop-server))
