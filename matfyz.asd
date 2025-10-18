(defsystem "matfyz"
  :description "Web server for Rust programming course materials at MatFyz CUNI"
  :author "Lukáš Hozda <luk.hozda@gmail.com>"
  :license "COLL"
  :version "0.1.0"
  :serial t
  :depends-on (:hunchentoot
               :spinneret
               :cl-fad
               :cl-ppcre
               :uiop
               :split-sequence
               :serapeum
               :anaphora)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "types")
                             (:file "config")
                             (:file "filesystem")
                             (:file "html")
                             (:file "server")))))
