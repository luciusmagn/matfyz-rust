(in-package :matfyz)

(defparameter *base-directory* (getcwd)
  "Base directory for serving files")

(defparameter *port* 27777
  "Port the server listens on")

(defparameter *server* nil
  "Current server instance")

(defparameter *author* "Lukáš Hozda (LHO)"
  "Course author name")

(defparameter *contacts*
  '(("Discord" "@magnusi")
    ("Email"   "lukas.hozda@braiins.cz, luk.hozda@gmail.com"))
  "Contact information alist")

(defparameter *department-url* "https://d3s.mff.cuni.cz/"
  "Department website URL")

(defparameter *rust-resources*
  '(("Rust Book"       "https://doc.rust-lang.org/book/")
    ("Rust By Example" "https://doc.rust-lang.org/rust-by-example/")
    ("Rustlings"       "https://github.com/rust-lang/rustlings")
    ("Rust Reference"  "https://doc.rust-lang.org/reference/"))
  "External Rust learning resources")
