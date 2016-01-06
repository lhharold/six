(defpackage :hello-system
  (:use #:asdf #:cl))

(in-package :hello-system)

(defsystem hello
  :name "hello world"
  :version "0.1"
  :author "lh"
  :depends-on ()
  :components ((:file "package")
               (:file "hello" :depends-on ("package"))))
