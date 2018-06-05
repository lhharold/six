(in-package :hello-system)

(defpackage hello
  (:nicknames hello)
  (:use #:cl)
  (:export main *default-name*))
