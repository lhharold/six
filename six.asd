;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(defpackage :six
  (:use #:cl #:asdf))

(in-package :six)

(defsystem six-sys
  :name "six"
  :version "0.0.1"
  :components ((:file "lisptest")))

