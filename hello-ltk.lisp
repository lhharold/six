(defpackage :hello-ltk
  (:use :common-lisp :ltk)
  (:export #:main))

(in-package :hello-ltk)

(defun main()
  (setf *debug-tk* nil)
  (with-ltk ()
    (let ((b (make-instance 'button
                            :text "Hello World"
                            :command (lambda()
                                       (do-msg "Bye!")
                                       (setf *exit-mainloop* t)))))
      (pack b))))


;; sbcl --eval "(progn (compile-file \"C:/Users/Anny/AppData/Roaming/quicklisp/dists/quicklisp/software/ltk-20150113-http/ltk\") (load \"C:/Users/Anny/AppData/Roaming/quicklisp/dists/quicklisp/software/ltk-20150113-http/ltk\") (compile-file \"e:/work/six/hello-ltk\") (load \"e:/work/six/hello-ltk\") (save-lisp-and-die \"e:/work/six/hello-ltk.core\"))"

;; sbcl --core hello-world.core --noinform --eval "(progn (hello-ltk:main) (quit))"
