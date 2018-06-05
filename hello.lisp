(in-package :hello)  

(defvar *default-name* "world")  

(defun main (args)  
  (if (null args)  
      (format t "hello ~a~%" *default-name*)  
      (hello args)))  

(defun hello(names)  
  (when names  
    (format t "hello ~a~%" (car names))  
    (hello (cdr names))))  
