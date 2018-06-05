(in-package ltk)

(defun hello()
  (with-ltk()
    (let ((b (make-instance 'button
                            :master nil
                            :text "hello"
                            :command (lambda()
                                       (format t "hello~&")))))
      (pack b))))
