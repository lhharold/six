(defun hello-1()
  (with-ltk()
    (let ((b (make-instance 'button
                            :master nil
                            :text "prss me"
                            :command (lambda ()
                                       (format t "hello world!~&")))))
      (pack b))))

(defun hello-2()
  (with-ltk()
    (let* ((f (make-instance 'frame))
           (b1 (make-instance 'button
                              :master f
                              :text "Button 1"
                              :command (lambda () (format t "button 1~&"))))
           (b2 (make-instance 'button
                              :master f
                              :text "button 2"
                              :command (lambda () (format t "button 2~&")))))
      (pack f)
      (pack b1 :side :left)
      (pack b2 :side :left)
      (configure f :borderwidth 30)
      (configure f :relief :solid))))

(defun canvas-test()
  (with-ltk()
    (let* ((sc (make-instance 'scrolled-canvas))
           (c (canvas sc))
           (line (create-line c (list 100 100 400 50 700 150)))
           (polygon (create-polygon c (list 50 150 250 160 250 300 50 330)))
           (text (create-text c 260 250 "Canvas test")))
      (pack sc :expand 1 :fill :both)
      (scrollregion c 0 0 800 800))))

(defun scribble()
  (with-ltk()
    (let* ((canvas (make-instance 'canvas))
           (down nil))
      (pack canvas)
      (bind canvas "<ButtonPress-1>"
            (lambda (evt)
              (setf down t)
              (create-oval canvas
                           (- (event-x evt) 10) (- (event-y evt) 10)
                           (+ (event-x evt) 10) (- (event-y evt) 10))))
      (bind canvas "<ButtonRelease-1>" (lambda (evt)
                                        (declare (ignore evt))
                                        (setf down nil)))
      (bind canvas "<Motion>"
            (lambda (evt)
              (when down
                (create-oval canvas
                             (- (event-x evt) 10) (- (event-y evt) 10)
                             (+ (event-x evt) 10) (- (event-y evt) 10))))))))
`
