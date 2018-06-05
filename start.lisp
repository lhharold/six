;; (ql:quickload :caveman2)
;; (asdf:clear-source-registry)
;; (push #p"~/work/webserver/" asdf:*central-registry*)
;; (asdf:load-system :webserver)
;; (webserver:start :port 8080)

;; (in-package :webserver.web)

;; (defroute "/welcome" (&key (|name| "Guest"))
;;   (render #P"index.html")
;;   ;; (format nil "Welcome, ~A" |name|)
;;   )
;; (defroute "/hello" (&key (|name| "Guest"))
;;   (render #P"hello.html")
;;   ;; (format nil "Welcome, ~A" |name|)
;;   )



;;test
(defun clamp (x lowerlimit upperlimit)
  (if (< x lowerlimit)
      lowerlimit
      (if (> x upperlimit)
          upperlimit
          x)))

(defun smoothstep (edge0 edge1 x)
  ;;scale, bias and saturate x to 0..1 range
  (let ((r (clamp (/ (- x edge0) (- edge1 edge0)) 0.0 1.0)))
    ;;Evaluate polynomial
    (* r r (- 3 (* 2 r)))))

(defun smootherstep (edge0 edge1 x)
  ;;scale, and clamp x to 0..1 range)
  (let ((r (clamp (/ (- x edge0) (- edge1 edge0)) 0.0 1.0)))
    (* r r r (+ 10 (* r (- (* r 6) 15))))))

(defun pascal-triangle (a b)
  ;; return binomail coefficient without explicit use of factorials.
  ;; which can't be used with negative integers
  (let ((result 1))
    (dotimes (i b)
      (setf result (* result (/ (- a i) (+ i 1)))))
    result))

(defun general-smoothstep (n x)
  (let ((clamp-x (clamp x 0 1))
        (result 0))
    (dotimes (i (+ n 1))
      (incf result (* (pascal-triangle (- -1 n) i)
                      (pascal-triangle (+ 1 (* 2 n)) (- n i))
                      (expt clamp-x (+ n i 1)))))
    result))


(defun liner-interpolation (a b r)
  (+ (* a (- 1 r)) (* b r)))
(defun smoothstep-interpolation (a b r)
  (let ((v (smoothstep 0 1 r)))
    (liner-interpolation a b v)))
(defun smootherstep-interpolation (a b r)
  (let ((v (smootherstep 0 1 r)))
    (liner-interpolation a b v)))
(defun squared-interpolation (a b r)
  (let ((v (* r r)))
    (liner-interpolation a b v)))
(defun invsquared-interpolation (a b r)
  (let ((v (- 1 (* (- 1 r) (- 1 r)))))
    (liner-interpolation a b v)))
(defun sin-interpolation (a b r)
  (let ((v (sin (* r pi 0.5))))
    (liner-interpolation a b v)))

(defun test()
  (let* ((start 3)
         (end 4)
         (speed 5.0)
         (total-time (/ (- end start) speed))
         (cur 0.0)
         (i 0))
    (do ((cur-time 0.0 (+ cur-time 0.05)))
        ((= cur end) 'done)
      (setq cur (liner-interpolation start end (clamp (/ cur-time total-time) 0.0 1.0)))
      (format t "~a " cur)
      (when (= (mod i 5) 0)
        (format t "~%"))
      (incf i))))
