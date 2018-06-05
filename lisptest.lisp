(in-package :CL-USER)

(defun hello-world () (format t "hello, world!!!"))

(defvar *db* nil)

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a: ~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped")))

(defun add-record (cd)
  (push cd *db*))

;;;add cd by input in console
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another?")) (return))))

;;;save db to file
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
     (print *db* out))))

;;;read db from file
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;;(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
;;(remove-if-not #'(lambda (x) (= 1 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))

;; (defun select-by-artist (artist)
;;   (remove-if-not
;;    #'(lambda (cd) (equal (getf cd :artist) artist))
;;    *db*))
;; (defun artist-selector (artist)
;;   #'(lambda (cd) (equal (getf cd :artist) artist)))
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))
;; (defun where (&key title artist rating (ripped nil ripped-p))
;;   #'(lambda (cd)
;;       (and
;;        (if title  (equal (getf cd :title) title) t)
;;        (if artist (equal (getf cd :artist) artist) t)
;;        (if rating (equal (getf cd :rating) rating) t)
;;        (if ripped-p (equal (getf cd :ripped) ripped) t))))
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comaprisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comaprisons-list clauses))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title  (setf (getf row :title) title))
               (if artist (setf (getf row :artist) artist))
               (if rating (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))
(defun delete-row (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))




;;*************************************
(defvar *test-name* nil)
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))
(defmacro with-gensyms2 ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))
(defmacro combine-results (&body forms)
  (with-gensyms2 (result)
    `(let ((,result t))
           ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
           ,result)))
(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)
(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(deftest test-*()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 14)))
(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))
(deftest test-math ()
  (test-arithmetic))

