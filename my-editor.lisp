(defpackage my-editor
  (:use capi))

(capi:define-interface button-test()
  ()
  (:panes
   (pubsdaf
    :text "Push"
    :data :push-button
    )
   (dsafs
    capi:check-button
    :text "check"
    :data :check-button))
  (:layouts
   (row
    capi:row-layout
    '(push-button check-button)))
  (:default-initargs
   :layout 'default-layout
   :title "button test"))

(defun test-buttons ()
  (let ((first (capi:display (make-instance 'button-test))))
    (multiple-value-bind (x y width beight)
        (capi:top-level-interface-geometry first)
      (declare (ignore x with height)))))
