(defpackage "MYTEST"
  (:add-use-defaults t)
  (:use "CAPI"))

(in-package "MYTEST")

(defun message (&rest args)
  (display-message "hello world!"))

(display (make-instance 'interface
               :visible-min-width 200
               :background :blue
               :foreground :yellow
               :title "my interface"))

(contain
 (make-instance 'title-pane
                :text "a large piece of text"
                :font (gp:make-font-description
                       :family "Times"
                       :size 32
                       :weight :medium
                       :slant :roman)))

(contain (make-instance 'push-button
                        :data "Button"))

(contain (make-instance 'push-button
                        :data "hello world"
                        :mnemonic #\h
                        :callback
                        #'(lambda (&rest args)
                                 (display-message
                                  "hello world"))))

;; title pane
(contain
 (make-instance 'title-pane
                :visible-min-width 200
                :text "Title"))
;; title for element
(setq button
 (make-instance 'push-button
                :text "hello"
                :title "Press"
                :title-position :left
                :callback 'message))
(contain button)
(apply-in-pane-process
 button #'(setf titled-object-title) "Press here: " button)
(apply-in-pane-process
 button #'(setf titled-object-title-font)
 (gp:merge-font-descriptions
  (gp:make-font-description :size 42)
  (gp:convert-to-font-description
   button
   (titled-object-title-font button)))
 button)


;;display-pane
(contain
 (make-instance 'display-pane
                :text "this is a display pane"))

;; text-input-pane
(contain
 (make-instance 'text-input-pane
                :title "Search: "
                :buttons
                (list :cancel t
                      :ok nil
                      :browse-file
                      (list :operation :open
                            :filter "*.lisp;*.lsp"))))
;; editor-pane
(contain
 (make-instance 'editor-pane
                :text "hello"))
(contain (make-instance 'collector-pane
                        :title "exmample collector pane: "))
(contain (make-instance 'listener-pane
                        :title "exmample collector pane: "))
