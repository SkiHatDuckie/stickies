;;;; main.lisp. The entrypoint is located here.
;;;; Run using `sbcl --load main.lisp`

;;; Dependencies

(asdf:load-system :cffi)

(cffi:define-foreign-library libffi  ; Required for loading sketch.
  (t (:default "libffi-7")))

(cffi:use-foreign-library libffi)

(ql:quickload :sketch)

(defpackage :stickies (:use :cl :sketch))
(in-package :stickies)

;;; Toolbar

(defclass toolbar () 
  ((width
    :initarg :width
    :initform (error "Field `width` not initialized."))
   (height
    :initarg :height
    :initform (error "Field `height` not initialized."))))

(defmethod draw ((instance toolbar) &key &allow-other-keys)
  (with-pen
    (make-pen :stroke +blue+ :fill +white+ :weight 1)
    (rect 0
          0
          (slot-value instance 'width)
          (slot-value instance 'height))))

;;; Buttons

(defclass button () ())
(defclass save-button (button) ())
(defclass load-button (button) ())
(defclass box-button (button) ())
(defclass arrow-button (button) ())
(defclass connector-button (button) ())

(defmethod on-hover ((instance button)))
(defmethod on-release ((instance button)))

;;; Snapgrid

(defclass snapgrid () ())

;;; Boxes

(defclass box () ())
(defclass terminal-box (box) ())
(defclass process-box (box) ())
(defclass decision-box (box) ())
(defclass input-output-box (box) ())
(defclass comment-box (box) ())
(defclass predefined-process-box (box) ())

;;; Arrow

(defclass arrow () ())

;;; Connector

(defclass connector () ())
(defclass on-page-connector (connector) ())
(defclass off-page-connector (connector) ())

;;; Stickies App

(defsketch app 
    ((title "Stickies") (width 600) (height 400)
    (toolbar (make-instance 'toolbar :width width :height 50)))
  (draw toolbar))

(defun main ()
  (make-instance 'app))

(main)