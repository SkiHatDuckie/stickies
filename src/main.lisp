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
    :initform (error "Required field `width` not initialized."))
   (height
    :initarg :height
    :initform (error "Required field `height` not initialized."))))

(defmethod draw ((instance toolbar) &key &allow-other-keys)
  (with-pen
    (make-pen :stroke +blue+ :fill +white+ :weight 1)
    (rect 0
          0
          (slot-value instance 'width)
          (slot-value instance 'height))))

;;; Buttons

(defclass button ()
  ((image-resource
    :initarg :image-resource
    :initform nil
    :documentation "Image to be displayed on the button.")
   (width
    :initarg :width
    :initform nil
    :documentation "Width of the button.")
   (height
    :initarg :height
    :initform nil
    :documentation "Height of the button.")
   (x-offset
    :initarg :x-offset
    :initform 0
    :documentation "The x position of the top left corner of the button.")
   (y-offset
    :initarg :y-offset
    :initform 0
    :documentation "The y position of the top left corner of the button.")))

(defmethod initialize-instance :after ((instance button) &key)
  (with-slots (image-resource width height) instance
    (unless width
      (setf width (if image-resource
                      (slot-value image-resource 'sketch::width)
                      32)))
    (unless height
      (setf height (if image-resource
                       (slot-value image-resource 'sketch::height)
                       32)))))

(defmethod draw ((instance button) &key &allow-other-keys)
  (with-pen
    (make-pen :stroke +blue+ :fill +white+ :weight 1)
    (with-slots (image-resource width height x-offset y-offset) instance
      (rect x-offset y-offset width height)
      (when image-resource
        (image image-resource x-offset y-offset)))))

(defclass save-button (button) ())
(defclass load-button (button) ())
(defclass box-button (button) ())
(defclass arrow-button (button) ())
(defclass connector-button (button) ())

(defmethod on-hover ((instance button)))
(defmethod on-release ((instance button)))

;;; Snapgrid

;; Used when creating a new snapgrid
(defparameter *default-cell-width* 32)
(defparameter *default-grid-length* 50)
(defparameter *default-grid-width* 50)
(defparameter *default-x-offset* 0)
(defparameter *default-y-offset* 50)

(defclass snapgrid ()
  ((cell-width
    :initarg :cell-width
    :initform *default-cell-width*
    :documentation "The width (in pixels) of each grid cell.")
   (grid-length
    :initarg :grid-length
    :initform *default-grid-length*
    :documentation "The length (in cells) of the grid.")
   (grid-width
    :initarg :grid-width
    :initform *default-grid-width*
    :documentation "The width (in cells) of the grid.")
   (x-offset
    :initarg :x-offset
    :initform *default-x-offset*
    :documentation "The x position (in pixels) of the top left corner of the grid.")
   (y-offset
    :initarg :y-offset
    :initform *default-y-offset*
    :documentation "The y position (in pixels) of the top left corner of the grid.")))

(defmethod draw ((instance snapgrid) &key &allow-other-keys)
  (with-pen
    (make-pen :stroke (rgb-255 50 50 50) :fill +white+ :weight 1)
    (with-slots (cell-width grid-length grid-width x-offset y-offset) instance
      (dotimes (i grid-length)  ; Horizontal lines
        (line
          x-offset
          (+ (* i cell-width) y-offset)
          (+ (* grid-width cell-width) x-offset)
          (+ (* i cell-width) y-offset)))
      (dotimes (i grid-width)  ; Vertical lines
        (line
          (+ (* i cell-width) x-offset)
          y-offset
          (+ (* i cell-width) x-offset)
          (+ (* grid-length cell-width) y-offset))))))

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
     (snapgrid (make-instance 'snapgrid))
     (toolbar (make-instance 'toolbar :width width :height 50))
     (save-icon (load-resource "..\\assets\\StickiesSaveIcon.png")))
  (background +white+)
  (draw snapgrid)
  (draw toolbar)
  (let ((save-button (make-instance 'button :image-resource save-icon)))
    (draw save-button)))

(defun main ()
  (make-instance 'app))

(main)