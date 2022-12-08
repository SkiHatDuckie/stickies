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

;;; UI Surface

(defclass ui-surface ()
  ((surface-image :initarg :surface-image
                  :initform nil
                  :documentation "An image to be rendered on to the surface.")
   (surface-color :initarg :surface-color
                  :initform +white+
                  :documentation "Color of the surface.")
   (border-color :initarg :border-color
                 :initform +black+
                 :documentation "Color of the surface's border.")
   (width :initarg :width
          :initform 0
          :documentation "Width of the surface.")
   (height :initarg :height
           :initform 0
           :documentation "Height of the surface.")
   (x-offset :initarg :x-offset
             :initform 0
             :documentation "The x position of the top left corner of the surface.")
   (y-offset :initarg :y-offset
             :initform 0
             :documentation "The y position of the top left corner of the surface.")))

(defmethod draw ((instance ui-surface) &key &allow-other-keys)
  (with-slots (surface-image surface-color border-color width height x-offset y-offset) instance
    (with-pen
      (make-pen :stroke border-color :fill surface-color :weight 1)
      (rect x-offset y-offset width height)
      (when surface-image
        (image surface-image x-offset y-offset)))))

;;; Toolbar

(defclass toolbar (ui-surface) ())

;;; Buttons

(defclass button (ui-surface) ())

(defclass save-button (button) ())
(defclass load-button (button) ())
(defclass box-button (button) ())
(defclass arrow-button (button) ())
(defclass connector-button (button) ())

(defmethod on-hover ((instance button)))
(defmethod on-release ((instance button)))

;;; Snapgrid

(defclass snapgrid ()
  ((cell-width :initarg :cell-width
               :initform 32
               :documentation "The width (in pixels) of each grid cell.")
   (grid-length :initarg :grid-length
                :initform 50
                :documentation "The length (in cells) of the grid.")
   (grid-width :initarg :grid-width
               :initform 50
               :documentation "The width (in cells) of the grid.")
   (x-offset :initarg :x-offset
             :initform 0
             :documentation "The x position (in pixels) of the top left corner of the grid.")
   (y-offset :initarg :y-offset
             :initform 0
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
     (snapgrid (make-instance 'snapgrid :y-offset 50))
     (toolbar (make-instance 'toolbar :border-color +blue+ :width width :height 50))
     (save-icon (load-resource "..\\assets\\StickiesSaveIcon.png"))
     (load-icon (load-resource "..\\assets\\StickiesLoadIcon.png"))
     (box-icon (load-resource "..\\assets\\StickiesBoxIcon.png"))
     (arrow-icon (load-resource "..\\assets\\StickiesArrowIcon.png"))
     (connector-icon (load-resource "..\\assets\\StickiesConnectorIcon.png")))
  (background +white+)
  (draw snapgrid)
  (draw toolbar)
  (let ((save-button (make-instance 'button :surface-image save-icon :border-color +blue+
                      :width 50 :height 50)))
    (draw save-button))
  (let ((load-button (make-instance 'button :surface-image load-icon :border-color +blue+
                      :width 50 :height 50 :x-offset 50)))
    (draw load-button))
  (let ((box-button (make-instance 'button :surface-image box-icon :border-color +blue+
                      :width 50 :height 50 :x-offset 100)))
    (draw box-button))
  (let ((arrow-button (make-instance 'button :surface-image arrow-icon :border-color +blue+
                      :width 50 :height 50 :x-offset 150)))
    (draw arrow-button))
  (let ((connector-button (make-instance 'button :surface-image connector-icon :border-color +blue+
                      :width 50 :height 50 :x-offset 200)))
    (draw connector-button)))

(defun main ()
  (make-instance 'app))

(main)