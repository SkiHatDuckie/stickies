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
                  :accessor surface-image
                  :documentation "Image to be drawn on the surface.")
   (surface-color :initarg :surface-color
                  :initform +white+
                  :accessor surface-color
                  :documentation "Color of the surface.")
   (border-color :initarg :border-color
                 :initform +black+
                 :accessor border-color
                 :documentation "Color of the surface's border.")
   (width :initarg :width
          :initform 0
          :accessor width
          :documentation "Width of the surface.")
   (height :initarg :height
           :initform 0
           :accessor height
           :documentation "Height of the surface.")
   (x-offset :initarg :x-offset
             :initform 0
             :accessor x-offset
             :documentation "The x position of the top left corner of the surface.")
   (y-offset :initarg :y-offset
             :initform 0
             :accessor y-offset
             :documentation "The y position of the top left corner of the surface.")))

(defmethod mouse-over-surface-p ((instance ui-surface) mouse-x mouse-y)
  (with-slots (width height x-offset y-offset) instance
    (and (< x-offset mouse-x (+ x-offset width))
         (< y-offset mouse-y (+ y-offset height)))))

(defmethod draw ((instance ui-surface) &key &allow-other-keys)
  (with-slots (surface-image surface-color border-color width height x-offset y-offset) instance
    (with-pen
      (make-pen :stroke border-color :fill surface-color :weight 1)
      (rect x-offset y-offset width height)
      (when surface-image
        (image surface-image x-offset y-offset)))))

;;; Toolbar

(defclass toolbar (ui-surface)
  ((widgets :initarg :widgets
            :initform (vector)
            :accessor widgets
            :documentation #.(format nil "List of ui-surface objects contained in toolbar.
                                          The order of items in the list is the order they'll
                                          be rendered, going from left to right."))))

(defmethod reposition-widgets ((instance toolbar))
  (with-accessors ((widgets widgets)) instance
    (let ((next-open-x-pos 0))
      (dotimes (i (length widgets))
        (with-accessors ((x-offset x-offset) (width width)) (elt widgets i)
          (setf x-offset next-open-x-pos)
          (incf next-open-x-pos width))))))

(defmethod initialize-instance :after ((instance toolbar) &key)
  (reposition-widgets instance))

;;; Buttons

(defclass button (ui-surface)
  ((is-hovered :initform nil
               :accessor is-hovered
               :documentation "If button's surface is being hovered over or not.")
   (hover-color :initform (rgb-255 0 0 0 50)
                :accessor hover-color
                :documentation "Color drawn over the button's surface when hovered.")))

(defmethod on-motion ((instance button) mouse-x mouse-y)
  (with-accessors ((is-hovered is-hovered)) instance
    (if (mouse-over-surface-p instance mouse-x mouse-y)
      (setf is-hovered t)
      (setf is-hovered nil))))

(defmethod on-release ((instance button)))

(defmethod draw :after ((instance button) &key &allow-other-keys)
  (with-accessors ((width width)
                   (height height)
                   (x-offset x-offset)
                   (y-offset y-offset)
                   (is-hovered is-hovered)
                   (hover-color hover-color)) instance
    (with-pen
      (make-pen :stroke hover-color :fill hover-color :weight 1)
      (when is-hovered
        (rect x-offset y-offset width height)))))

(defclass save-button (button) ())
(defclass load-button (button) ())

(defclass box-button (button) ())

(defmethod on-release :after ((instance box-button))
  )

(defclass arrow-button (button) ())
(defclass connector-button (button) ())

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
     (toolbar)
     (save-button)
     (load-button)
     (box-button)
     (arrow-button)
     (connector-button)
     (save-icon (load-resource "..\\assets\\StickiesSaveIcon.png"))
     (load-icon (load-resource "..\\assets\\StickiesLoadIcon.png"))
     (box-icon (load-resource "..\\assets\\StickiesBoxIcon.png"))
     (arrow-icon (load-resource "..\\assets\\StickiesArrowIcon.png"))
     (connector-icon (load-resource "..\\assets\\StickiesConnectorIcon.png")))
  (background +white+)
  (draw snapgrid)
  (draw toolbar)
  (draw save-button)
  (draw load-button)
  (draw box-button)
  (draw arrow-button)
  (draw connector-button))

(defmethod setup ((window app) &key &allow-other-keys)
  (with-slots (toolbar
               width
               save-button save-icon
               load-button load-icon
               box-button box-icon
               arrow-button arrow-icon
               connector-button connector-icon) window 
    (setf save-button (make-instance 'button :surface-image save-icon
                                             :border-color +blue+
                                             :width 50
                                             :height 50))
    (setf load-button (make-instance 'button :surface-image load-icon
                                             :border-color +blue+
                                             :width 50
                                             :height 50))
    (setf box-button (make-instance 'button :surface-image box-icon
                                            :border-color +blue+
                                            :width 50
                                            :height 50))
    (setf arrow-button (make-instance 'button :surface-image arrow-icon
                                              :border-color +blue+
                                              :width 50
                                              :height 50))
    (setf connector-button (make-instance 'button :surface-image connector-icon
                                                  :border-color +blue+
                                                  :width 50
                                                  :height 50))
    (setf toolbar (make-instance 'toolbar :border-color +blue+
                                          :width width
                                          :height 50
                                          :widgets (vector save-button
                                                           load-button
                                                           box-button
                                                           arrow-button
                                                           connector-button)))))

(defmethod kit.sdl2:mousemotion-event ((window app) timestamp state x y xrel yrel)
  (with-slots (toolbar) window
    (with-accessors ((widgets widgets)) toolbar
      (dotimes (i (length widgets))
        (when (eq 'button (type-of (elt widgets i)))
          (on-motion (elt widgets i) x y))))))

(defun main ()
  (make-instance 'app))

(main)