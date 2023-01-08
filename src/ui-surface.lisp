;;;; UI surface

(in-package :stickies-system)

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