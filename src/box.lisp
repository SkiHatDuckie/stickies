;;;; Boxes

(in-package :stickies-system)

(defclass box ()
  ((height :initform 1
           :accessor height
           :documentation "Height (in cells) of the box rect.")
   (width :initform 1
          :accessor width
          :documentation "Width (in cells) of the box rect.")
   (surface-color :initarg :surface-color
                  :initform +white+
                  :accessor surface-color
                  :documentation "Color of the surface of the box.")
   (border-color :initarg :border-color
                 :initform +black+
                 :accessor border-color
                 :documentation "Color of the border of the box.")
   (box-text :initarg :text
             :initform "Text goes here"
             :accessor box-text
             :documentation "Text to be rendered onto the box surface")))

(defmethod draw ((instance box) &key cell-width &allow-other-keys))

(defclass terminal-box (box) ())

(defclass process-box (box) ())

(defmethod draw :after ((instance process-box) &key cell-width &allow-other-keys)
  (with-accessors ((height height)
                   (width width)
                   (box-text box-text)
                   (surface-color surface-color)
                   (border-color border-color)) instance
    (with-pen
      (make-pen :stroke surface-color :fill border-color :weight 1)
      (rect 60
            60
            (* width cell-width)
            (* height cell-width)))))

(defclass decision-box (box) ())

(defclass input-output-box (box) ())

(defclass comment-box (box) ())

(defclass predefined-process-box (box) ())