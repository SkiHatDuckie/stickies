;;;; Snapgrid

(in-package :stickies-system)

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