;;;; Snapgrid

(in-package :stickies-system)

(defclass snapgrid ()
  ((cell-width :initarg :cell-width
               :initform 32
               :accessor cell-width
               :documentation "The width (in pixels) of each grid cell.")
   (grid-height :initarg :grid-height
                :initform 50
                :accessor grid-height
                :documentation "The height (in cells) of the grid.")
   (grid-width :initarg :grid-width
               :initform 50
               :accessor grid-width
               :documentation "The width (in cells) of the grid.")
   (x-offset :initarg :x-offset
             :initform 0
             :accessor x-offset
             :documentation "The x position (in pixels) of the top left corner of the grid.")
   (y-offset :initarg :y-offset
             :initform 0
             :accessor y-offset
             :documentation "The y position (in pixels) of the top left corner of the grid.")
   (boxes :initarg :boxes
          :initform (make-array 5 :fill-pointer 0 :adjustable t)
          :accessor boxes
          :documentation "Vector of boxes to be drawn on the snapgrid.")))

(defmethod add-box ((instance snapgrid) (new-box box))
  (with-accessors ((boxes boxes)) instance
    (vector-push-extend new-box boxes)
    (format t "~A~%" boxes)))

(defmethod draw ((instance snapgrid) &key &allow-other-keys)
  (with-pen
    (make-pen :stroke (rgb-255 50 50 50) :fill +white+ :weight 1)
    (with-accessors ((cell-width cell-width)
                     (grid-height grid-height)
                     (grid-width grid-width)
                     (x-offset x-offset)
                     (y-offset y-offset)
                     (boxes boxes)) instance
      (dotimes (i grid-height)  ; Horizontal lines
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
          (+ (* grid-height cell-width) y-offset)))
      (dotimes (i (length boxes))
        (draw (elt boxes i) :cell-width cell-width)))))

(defvar *snapgrid* (make-instance 'snapgrid :y-offset 50))