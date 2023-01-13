;;;; Buttons

(in-package :stickies-system)

(defclass button (ui-surface)
  ((hover :initform nil
          :accessor hover
          :documentation "If button's surface is being hovered over or not.")
   (hover-color :initform (rgb-255 0 0 0 50)
                :accessor hover-color
                :documentation "Color drawn over the button's surface when hovered.")))

(defmethod on-motion ((instance button) mouse-x mouse-y)
  (with-accessors ((hover hover)) instance
    (if (mouse-over-surface-p instance mouse-x mouse-y)
      (setf hover t)
      (setf hover nil))))

(defmethod on-release ((instance button)))

(defmethod draw :after ((instance button) &key &allow-other-keys)
  (with-accessors ((width width)
                   (height height)
                   (x-offset x-offset)
                   (y-offset y-offset)
                   (hover hover)
                   (hover-color hover-color)) instance
    (with-pen
      (make-pen :stroke hover-color :fill hover-color :weight 1)
      (when hover
        (rect x-offset y-offset width height)))))

(defclass save-button (button) ())

(defclass load-button (button) ())

(defclass box-button (button) ())

(defmethod on-release :after ((instance box-button))
  (with-accessors ((hover hover)) instance
    (when hover
      (setf *current-toolbar* *box-toolbar*))))

(defclass arrow-button (button) ())

(defclass connector-button (button) ())

(defclass terminal-box-button (button) ())

(defclass process-box-button (button) ())

(defmethod on-release :after ((instance process-box-button))
  (with-accessors ((hover hover)) instance
    (when hover
      (add-box *snapgrid* (make-instance 'process-box :surface-color +green+
                                                      :border-color +yellow+)))))

(defclass decision-box-button (button) ())

(defclass input-output-box-button (button) ())

(defclass comment-box-button (button) ())

(defclass predefined-process-box-button (button) ())

(defclass back-button (button) ())

(defmethod on-release :after ((instance back-button))
  (with-accessors ((hover hover)) instance
    (when hover
      (setf *current-toolbar* *file-toolbar*))))