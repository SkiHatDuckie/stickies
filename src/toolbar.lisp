;;;; Toolbar

(in-package :stickies-system)

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

(defmethod draw :after ((instance toolbar) &key &allow-other-keys)
  (with-accessors ((widgets widgets)) instance
    (dotimes (i (length widgets))
      (draw (elt widgets i)))))

(defvar *file-toolbar* (make-instance 'toolbar))
(defvar *box-toolbar* (make-instance 'toolbar))
(defvar *current-toolbar* nil)  ; Make sure this is set to something before it's drawn!