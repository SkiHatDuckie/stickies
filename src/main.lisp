;;;; The entrypoint is located here

(in-package :stickies-system)

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
     (save-button)
     (load-button)
     (box-button)
     (arrow-button)
     (connector-button)
     (terminal-box-button)
     (process-box-button)
     (decision-box-button)
     (input-output-box-button)
     (comment-box-button)
     (predefined-process-box-button)
     (back-button)
     (save-icon (load-resource "assets\\SaveIcon.png"))
     (load-icon (load-resource "assets\\LoadIcon.png"))
     (box-icon (load-resource "assets\\BoxIcon.png"))
     (arrow-icon (load-resource "assets\\ArrowIcon.png"))
     (connector-icon (load-resource "assets\\ConnectorIcon.png"))
     (terminal-box-icon (load-resource "assets\\TerminalBoxIcon.png"))
     (process-box-icon (load-resource "assets\\ProcessBoxIcon.png"))
     (decision-box-icon (load-resource "assets\\DecisionBoxIcon.png"))
     (input-output-box-icon (load-resource "assets\\InputOutputBoxIcon.png"))
     (comment-box-icon (load-resource "assets\\CommentBoxIcon.png"))
     (predefined-process-box-icon (load-resource "assets\\PredefinedProcessBoxIcon.png"))
     (back-icon (load-resource "assets\\BackIcon.png")))
  (background +white+)
  (draw snapgrid)
  (draw *current-toolbar*))

(defmethod setup ((window app) &key &allow-other-keys)
  (with-slots (width
               save-button save-icon
               load-button load-icon
               box-button box-icon
               arrow-button arrow-icon
               connector-button connector-icon
               terminal-box-button terminal-box-icon
               process-box-button process-box-icon
               decision-box-button decision-box-icon
               input-output-box-button input-output-box-icon
               comment-box-button comment-box-icon
               predefined-process-box-button predefined-process-box-icon
               back-button back-icon) window 
    (setf save-button (make-instance 'save-button :surface-image save-icon
                                                  :border-color +blue+
                                                  :width 50
                                                  :height 50))
    (setf load-button (make-instance 'load-button :surface-image load-icon
                                                  :border-color +blue+
                                                  :width 50
                                                  :height 50))
    (setf box-button (make-instance 'box-button :surface-image box-icon
                                                :border-color +blue+
                                                :width 50
                                                :height 50))
    (setf arrow-button (make-instance 'arrow-button :surface-image arrow-icon
                                                    :border-color +blue+
                                                    :width 50
                                                    :height 50))
    (setf connector-button (make-instance 'connector-button :surface-image connector-icon
                                                            :border-color +blue+
                                                            :width 50
                                                            :height 50))
    (setf terminal-box-button (make-instance 'terminal-box-button :surface-image terminal-box-icon
                                                                  :border-color +blue+
                                                                  :width 50
                                                                  :height 50))
    (setf process-box-button (make-instance 'process-box-button :surface-image process-box-icon
                                                                :border-color +blue+
                                                                :width 50
                                                                :height 50))
    (setf decision-box-button (make-instance 'decision-box-button :surface-image decision-box-icon
                                                                  :border-color +blue+
                                                                  :width 50
                                                                  :height 50))
    (setf input-output-box-button
      (make-instance 'input-output-box-button :surface-image input-output-box-icon
                                              :border-color +blue+
                                              :width 50
                                              :height 50))
    (setf comment-box-button (make-instance 'comment-box-button :surface-image comment-box-icon
                                                               :border-color +blue+
                                                               :width 50
                                                               :height 50))
    (setf predefined-process-box-button
      (make-instance 'predefined-process-box-button :surface-image predefined-process-box-icon
                                                    :border-color +blue+
                                                    :width 50
                                                    :height 50))
    (setf back-button (make-instance 'back-button :surface-image back-icon
                                                  :border-color +blue+
                                                  :width 50
                                                  :height 50))
    (setf *file-toolbar* (make-instance 'toolbar :border-color +blue+
                                                 :width width
                                                 :height 50
                                                 :widgets (vector save-button
                                                                  load-button
                                                                  box-button
                                                                  arrow-button
                                                                  connector-button)))
    (setf *box-toolbar* (make-instance 'toolbar :border-color +blue+
                                                :width width
                                                :height 50
                                                :widgets (vector back-button
                                                                 terminal-box-button
                                                                 process-box-button
                                                                 decision-box-button
                                                                 input-output-box-button
                                                                 comment-box-button
                                                                 predefined-process-box-button))))
  (setf *current-toolbar* *file-toolbar*))

;; `(unless (< timestamp 1000)) was added to allow some time
;; for resources to load before checking events.
;; TODO: Get rid of the need to check this by creating a loading period during app startup.
(defmethod kit.sdl2:mousemotion-event ((window app) timestamp state x y xrel yrel)
  (unless (< timestamp 1000)
    (with-accessors ((widgets widgets)) *current-toolbar*
      (dotimes (i (length widgets))
          (when (subtypep (type-of (elt widgets i)) 'button)
            (on-motion (elt widgets i) x y))))))

;; Same situation as above
(defmethod kit.sdl2:mousebutton-event ((window app) state timestamp button x y)
  (unless (< timestamp 1000)
    (when (and (eql state :mousebuttonup) (eql button 1))
      (with-accessors ((widgets widgets)) *current-toolbar*
        (dotimes (i (length widgets))
            (when (subtypep (type-of (elt widgets i)) 'button)
              (on-release (elt widgets i))))))))

(defun main ()
  (make-instance 'app))

(main)