;;;; Boxes

(in-package :stickies-system)

(defclass box () ())

(defclass terminal-box (box) ())

(defclass process-box (box) ())

(defclass decision-box (box) ())

(defclass input-output-box (box) ())

(defclass comment-box (box) ())

(defclass predefined-process-box (box) ())