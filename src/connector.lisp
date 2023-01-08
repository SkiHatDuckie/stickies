;;;; Connectors

(in-package :stickies-system)

(defclass connector () ())

(defclass on-page-connector (connector) ())

(defclass off-page-connector (connector) ())