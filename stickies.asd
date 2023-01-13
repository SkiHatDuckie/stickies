;;;; Stickies system definition
;;;; Load system using `sbcl --load stickies.asd`, then `(asdf:load-system :stickies)`

(asdf:defsystem :stickies
  :name "Stickies"
  :description "Small desktop application to help with structuring larger programming projects."
  :author "SkiHatDuckie"
  :license "MIT"
  :version "0.0.1"
  :depends-on (#:cffi
               #:sketch)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "ui-surface")
               (:file "toolbar")
               (:file "box")
               (:file "snapgrid")
               (:file "arrow")
               (:file "connector")
               (:file "button")
               (:file "main")))