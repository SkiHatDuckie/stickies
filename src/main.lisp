;;;; main.lisp. The entrypoint is located here.
;;;; TEMP: Run using `sbcl --load main.lisp`

(asdf:load-system :cffi)

;; libffi is required for loading sketch
(cffi:define-foreign-library libffi
  (t (:default "libffi-7")))

(cffi:use-foreign-library libffi)

(ql:quickload :sketch)

(sketch:defsketch run-app ()
  (sketch:rect 100 100 200 200))

(defun main ()
  (make-instance `run-app))

(main)