(asdf:load-system :cffi)

(cffi:define-foreign-library libffi
    (t (:default "libffi-7")))

(cffi:use-foreign-library libffi)

(ql:quickload :sketch)