;;;; cl-poker-sim.asd

(asdf:defsystem #:cl-poker-sim
  :author "JRP"
  :version "0.1"
  :serial t
  :depends-on (#:cl-poker-eval)
  :components ((:file "package")
               (:file "cl-poker-sim")))

