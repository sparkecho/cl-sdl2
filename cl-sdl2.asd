;;;; cl-sdl2.asd

(asdf:defsystem #:cl-sdl2
  :description "CFFI lisp wrapper of SDL2."
  :author "sparkecho"
  :license "GPL-v3"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")

               (:file "timer")

               (:file "pixels")
               (:file "rect")
               (:file "surface")
               (:file "video")

               (:file "sdl")
               (:file "cl-sdl2")))

