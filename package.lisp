;;;; package.lisp

(defpackage #:cl-sdl2
  (:use #:cl #:cffi))

(in-package :cl-sdl2)

(define-foreign-library libsdl2
  (:darwin (:or (:framework "SDL2")
                (:default "libSDL2")))
  (:windows "SDL2.dll")
  (:unix (:or "libSDL2-2.0.so.0.2.0"
              "libSDL2-2.0.so.0.2"
              "libSDL2-2.0.so.0"
              "libSDL2-2.0.so"
	      "libSDL2")))

(use-foreign-library libsdl2)
