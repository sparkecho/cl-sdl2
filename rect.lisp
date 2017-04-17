;;;; rect.lisp
;;;; SDL_rect.h CFFI lisp wrapper

(in-package #:cl-sdl2)

(defcstruct %sdl-rect
  (x :int)
  (y :int)
  (w :int)
  (h :int))

(defctype sdl-rect (:struct %sdl-rect))
