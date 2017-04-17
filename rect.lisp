;;;; rect.lisp
;;;; SDL_rect.h CFFI lisp wrapper

(in-package #:cl-sdl2)

(defcstructype sdl-rect
  (x :int)
  (y :int)
  (w :int)
  (h :int))
