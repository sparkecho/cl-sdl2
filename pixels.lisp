;;;; pixels.lisp
;;;; SDL_pixels.h CFFI lisp wrapper

(in-package #:cl-sdl2)


(defcfun ("SDL_MapRGB" sdl-map-rgb) :uint32
  (format :pointer)                     ;(const SDL_PixelFormat *)
  (r :uint8)
  (g :uint8)
  (b :uint8))
