;;;; rwops.lisp
;;;; SDL_rwops.h CFFI lisp wrapper

(in-package #:cl-sdl2)


;; (SDL_RWops *)
(defcfun ("SDL_RWFromFile" sdl-rw-from-file) :pointer
  (file :string)
  (mode :string))
