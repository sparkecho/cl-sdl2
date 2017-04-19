;;;; error.lisp
;;;; SDL_error.h CFFI lisp wrapper

(in-package #:cl-sdl2)


;; extern DECLSPEC int SDLCALL SDL_SetError(const char *fmt, ...);

(defcfun ("SDL_GetError" sdl-get-error) :string)

(defcfun ("SDL_ClearError" sdl-clear-error) :void)

;; #define SDL_OutOfMemory()   SDL_Error(SDL_ENOMEM)
;; #define SDL_Unsupported()   SDL_Error(SDL_UNSUPPORTED)
;; #define SDL_InvalidParamError(param)    SDL_SetError("Parameter '%s' is invalid", (param))


(defcenum sdl-error-code
  (SDL-ENOMEM)
  (SDL-EF-READ)
  (SDL-EF-WRITE)
  (SDL-EF-SEEK)
  (SDL-UNSUPPORTED)
  (SDL-LAST-ERROR))

(defcfun ("SDL_Error" sdl-error) :int
  (code sdl-error-code))
