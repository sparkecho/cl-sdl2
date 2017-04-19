;;;; video.lisp
;;;; SDL_video.h CFFI lisp wrapper

(in-package #:cl-sdl2)

(defconstant SDL-WINDOWPOS-UNDEFINED-MASK #x1FFF0000)
(defconstant SDL-WINDOWPOS-UNDEFINED      #x1FFF0000)

(defcenum sdl-window-flags
  (SDL-WINDOW-FULLSCREEN    #x00000001) ; fullscreen window
  (SDL-WINDOW-OPENGL        #x00000002) ; window usable with OpenGL context
  (SDL-WINDOW-SHOWN         #x00000004) ; window is visible
  (SDL-WINDOW-HIDDEN        #x00000008) ; window is not visible
  (SDL-WINDOW-BORDERLESS    #x00000010) ; no window decoration
  (SDL-WINDOW-RESIZABLE     #x00000020) ; window can be resized
  (SDL-WINDOW-MINIMIZED     #x00000040) ; window is minimized
  (SDL-WINDOW-MAXIMIZED     #x00000080) ; window is maximized
  (SDL-WINDOW-INPUT_GRABBED #x00000100) ; window has grabbed input focus
  (SDL-WINDOW-INPUT_FOCUS   #x00000200) ; window has input focus
  (SDL-WINDOW-MOUSE_FOCUS   #x00000400) ; window has mouse focus
  ;; (SDL_WINDOW_FULLSCREEN_DESKTOP ( SDL_WINDOW_FULLSCREEN | 0x00001000 ))
  (SDL-WINDOW-FOREIGN       #x00000800) ; window not created by SDL
  (SDL-WINDOW-ALLOW_HIGHDPI #x00002000)) ; window should be created in high-DPI mode if supported



;; Argument flags is enum type, so foreign-funcall is used
;; (defun sdl-create-window (title x y w h flags)
;;   (foreign-funcall "SDL_CreateWindow"
;;                    :string title
;;                    :int x
;;                    :int y
;;                    :int w
;;                    :int h
;;                    sdl-window-flags flags
;;                    :pointer))
(defcfun ("SDL_CreateWindow" sdl-create-window) :pointer
  (title :string)
  (x :int)
  (y :int)
  (w :int)
  (h :int)
  (flags :uint32))


(defcfun ("SDL_GetWindowSurface" sdl-get-window-surface) :pointer
  (window :pointer))

(defcfun ("SDL_UpdateWindowSurface" sdl-update-window-surface) :int
  (window :pointer))

(defcfun ("SDL_DestroyWindow" sdl-destroy-window) :void
  (window :pointer))
