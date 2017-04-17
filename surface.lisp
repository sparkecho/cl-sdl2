;;;; surface.lisp
;;;; SDL_surface.h CFFI lisp wrapper

(in-package #:cl-sdl2)

(defcstruct sdl-surface
  (flags :uint32)      ; Read-only
  (format :pointer)    ; Read-only  (SDL_PixelFormat *)
  (w :int)             ; Read-only
  (h :int)             ; Read-only
  (pitch :int)         ; Read-only
  (pixels :pointer)    ; Read-write
  ;; Application data associated with the surface
  (userdata :pointer)                   ; Read-write
  ;; information needed for surfaces requiring locks
  (locked :int)        ; Read-only
  (lock-data :pointer) ; Read-only
  ;; clipping information
  (clip-rect (:struct sdl-rect))        ;Read-only
  ;; info for fast blit mapping to other surfaces
  (map :pointer)       ;Private (struct SDL_BlitMap *)  (:pointer (:struct sdl-blit-map))
  ;; Reference count -- used when freeing surface
  (refcount :int))                      ; Read-mostly


(defcfun ("SDL_FillRect" sdl-fill-rect) :int
  (dst :pointer)                        ;(SDL_Surface *)
  (rect :pointer)                       ;(const SDL_Rect *)
  (color :uint32))


(defcfun ("SDL_FillRects" sdl-fill-rects) :int
  (dst :pointer)                        ;(SDL_Surface *)
  (rects :pointer)                      ;(const SDL_Rect *)
  (count :int)
  (color :uint32))
