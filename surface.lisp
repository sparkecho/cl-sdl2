;;;; surface.lisp
;;;; SDL_surface.h CFFI lisp wrapper

(in-package #:cl-sdl2)

(defconstant SDL-SWSURFACE 0)          ; Just here for compatibility
(defconstant SDL-PREALLOC  #x00000001) ; Surface uses preallocated memory
(defconstant SDL-RLEACCEL  #x00000002) ; Surface is RLE encoded
(defconstant SDL-DONTFREE  #x00000004) ; Surface is referenced internally


(defcstruct %sdl-surface
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
  (clip-rect sdl-rect) ;Read-only
  ;; info for fast blit mapping to other surfaces
  (map :pointer)       ;Private (struct SDL_BlitMap *)  (:pointer (:struct sdl-blit-map))
  ;; Reference count -- used when freeing surface
  (refcount :int))                      ; Read-mostly

(defctype sdl-surface (:struct %sdl-surface))


(defcfun ("SDL_FreeSurface" sdl-free-surface) :void
  (surface :pointer))                   ;(SDL_Surface *)


;SDL_Surface *
(defcfun ("SDL_LoadBMP_RW" sdl-load-bmp-rw) :pointer
  (src :pointer)                        ;SDL_RWops *
  (freesrc :int))

;; #define SDL_LoadBMP(file)   SDL_LoadBMP_RW(SDL_RWFromFile(file, "rb"), 1)
(defun sdl-load-bmp (file)
  (sdl-load-bmp-rw (sdl-rw-from-file file "rb") 1))


(defcfun ("SDL_FillRect" sdl-fill-rect) :int
  (dst :pointer)                        ;(SDL_Surface *)
  (rect :pointer)                       ;(const SDL_Rect *)
  (color :uint32))


(defcfun ("SDL_FillRects" sdl-fill-rects) :int
  (dst :pointer)                        ;(SDL_Surface *)
  (rects :pointer)                      ;(const SDL_Rect *)
  (count :int)
  (color :uint32))



(defcfun ("SDL_UpperBlit" sdl-upper-blit) :int
  (src :pointer)                        ; (SDL_Surface *)
  (srcrect :pointer)                    ; (const SDL_Rect *)
  (dst :pointer)                        ; (SDL_Surface *)
  (dstrect :pointer))                   ; (SDL_Rect *)

;; #define SDL_BlitSurface SDL_UpperBlit
(defun sdl-blit-surface (src srcrect dst dstrect)
  (sdl-upper-blit src srcrect dst dstrect))

(defcfun ("SDL_LowerBlit" sdl-lower-blit) :int
  (src :pointer)                        ; (SDL_Surface *)
  (srcrect :pointer)                    ; (const SDL_Rect *)
  (dst :pointer)                        ; (SDL_Surface *)
  (dstrect :pointer))                   ; (SDL_Rect *)
