;;;; sdl.lisp
;;;; SDL CFFI lisp wrapper

;;; Main include header for the SDL library

;; "SDL_main.h"
;; "SDL_stdinc.h"
;; "SDL_assert.h"
;; "SDL_atomic.h"
;; "SDL_audio.h"
;; "SDL_clipboard.h"
;; "SDL_cpuinfo.h"
;; "SDL_endian.h"
;; "SDL_error.h"
;; "SDL_events.h"
;; "SDL_filesystem.h"
;; "SDL_joystick.h"
;; "SDL_gamecontroller.h"
;; "SDL_haptic.h"
;; "SDL_hints.h"
;; "SDL_loadso.h"
;; "SDL_log.h"
;; "SDL_messagebox.h"
;; "SDL_mutex.h"
;; "SDL_power.h"
;; "SDL_render.h"
;; "SDL_rwops.h"
;; "SDL_system.h"
;; "SDL_thread.h"
;; "SDL_timer.h"
;; "SDL_version.h"
;; "SDL_video.h"
;; "begin_code.h"


(in-package #:cl-sdl2)

(defconstant SDL-INIT-TIMER          #x00000001)
(defconstant SDL-INIT-AUDIO          #x00000010)
(defconstant SDL-INIT-VIDEO          #x00000020)  ; SDL_INIT_VIDEO implies SDL_INIT_EVENTS
(defconstant SDL-INIT-JOYSTICK       #x00000200)  ; SDL_INIT_JOYSTICK implies SDL_INIT_EVENTS
(defconstant SDL-INIT-HAPTIC         #x00001000)  ;
(defconstant SDL-INIT-GAMECONTROLLER #x00002000)  ; SDL_INIT_GAMECONTROLLER implies SDL_INIT_JOYSTICK
(defconstant SDL-INIT-EVENTS         #x00004000)  ;
(defconstant SDL-INIT-NOPARACHUTE    #x00100000)  ; Don't catch fatal signals
(defconstant SDL-INIT-EVERYTHING     #x0000FFFF)


(defcfun ("SDL_Init" sdl-init) :int
  (flags :uint32))

(defcfun ("SDL_InitSubSystem" sdl-init-subsystem) :int
  (flags :uint32))

(defcfun ("SDL_QuitSubSystem" sdl-quit-subsystem) :void
  (flags :uint32))

(defcfun ("SDL_WasInit" sdl-was-init) :uint32
  (flags :uint32))

(defcfun ("SDL_Quit" sdl-quit) :void)
