;;;; timer.lisp
;;;; SDL_timer.h CFFI lisp wrapper

(in-package #:cl-sdl2)


(defcfun ("SDL_GetTicks" sdl-get-ticks) :uint32)

;; #define SDL_TICKS_PASSED(A, B)  ((Sint32)((B) - (A)) <= 0)

(defcfun ("SDL_GetPerformanceCounter" sdl-get-performance-counter) :uint64)

(defcfun ("SDL_GetPerformanceFrequency" sdl-get-performance-frequency) :uint64)

;; typedef Uint32 (SDLCALL * SDL_TimerCallback) (Uint32 interval, void *param);

(defctype sdl-timer-id :int
  "Definition of the timer ID type.")

;; extern DECLSPEC SDL_TimerID SDLCALL SDL_AddTimer(Uint32 interval,
;;                                                  SDL_TimerCallback callback,
;;                                                  void *param);

;; extern DECLSPEC SDL_bool SDLCALL SDL_RemoveTimer(SDL_TimerID id);

(defcfun ("SDL_Delay" sdl-delay) :void
  (ms :uint32))
