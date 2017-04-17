;;;; events.lisp
;;;; SDL_events.h CFFI lisp wrapper

(in-package #:cl-sdl2)

;; SDL_stdinc.h
;; SDL_error.h
;; SDL_video.h
;; SDL_keyboard.h
;; SDL_mouse.h
;; SDL_joystick.h
;; SDL_gamecontroller.h
;; SDL_quit.h
;; SDL_gesture.h
;; SDL_touch.h
;; begin_code.h


(defconstant sdl-release 0 "General keyboard/mouse state definitions")
(defconstant sdl-pressed 1 "General keyboard/mouse state definitions")


(defcenum sdl-event-type
    (:SDL_FIRSTEVENT     0)             ; Unused (do not remove)
    ;; Application events
    (:SDL_QUIT           #x100)         ; User-requested quit
    ;; These application events have special meaning on iOS, see README-ios.txt for details
    (:SDL_APP_TERMINATING) ; The application is being terminated by the OS
                                        ; Called on iOS in applicationWillTerminate()
                                        ; Called on Android in onDestroy()
    (:SDL_APP_LOWMEMORY) ; The application is low on memory, free memory if possible.
                                        ; Called on iOS in applicationDidReceiveMemoryWarning()
                                        ; Called on Android in onLowMemory()
    (:SDL_APP_WILLENTERBACKGROUND) ;The application is about to enter the background
                                        ;Called on iOS in applicationWillResignActive()
                                        ; Called on Android in onPause()
    (:SDL_APP_DIDENTERBACKGROUND) ; The application did enter the background and may not get CPU for some time
                                        ; Called on iOS in applicationDidEnterBackground()
                                        ; Called on Android in onPause()

    (:SDL_APP_WILLENTERFOREGROUND) ; The application is about to enter the foreground
                                        ; Called on iOS in applicationWillEnterForeground()
                                        ; Called on Android in onResume()

    (:SDL_APP_DIDENTERFOREGROUND) ; The application is now interactive
                                        ; Called on iOS in applicationDidBecomeActive()
                                        ; Called on Android in onResume()
    ;; Window events
    (:SDL_WINDOWEVENT    #x200)         ; Window state change
    (:SDL_SYSWMEVENT)                   ; System specific event

    ;; Keyboard events
    (:SDL_KEYDOWN        #x300)  ; Key pressed
    (:SDL_KEYUP)                 ; Key released
    (:SDL_TEXTEDITING)           ; Keyboard text editing (composition)
    (:SDL_TEXTINPUT)             ; Keyboard text input

    ;; Mouse events
    (:SDL_MOUSEMOTION    #x400)         ; Mouse moved
    (:SDL_MOUSEBUTTONDOWN)              ; Mouse button pressed
    (:SDL_MOUSEBUTTONUP)                ; Mouse button released
    (:SDL_MOUSEWHEEL)                   ; Mouse wheel motion

    ;; Joystick events
    (:SDL_JOYAXISMOTION  #x600)         ; Joystick axis motion
    (:SDL_JOYBALLMOTION)                ; Joystick trackball motion
    (:SDL_JOYHATMOTION)                 ; Joystick hat position change
    (:SDL_JOYBUTTONDOWN)                ; Joystick button pressed
    (:SDL_JOYBUTTONUP)                  ; Joystick button released
    (:SDL_JOYDEVICEADDED)               ; A new joystick has been inserted into the system
    (:SDL_JOYDEVICEREMOVED)             ; An opened joystick has been removed

    ;; Game controller events
    (:SDL_CONTROLLERAXISMOTION  #x650)  ; Game controller axis motion
    (:SDL_CONTROLLERBUTTONDOWN)      ; Game controller button pressed
    (:SDL_CONTROLLERBUTTONUP)        ; Game controller button released
    (:SDL_CONTROLLERDEVICEADDED) ; A new Game controller has been inserted into the system
    (:SDL_CONTROLLERDEVICEREMOVED) ; An opened Game controller has been removed
    (:SDL_CONTROLLERDEVICEREMAPPED) ; The controller mapping was updated

    ;; Touch events
    (:SDL_FINGERDOWN      #x700)
    (:SDL_FINGERUP)
    (:SDL_FINGERMOTION)

    ;; Gesture events
    (:SDL_DOLLARGESTURE   #x800)
    (:SDL_DOLLARRECORD)
    (:SDL_MULTIGESTURE)

    ;; Clipboard events
    (:SDL_CLIPBOARDUPDATE #x900)        ; The clipboard changed

    ;; Drag and drop events
    (:SDL_DROPFILE        #x1000)    ; The system requests a file open

    ;; Render events
    (:SDL_RENDER_TARGETS_RESET #x2000) ; The render targets have been reset

    ;; Events ::SDL_USEREVENT through ::SDL_LASTEVENT are for your use,
    ;; and should be allocated with SDL_RegisterEvents()
    (:SDL_USEREVENT    #x8000)

    ;; This last event is only for bounding internal arrays
    (:SDL_LASTEVENT    #xFFFF))


;; Fields shared by every event
(defcstructype sdl-common-event
  (type      :uint32)
  (timestamp :uint32))


;; Window state change event data (event.window.*)
(defcstructype sdl-window-event
  (type :uint32)                        ; ::SDL_WINDOWEVENT
  (timestamp :uint32)
  (windowID :uint32)                    ; The associated window
  (event :uint8)                        ; ::SDL_WindowEventID
  (padding1 :uint8)
  (padding2 :uint8)
  (padding3 :uint8)
  (data1 :int32)                        ; event dependent data
  (data2 :int32))                       ; event dependent data
;; line 175


;; line 449
;; The "quit requested" event
(defcstructype sdl-quit-event
  (type      :uint32)                   ; ::SDL_QUIT
  (timestamp :uint32))
;; line 455


;; line 498
(defcuniontype sdl-event
    (type     :uint32)                  ; Event type, shared with all events
    (common   sdl-common-event)         ; Common event data
    (window   sdl-window-event)         ; Window event data
    ;; (key      SDL_KeyboardEvent)        ; Keyboard event data
    ;; (edit     SDL_TextEditingEvent)     ; Text editing event data
    ;; (text     SDL_TextInputEvent)       ; Text input event data
    ;; (motion   SDL_MouseMotionEvent)     ; Mouse motion event data
    ;; (button   SDL_MouseButtonEvent)     ; Mouse button event data
    ;; (wheel    SDL_MouseWheelEvent)      ; Mouse wheel event data
    ;; (jaxis    SDL_JoyAxisEvent)         ; Joystick axis event data
    ;; (jball    SDL_JoyBallEvent)         ; Joystick ball event data
    ;; (jhat     SDL_JoyHatEvent)          ; Joystick hat event data
    ;; (jbutton  SDL_JoyButtonEvent)       ; Joystick button event data
    ;; (jdevice  SDL_JoyDeviceEvent)       ; Joystick device change event data
    ;; (caxis    SDL_ControllerAxisEvent)  ; Game Controller axis event data
    ;; (cbutton  SDL_ControllerButtonEvent); Game Controller button event data
    ;; (cdevice  SDL_ControllerDeviceEvent); Game Controller device event data
    (quit     sdl-quit-event)           ; Quit request event data
    ;; (user     SDL_UserEvent)            ; Custom event data
    ;; (syswm    SDL_SysWMEvent)           ; System dependent window event data
    ;; (tfinger  SDL_TouchFingerEvent)     ; Touch finger event data
    ;; (mgesture SDL_MultiGestureEvent)    ; Gesture event data
    ;; (dgesture SDL_DollarGestureEvent)   ; Gesture event data
    ;; (drop     SDL_DropEvent)            ; Drag and drop event data

    ;; This is necessary for ABI compatibility between Visual C++ and GCC
    ;; Visual C++ will respect the push pack pragma and use 52 bytes for
    ;; this structure, and GCC will use the alignment of the largest datatype
    ;; within the union, which is 8 bytes.
    ;; So... we'll add padding to force the size to be 56 bytes for both.
    (padding :uint8 :count 56))
;; line 554


;; line 598
(defcfun ("SDL_PollEvent" sdl-poll-event) :int
  (event :pointer))                     ;(SDL_Event *)
;; line 599
