;;;; Lazy Foo' SDL Tutorial 01-hello-sdl.lisp
(in-package :cl-sdl2)

(defun hello-sdl ()
  (let ((window (null-pointer))
        (screen-surface (null-pointer))
        (screen-width 640)
        (screen-height 480))
    (cond ((< (sdl-init SDL-INIT-VIDEO) 0)
           (format t "SDL could not initialize! sdl-error: ~A~%" (sdl-get-error)))
          (t (setf window (sdl-create-window "SDL Tutorial"
                                             SDL-WINDOWPOS-UNDEFINED
                                             SDL-WINDOWPOS-UNDEFINED
                                             screen-width
                                             screen-height
                                             SDL-WINDOW-SHOWN))
             (cond ((null-pointer-p window)
                    (format t "Window could not be created! sdl-error: ~A~%" (sdl-get-error)))
                   (t (setf screen-surface (sdl-get-window-surface window))
                      (sdl-fill-rect screen-surface
                                     (null-pointer)
                                     (sdl-map-rgb (sdl-surface-format screen-surface) #xFF #xFF #xFF))
                      (sdl-update-window-surface window)
                      (sdl-delay 2000)))))
    (sdl-destroy-window window)
    (sdl-quit)))
