;;;; 02-getting-an-image-on-the-screen.lisp

(in-package :cl-sdl2)

(defconstant +screen-width+ 640)
(defconstant +screen-height+ 480)

(defparameter *window* (null-pointer))
(defparameter *screen-surface* (null-pointer))
(defparameter *hello-world* (null-pointer))


(defun init ()
  (let ((success t))
    (cond ((< (sdl-init SDL-INIT-VIDEO) 0)
           (format t "SDL could not initialize! SDL_Error: ~A~%" (sdl-get-error))
           (setf success nil))
          (t (setf *window* (sdl-create-window "SDL Tutorial"
                                               SDL-WINDOWPOS-UNDEFINED SDL-WINDOWPOS-UNDEFINED
                                               +screen-width+ +screen-height+
                                               :SDL-WINDOW-SHOWN))
             (cond ((null-pointer-p *window*)
                    (format t "Window could not be created! SDL_Error: ~A~%" (sdl-get-error))
                    (setf success nil))
                   (t (setf *screen-surface* (sdl-get-window-surface *window*))))))
    success))


(defun load-media ()
  (let ((success t)
        (full-path (namestring (merge-pathnames "examples/hello_world.bmp" (asdf:system-source-directory :cl-sdl2)))))
    (setf *hello-world* (sdl-load-bmp full-path))
    (when (null-pointer-p *hello-world*)
      (format t "Unable to load image ~S! SDL Error: ~A~%" "hello_world.bmp" (sdl-get-error))
      (setf success nil))
    success))


;; Name `close' conflicts with lisp built-in.
(defun close-all ()
  ;; Deallocate surface
  (sdl-free-surface *hello-world*)
  (setf *hello-world* (null-pointer))
  ;; Destroy window
  (sdl-destroy-window *window*)
  (setf *window* (null-pointer))
  ;; Quit SDL subsystem
  (sdl-quit))


(defun main ()
  (cond ((null (init)) (format t "Failed to initialize!~%"))
        (t (cond ((null (load-media)) (format t "Failed to load media!~%"))
                 ;; ;;Apply the image
                 (t (sdl-blit-surface *hello-world* (null-pointer) *screen-surface* (null-pointer))
                    ;; Update the surface
                    (sdl-update-window-surface *window*)
                    ;; Wait two seconds
                    (sdl-delay 2000)))))
  ;; Free resources and close SDL
  (close-all))
                    
