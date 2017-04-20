;;;; 03_event_driven_programming.lisp

(in-package :cl-sdl2)


(defconstant +screen-width+ 640)
(defconstant +screen-height+ 480)

(defparameter *window* (null-pointer))
(defparameter *screen-surface* (null-pointer))
(defparameter *x-out* (null-pointer))


;; while loop
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))



(defun init ()
  (let ((success t))
    (cond ((< (sdl-init SDL-INIT-VIDEO) 0)
           (format t "SDL could not initialize! SDL_Error: ~A~%" (sdl-get-error))
           (setf success nil))
          (t (setf *window* (sdl-create-window "SDL Tutorial"
                                               SDL-WINDOWPOS-UNDEFINED SDL-WINDOWPOS-UNDEFINED
                                               +screen-width+ +screen-height+
                                               SDL-WINDOW-SHOWN))
             (cond ((null-pointer-p *window*)
                    (format t "Window could not be created! SDL_Error: ~A~%" (sdl-get-error))
                    (setf success nil))
                   (t (setf *screen-surface* (sdl-get-window-surface *window*))))))
    success))


(defun load-media ()
  (let ((success t)
        (full-path (namestring (merge-pathnames "examples/x.bmp" (asdf:system-source-directory :cl-sdl2)))))
    (setf *x-out* (sdl-load-bmp full-path))
    (when (null-pointer-p *x-out*)
      (format t "Unable to load image ~S! SDL Error: ~A~%" "x.bmp" (sdl-get-error))
      (setf success nil))
    success))


;; Name `close' conflicts with lisp built-in.
(defun close-all ()
  ;; Deallocate surface
  (sdl-free-surface *x-out*)
  (setf *x-out* (null-pointer))
  ;; Destroy window
  (sdl-destroy-window *window*)
  (setf *window* (null-pointer))
  ;; Quit SDL subsystem
  (sdl-quit))


(defun main ()
  (cond ((null (init)) (format t "Failed to initialize!~%"))
        (t (cond ((null (load-media)) (format t "Failed to load media!~%"))
                 ;; ;;Apply the image
                 (t (let ((quit nil))
                      ;; While application is running
                      (while (not quit)
                        ;; Event handler
                        (with-foreign-object (e 'sdl-event)
                          ;; Handle events on queue
                          (while (not (zerop (sdl-poll-event e)))
                            ;; User requests quit
                            (when (= (sdl-event-type e) SDL-QUIT)
                              (setf quit t))))
                          ;; Apply the image
                          (sdl-blit-surface *x-out* (null-pointer) *screen-surface* (null-pointer))
                          ;; Update the surface
                          (sdl-update-window-surface *window*)))))))
  (close-all))
