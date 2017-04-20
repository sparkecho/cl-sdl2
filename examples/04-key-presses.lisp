;;;; 03_event_driven_programming.lisp

(in-package :cl-sdl2)


(defconstant +screen-width+ 640)
(defconstant +screen-height+ 480)

(defcenum key-press-surfaces
  (key-press-surface-default)
  (key-press-surface-up)
  (key-press-surface-down)
  (key-press-surface-left)
  (key-press-surface-right)
  (key-press-surface-total))

(defparameter *window* (null-pointer))
(defparameter *screen-surface* (null-pointer))
(defparameter *key-press-surfaces*
  (make-array key-press-surface-total :initial-element (null-pointer)))
(defparameter *current-surface* (null-pointer))


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


(defun load-surface (path)
  (let ((loaded-surface (sdl-load-bmp path)))
    (when (null-pointer-p loaded-surface)
      (format t "Unable to load image ~S! SDL Error: ~S~%" path (sdl-get-error)))
    loaded-surface))


(defun cats (&rest strings)
  (apply #'concatenate 'string strings))


(defun load-media ()
  (let ((success t)                     ;Loading success flag
        (full-path (namestring (merge-pathnames "examples/" (asdf:system-source-directory :cl-sdl2)))))
    ;; Load default surface
    (setf (aref *key-press-surfaces* key-press-surface-default)
          (load-surface (cats full-path "press.bmp")))
    (when (null-pointer-p (aref *key-press-surfaces* key-press-surface-default))
      (format t "Failed to load default image!~%")
      (setf success nil))

    ;; Load up surface
    (setf (aref *key-press-surfaces* key-press-surface-up)
          (load-surface (cats full-path "up.bmp")))
    (when (null-pointer-p (aref *key-press-surfaces* key-press-surface-up))
      (format t "Failed to load up image!~%")
      (setf success nil))

    ;; Load down surface
    (setf (aref *key-press-surfaces* key-press-surface-down)
          (load-surface (cats full-path "down.bmp")))
    (when (null-pointer-p (aref *key-press-surfaces* key-press-surface-down))
      (format t "Failed to load down image!~%")
      (setf success nil))

    ;; Load left surface
    (setf (aref *key-press-surfaces* key-press-surface-left)
          (load-surface (cats full-path "left.bmp")))
    (when (null-pointer-p (aref *key-press-surfaces* key-press-surface-left))
      (format t "Failed to load left image!~%")
      (setf success nil))

    ;; Load right surface
    (setf (aref *key-press-surfaces* key-press-surface-right)
          (load-surface (cats full-path "right.bmp")))
    (when (null-pointer-p (aref *key-press-surfaces* key-press-surface-right))
      (format t "Failed to load right image!~%")
      (setf success nil))

    success))


;; Name `close' conflicts with lisp built-in.
(defun close-all ()
  ;; Deallocate surfaces
  (loop for i from 0 below key-press-surface-total
     do (progn (sdl-free-surface (aref *key-press-surfaces* i))
               (setf (aref *key-press-surfaces* i) (null-pointer))))
  ;; Destroy window
  (sdl-destroy-window *window*)
  (setf *window* (null-pointer))
  ;; Quit SDL subsystem
  (sdl-quit))


(defun main ()
  (cond ((null (init)) (format t "Failed to initialize!~%"))
        (t (cond ((null (load-media)) (format t "Failed to load media!~%"))
                 ;; ;;Apply the image
                 ;; (t (let ((quit nil)                      ;Main loop flag
                 ;;          (e (foreign-alloc 'sdl-event))) ;Event handler
                 (t (let ((quit nil))                      ;Main loop flag
                      ;; Set default current surface
                      (setf *current-surface* (aref *key-press-surfaces* key-press-surface-default))
                      ;; While application is running
                      (while (not quit)
                        (with-foreign-object (e 'sdl-event)
                          ;; Handle events on queue
                          (while (not (zerop (sdl-poll-event e)))
                            ;; User requests quit
                            (cond ((= (sdl-event-type e) SDL-QUIT) (setf quit t))
                                  ((= (sdl-event-type e) SDL-KEY-DOWN)
                                   (let ((pressed-key (sdl-keysym-sym (sdl-keyboard-event-keysym (sdl-event-key e)))))
                                     (case pressed-key
                                       (#.SDLK-UP   (setf *current-surface* (aref *key-press-surfaces* key-press-surface-up)))
                                       (#.SDLK-DOWN (setf *current-surface* (aref *key-press-surfaces* key-press-surface-down)))
                                       (#.SDLK-LEFT (setf *current-surface* (aref *key-press-surfaces* key-press-surface-left)))
                                       (#.SDLK-RIGHT (setf *current-surface* (aref *key-press-surfaces* key-press-surface-right)))
                                       (otherwise   (setf *current-surface* (aref *key-press-surfaces* key-press-surface-default)))))))))
                        ;; Apply the current image
                        (sdl-blit-surface *current-surface* (null-pointer) *screen-surface* (null-pointer))
                        ;; Update the surface
                        (sdl-update-window-surface *window*)))))))
  (close-all))
