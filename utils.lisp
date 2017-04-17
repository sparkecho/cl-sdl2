;;;; utils.lisp
;;;; Utilities for cl-sdl2

(in-package #:cl-sdl2)

;; Combine the `defcstruct' and `defctype'
;; Bind ffi of C program definitions like below
;; typedef struct struct-name
;; {
;;     type slot-name;
;; } struct-name;
(defmacro defcstructype (name-and-options &body fields)
  (destructuring-bind (name . options)
      (cffi::ensure-list name-and-options)
    (let ((%name (intern (concatenate 'string "%" (symbol-name name)))))
      `(progn
         (defcstruct ,%name ,@options ,@fields)
         (defctype ,name (:struct ,%name))))))
