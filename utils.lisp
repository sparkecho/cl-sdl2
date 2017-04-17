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


;; Combine the `defcunion' and `defctype'
;; Bind ffi of C program definitions like below
;; typedef union union-name
;; {
;;     type slot-name;
;; } union-name;
(defmacro defcuniontype (name-and-options &body fields)
  (destructuring-bind (name &key size)
      (cffi::ensure-list name-and-options)
    (let ((%name (intern (concatenate 'string "%" (symbol-name name))))
          (options (if (null size)
                       nil
                       `(:size ,size))))
      `(progn
         (defcunion ,%name ,@options ,@fields)
         (defctype ,name (:union ,%name))))))
