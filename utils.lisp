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
      (ensure-list name-and-options)
    (let ((%name (symbol-combine '% name))
          (conc-name (or (getf options :conc-name)
                         (symbol-combine name '-))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defcstruct (,%name ,@options) ,@fields)
         (defctype ,name (:struct ,%name))
         ,@(generate-struct-accessors %name conc-name
                                      (mapcar #'car fields))
         ',name))))         


(defun generate-struct-accessors (name conc-name slot-names)
  (loop with pointer-arg = (symbol-combine '#:pointer-to- name)
        for slot in slot-names
        for accessor = (symbol-combine conc-name slot)
        collect `(defun ,accessor (,pointer-arg)
                   (foreign-slot-value ,pointer-arg '(:struct ,name) ',slot))
        collect `(defun (setf ,accessor) (value ,pointer-arg)
                   (setf (foreign-slot-value ,pointer-arg '(:struct ,name) ',slot) value))))



;; Combine the `defcunion' and `defctype'
;; Bind ffi of C program definitions like below
;; typedef union union-name
;; {
;;     type slot-name;
;; } union-name;
(defmacro defcuniontype (name-and-options &body fields)
  (destructuring-bind (name &key size)
      (ensure-list name-and-options)
    (let ((%name (symbol-combine '% name))
          (options (if (null size)
                       nil
                       `(:size ,size))))
      `(progn
         (defcunion ,%name ,@options ,@fields)
         (defctype ,name (:union ,%name))))))


;; Get a symbol whose name is the concatenate of each symbol's name in `symbols'.
(defun symbol-combine (&rest symbols)
  "Get a symbol whose name is the concatenate of each given (as arguments) symbol's name."
  (let* ((name (apply #'concatenate
                     'string
                     (mapcar #'symbol-name symbols)))
         (sym (find-symbol name)))
    (if sym
        (values sym :internal)
        (intern name))))


(defun ensure-list (list)
  "If LIST is a list, it is returned. Otherwise returns the list designated by LIST."
  (if (listp list)
      list
      (list list)))
