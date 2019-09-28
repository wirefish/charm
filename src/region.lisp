(in-package :charm)

(defstruct region
  name full climate)

(defun transform-region-slot (slot)
  (destructuring-bind (name init-form) slot
    (list (intern (symbol-name name) :keyword) init-form)))

(defparameter *current-region* nil)

(defmacro defregion (name &body slots)
  `(progn
     (defparameter ,name (make-region ,@(mapcan #'transform-region-slot slots)))
     (setf *current-region* ,name)))
