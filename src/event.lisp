(in-package :charm)

(defmacro defevent (name (&rest args) &body body)
  (let ((can-phase (intern (format nil "CAN-~S" name)))
        (will-phase (intern (format nil "WILL-~S" name)))
        (did-phase (intern (format nil "DID-~S" name)))
        (event-function (intern (format nil "DO-~S" name)))
        (event-args (cons 'observer args)))
    `(progn
       (defgeneric ,can-phase ,event-args)
       (defmethod ,can-phase ,event-args t)
       (defgeneric ,will-phase ,event-args)
       (defmethod ,will-phase ,event-args nil)
       (defgeneric ,did-phase ,event-args)
       (defmethod ,did-phase ,event-args nil)
       (defgeneric ,event-function ,args)
       (defun ,name ,args
         ,@(or body `((,event-function ,@args))))
       (export '(,name ,can-phase ,will-phase ,did-phase ,event-function)))))

(defun notify-observers (place fn actor &rest args)
  "Implements the standard notification scheme for the will- and did- phases of
  an event, by calling the event handler for each relevant observer: `place` and
  each entity contained within `place`. Note that `actor` is passed as the
  second argument to `fn`, after the observer."
  (apply fn place actor args)
  (dolist (obs (contents place))
    (apply fn obs actor args)))

(defun query-observers (place fn actor &rest args)
  "Similar to `notify-observers`, but implements the can- phase of an event.
  Returns t iff all observers' implementations of the event handler return t."
  (and (apply fn place actor args)
       (every #'(lambda (obs) (apply fn obs actor args)) (contents place))))
