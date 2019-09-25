(in-package :charm)

(defgeneric accept-offer (actor offer source))

(defmethod accept-offer (actor (offer null) source)
  (show-text actor "You have not been offered anything."))

(defun make-offer (actor offer source)
  (setf (pending-offer actor) (list offer source)))

(defcommand (actor "accept")
  "The accept command allows you to accept whatever was most recently offered to
  you. The offer could be for any number of things, such as a quest, a trade, or
  a party invitation."
  (destructuring-bind (&optional offer source) (pending-offer actor)
    (setf (pending-offer actor) nil)
    (accept-offer actor offer source)))
