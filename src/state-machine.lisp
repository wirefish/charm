(in-package :charm)

(defproto state-machine ()
  (initial-state :start)
  (state nil :instance)
  (next-event nil :instance))

(defevent change-state (actor state))

(defmethod do-change-state :around ((actor state-machine) state)
  (setf (state actor) state)
  (format-log :debug "~a changing state to ~a" actor state)
  (setf (next-event actor) (call-next-method)))

;;;

(defgeneric start-state-machine (actor))

(defmethod start-state-machine (actor))

(defmethod start-state-machine ((actor state-machine))
  (format-log :debug "starting state machine for ~a" actor)
  (change-state actor (initial-state actor)))

;;;

(defgeneric reset-state-machine (actor))

(defmethod reset-state-machine (actor))

(defmethod reset-state-machine :around ((actor state-machine))
  (call-next-method)
  (setf (next-event actor) nil)
  (setf (state actor) nil))
