(in-package :charm)

(defproto corpse (entity container)
  (brief "a corpse of ")
  (pose "lies quietly nearby.")
  (full "The corpse is decaying rather quickly.")
  (icon :tombstone)
  (of nil :instance))

(defun make-corpse (deceased)
  (make-instance 'corpse :id (id deceased) :of deceased))

(defmethod describe-brief ((entity corpse) &rest args)
  (concatenate 'string
               (apply #'format-noun (brief entity) args)
               (describe-brief (of entity))))
