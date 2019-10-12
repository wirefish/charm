(in-package :charm)

(defproto corpse (container entity)
  (brief "a corpse of ")
  (pose "lies nearby.")
  (full "The corpse is decaying rather quickly.")
  (icon 'tombstone)
  (of nil :instance))

(defun make-corpse (deceased)
  (make-instance 'corpse :of (describe-brief deceased)))

(defmethod describe-brief ((entity corpse) &rest args)
  (concatenate 'string
               (apply #'format-noun (brief entity) args)
               (of entity)))
