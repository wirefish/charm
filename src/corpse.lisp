(in-package :charm)

(defproto corpse (container entity)
  (brief "a corpse of ")
  (pose "lies nearby.")
  (full "The corpse is decaying rather quickly.")
  (icon 'tombstone)
  (of nil :instance))

;; TODO: remember who can loot the corpse.

;; TODO: avatars should leave a corpse with a few random equipped items on it,
;; only lootable by the player, that doesn't decay.

(defun make-corpse (deceased)
  (when-let ((loot (create-loot (loot deceased))))
    (make-instance 'corpse :of (describe-brief deceased) :contents loot)))

(defmethod describe-brief ((entity corpse) &rest args)
  (concatenate 'string
               (apply #'format-noun (brief entity) args)
               (of entity)))
