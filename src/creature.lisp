(in-package :charm)

;;; FIXME: reorganize this

(defproto creature (entity)
  (attitude :neutral)
  (begins-quests nil)
  (ends-quests nil))

;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod transform-slot-init-form (def-name (slot-name (eql 'attacks)) init-form)
    `(list ,@(mapcar #'(lambda (class) `(make-instance ',class)) init-form))))

(defproto monster (combatant creature)
  (attacks nil))

(defmethod select-attack ((attacker monster) target)
  ;; FIXME: select an attack based on probabilities and cooldowns
  (first (attacks attacker)))

;;;

(defproto npc (creature)
  (attitude :friendly))

(defproto vendor (npc)
  (sells nil))

(defproto trainer (npc)
  (teaches nil))
