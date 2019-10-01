(in-package :charm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod transform-slot-init-form (def-name (slot-name (eql 'attack-verb)) init-form)
    `(parse-verb ,init-form))

  (defmethod transform-slot-init-form (def-name (slot-name (eql 'base-damage)) init-form)
    `(list ,@init-form)))

;;;

(defproto equipment (item)
  (level 0)
  (slot nil)
  (proficiency nil)
  (mastery nil)
  (modifiers nil)
  (quality 0 :instance)
  (affixes nil :instance)
  (inscription nil :instance))

(defmethod get-modifier (modifier (entity equipment))
  (getf (modifiers entity) modifier 0))
