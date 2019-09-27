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
  (allow-nonproficient-use nil)
  (mastery nil)
  (modifiers nil)
  (quality nil :instance)
  (affixes nil :instance)
  (inscription nil :instance))

(defproto weapon (equipment)
  (damage-type :crushing)
  (base-damage (1 4))
  (attack-verb "hits")
  (attack-delay 3))

(defmethod get-modifier (modifier (entity equipment))
  (getf (modifiers entity) modifier 0))
