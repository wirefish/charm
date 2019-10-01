(in-package :charm)

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
