(in-package :charm)

(defproto tool (equipment)
  (slot :tool)
  (required-skill nil)
  (required-rank 0))

(defmethod do-equip-item :around (actor (item tool) inventory-slot equipment-slot)
  (let ((rank (skill-rank actor (required-skill item))))
    (cond
      ((null rank)
       (show-text actor "You must learn the ~a skill before equipping that item."
                  (name (find-skill (required-skill item)))))
      ((< rank (required-rank item))
       (show-text actor "You must achieve rank ~d in the ~a skill before equipping that item."
                  (required-rank item)
                  (name (find-skill (required-skill item)))))
      (t
       (call-next-method)))))
