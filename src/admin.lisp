(in-package :charm)

(defcommand (actor "dump" subject "slot" slot)
  (let ((matches (if subject
                     (match-objects subject (contents (location actor)))
                     (list actor))))
    (dolist (entity matches)
      (show-raw actor
                (encode (if slot
                            (slot-value entity (find-symbol (string-upcase (first slot))))
                            entity))))))

(defcommand (actor "save")
  (save-avatar (session-account-id (session actor)) actor)
  (show-text actor "Your avatar has been saved."))

(defcommand (actor "update")
  (update-avatar actor
                 :name (name actor)
                 :icon (describe-icon actor)
                 :level (level actor)
                 :race (describe-brief (race actor) :article nil)
                 :health (health actor)
                 :max-health (max-health actor)
                 :energy (energy actor)
                 :max-energy (max-energy actor)
                 :mana (mana actor)
                 :max-mana (max-mana actor)
                 :xp (xp actor)
                 :xp-required (xp-required-for-next-level actor))
  (update-equipment actor)
  (update-inventory actor)
  (update-skills actor)
  (update-combat actor))

(defcommand (actor "loc")
  (show-raw actor (encode (type-of (location actor)))))
