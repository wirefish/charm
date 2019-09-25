(in-package :charm)

(defcommand (actor "dump" :word slot)
  (show-raw actor
            (encode (if slot
                        (slot-value actor (find-symbol (string-upcase slot)))
                        actor))))

(defcommand (actor "save")
  (save-avatar (session-username (session actor)) actor)
  (show-text actor "Your avatar has been saved."))
