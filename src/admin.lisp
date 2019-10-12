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
