(in-package :charm)

(defcommand (actor ("stat" "st"))
  "View basic information about your avatar."
  (with-slots (id name level race gender) actor
    (show-text actor
               "You are ~a level ~d ~a~a. (#~d)"
               (if name (format nil "~a, a" name) "a")
               level
               (describe-brief race :article nil)
               (if gender (concatenate 'string " " (string-downcase (symbol-name gender))) "")
               id))
  (show-text actor "You have ~:d of ~:d experience points and ~d unspent karma."
             (xp actor)
             (xp-required-for-next-level actor)
             (karma actor))
  (show-text actor "You have ~d of ~d health, ~d of ~d energy, and ~d of ~d mana."
             (health actor) (max-health actor)
             (energy actor) (max-energy actor)
             (mana actor) (max-mana actor))
  (show-text actor "Your attributes are: ~a."
             (format-list
              (mapcar #'(lambda (attr)
                          (let ((value (get-modifier attr actor)))
                            (format nil "~(~a~) ~d" attr value)))
                      (append *primary-attributes*
                              *secondary-attributes*))))
  (show-text actor "Your immunities are: ~a."
             (let ((immunities))
               (maphash-keys #'(lambda (damage-type)
                                 (let ((value (get-modifier damage-type actor)))
                                   (when (> value 0)
                                     (push (format nil "~(~a~) ~d" damage-type value)
                                           immunities))))
                             *damage-types*)
               (if immunities (format-list immunities) "none"))))
