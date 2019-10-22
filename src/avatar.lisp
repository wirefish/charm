(in-package :charm)

;;; An object of class `race` defines a collection of properties inherited by an
;;; avatar that is a member of that race.

(defclass race ()
  ((key
    :initarg :key :reader key
    :documentation "Symbol whose value is the singleton race instance.")
   (brief
    :initarg :brief :reader brief
    :documentation "A noun describing a member of the race, e.g. 'an elf'.")
   (male-icon
    :initarg :male-icon :initform nil :reader male-icon
    :documentation "Default icon for male members of the race.")
   (female-icon
    :initarg :female-icon :initform nil :reader female-icon
    :documentation "Default icon for female members of the race.")
   (full
    :initarg :full :reader full
    :documentation "A prose description of a generic member of the race.")
   (base-health
    :initarg :base-health :initform 15 :reader base-health
    :documentation "The base health per level for a member of the race.")
   (base-energy
    :initarg :base-energy :initform 100 :reader base-energy
    :documentation "The base energy (not scaled by level) for a member of the
      race.")
   (base-mana
    :initarg :base-mana :initform 15 :reader base-mana
    :documentation "The base mana per level for a member of the race.")
   (modifiers
    :initarg :modifiers :initform nil :reader modifiers
    :documentation "A plist containing base modifiers for a member of the
      race.")))

(defmacro defrace (name &body slots)
  (let ((args (loop for (slot-name init-form) in slots
                    append `(,(make-keyword slot-name)
                             ,(transform-slot-init-form name slot-name init-form)))))
    `(defparameter ,name (make-instance 'race :key ',name ,@args))))

(defmethod get-modifier (modifier (entity race))
  (getf (modifiers entity) modifier 0))

(defmethod describe-brief ((subject race) &rest args)
  (apply #'format-noun (brief subject) args))

(defmethod describe-full ((subject race))
  (full subject))

;;; An avatar can equip items in a number of slots.

(defparameter *equipment-slots*
  (plist-hash-table
   '(;; weapons
     :main-hand #("in your" "main hand")
     :off-hand #("in your" "off-hand")
     ;; tools
     :tool #("as your" "tool")
     ;; armor
     :head #("on your" "head")
     :torso #("on your" "body")
     :back #("across your" "shoulders")
     :hands #("on your" "hands")
     :waist #("around your" "waist")
     :legs #("on your" "legs")
     :feet #("on your" "feet")
     ;; accessories
     :ears #("on your" "ears")
     :neck #("around your" "neck")
     :wrists #("on your" "wrists")
     :left-finger #("on your" "left finger")
     :right-finger #("on your" "right finger")
     ;; containers
     :backpack #("on your" "back")
     :in-hands #("in your" "hands"))))

(defproto avatar-hands (container entity)
  (brief "hands")
  (empty-pose "are empty.")
  (nonempty-pose "are holding ~a.")
  (capacity 8)
  (size :medium))

(defun make-initial-equipment ()
  (plist-hash-table (list :in-hands (make-instance 'avatar-hands))))

(defproto avatar (combatant creature)
  (name nil :instance)
  (icon nil :instance)
  (full nil :instance)
  (race nil :instance)
  (gender nil :instance)
  (session nil :instance)
  (equipment (make-initial-equipment) :instance)
  (skills (make-hash-table) :instance) ; skill-key -> rank
  (abilities nil :instance) ; learned directly e.g. from books

  (level 0 :instance)
  (xp 0 :instance)
  (karma 0 :instance)
  (attitude :friendly :instance)

  (pending-offer nil :instance)
  (active-quests nil :instance) ; alist (quest-key . progress)
  (finished-quests (make-hash-table) :instance) ; quest-key -> completion time
  (tutorials-on t :instance)
  (tutorials-seen (make-hash-table) :instance) ; key -> t
  (recall-location nil :instance))

;;; Control serialization of slots.

(defmethod encode-slot ((name (eql 'location)) value)
  nil)

(defmethod encode-slot ((name (eql 'race)) value)
  (key value))

(defmethod decode-slot ((name (eql 'race)) value)
  (symbol-value value))

(defmethod encode-slot ((name (eql 'pending-offer)) value)
  nil)

(defmethod encode-slot ((name (eql 'session)) value)
  nil)

;;; Define how an avatar is described.

(defmethod visible-p ((subject avatar) observer)
  ;; FIXME: need to check for any invisibility aura
  (not (deadp subject)))

(defparameter *corpse-noun* (parse-noun "the corpse of "))

(defmethod describe-brief ((subject avatar) &rest args)
  (if (deadp subject)
      (concatenate 'string
                   (apply #'format-noun *corpse-noun* args)
                   (or (name subject) (describe-brief (race subject))))
      (or (name subject) (apply #'describe-brief (race subject) args))))

(defmethod describe-icon ((subject avatar))
  (if (deadp subject)
      'tombstone
      (or (with-slots (icon gender race) subject
            (cond
              (icon icon)
              ((eq gender :female) (female-icon race))
              (t (male-icon race))))
          'invisible)))

(defmethod describe-full ((subject avatar))
  (or (full subject) (describe-full (race subject))))

;;; Define how an avatar is matched against user input.

(defmethod match-tokens (tokens (target avatar))
  (best-match (call-next-method)
              (if (name target) (match-tokens tokens (name target)))
              (match-tokens tokens (race target))))

;;; Base health, energy, and mana are derived from the avatar's race.

(defmethod base-health ((entity avatar))
  (base-health (race entity)))

(defmethod base-energy ((entity avatar))
  (base-energy (race entity)))

(defmethod base-mana ((entity avatar))
  (base-mana (race entity)))

;;;

(defmethod get-equipment-modifier (modifier avatar)
  (let ((value 0))
    (maphash-values #'(lambda (x)
                        (incf value (get-modifier modifier x)))
                    (equipment avatar))
    value))

(defmethod get-modifier (modifier (entity avatar))
  "Modifiers for an avatar combine those from race and equipment in addition to
  the normal sources for a combatant."
  (+ (get-modifier modifier (race entity))
     (get-equipment-modifier modifier entity)
     (call-next-method)))

;;;

(defun change-race (avatar race)
  (setf (race avatar) race)
  (show-notice avatar "You are now ~a!" (describe-brief race))
  (update-resources avatar)
  (update-avatar avatar
                 :race (describe-brief race :article nil)
                 :health (health avatar)
                 :max-health (max-health avatar)
                 :energy (energy avatar)
                 :max-energy (max-energy avatar)
                 :mana (mana avatar)
                 :max-mana (max-mana avatar)))

(defun change-gender (avatar gender)
  (setf (gender avatar) gender)
  (show-notice avatar "You are now ~a!" (string-downcase (symbol-name gender))))

(defun valid-name-p (name)
  ;; FIXME:
  (declare (ignore name))
  t)

(defun change-name (avatar name)
  (setf (name avatar) name)
  (show-notice avatar "Your name is now ~a!" name)
  (update-avatar avatar :name name))

(defun xp-required-for-next-level (avatar)
  (* 1000 (1+ (level avatar))))

(defun karma-gained-at-level (level)
  (cond
    ((<= level 10) 5)
    ((<= level 20) 10)
    (t 15)))

(defun gain-xp (avatar xp)
  (incf (xp avatar) xp)
  (show-notice avatar "You gain ~d experience points." xp)
  (let ((xp-required (xp-required-for-next-level avatar)))
    (if (>= (xp avatar) xp-required)
        (progn
          (incf (level avatar))
          (decf (xp avatar) xp-required)
          (let ((karma-gained (karma-gained-at-level (level avatar))))
            (incf (karma avatar) karma-gained)
            (show-notice avatar "You are now level ~d! You gain ~d karma."
                         (level avatar) karma-gained))
          (update-resources avatar)
          (update-avatar avatar :level (level avatar)
                                :xp (xp avatar)
                                :xp-required (xp-required-for-next-level avatar)
                                :max-health (max-health avatar)
                                :max-energy (max-energy avatar)
                                :max-mana (max-mana avatar)))
        (update-avatar avatar :xp (xp avatar)))))

(defbehavior regenerate (avatar)
    ()
  (:regenerate
   (with-slots (race health max-health energy max-energy mana max-mana) avatar
     (when (or (< health max-health)
               (< energy max-energy)
               (< mana max-mana))
       (let ((bonus (floor (/ (get-modifier :resilience avatar) 20)))
             (scale (if (opponents avatar) 1/4 1)))
         (setf health (min max-health (+ health (floor (* scale (+ (base-health race) bonus)))))
               energy (min max-energy (+ energy 10 bonus))
               mana (min max-mana (+ mana (floor (* scale (+ (base-mana race) bonus))))))
         (update-avatar avatar
                        :health health
                        :energy energy
                        :mana mana))))
   (change-state :regenerate 3)))

(defmethod do-enter-world :after ((avatar avatar) location entry)
  (start-behavior avatar :regenerate #'regenerate))

;;; FIXME: move these?

(defun update-equipment (actor &optional slots)
  "Sends a structured update to the client describing the items equipped by
  `actor`. If `slots` is provided, the update describes only those slots."
  (with-slots (equipment) actor
    (let ((update (alist-hash-table
                   (mapcar #'(lambda (slot)
                               (let ((item (gethash slot equipment)))
                                 (cons slot (and item
                                                 (list (describe-icon item)
                                                       (describe-brief item :article nil))))))
                           (or slots
                               (remove :in-hands (hash-table-keys *equipment-slots*)))))))
      (send-client-command (session actor) "updateEquipment" update))))

(defun update-inventory (actor &key changed removed)
  (let ((update (alist-hash-table
                 (mapcar #'(lambda (slot) (cons slot (make-hash-table)))
                         '(:backpack :in-hands)))))
    ;; If items were removed, map from their id to nil in the appropriate slot.
    (loop for (slot . item) in removed do
         (setf (gethash (write-to-string (id item)) (gethash slot update)) nil))
    (if changed
        ;; Only send updates for changed items.
        (loop for (slot . item) in changed do
             (setf (gethash (write-to-string (id item)) (gethash slot update))
                   (list (describe-icon item)
                         (describe-brief item :article nil))))
        ;; Include all inventory items in the update.
        (dolist (slot '(:backpack :in-hands))
          (when-let ((container (gethash slot (equipment actor))))
            (dolist (item (contents container))
              (setf (gethash (write-to-string (id item)) (gethash slot update))
                    (list (describe-icon item)
                          (describe-brief item :article nil)))))))
    (send-client-command (session actor) "updateInventory" update)))

(defparameter *all-combat-keys*
  (plist-hash-table
   (list* :max-health #'health
          :max-energy #'max-energy
          :max-mana #'max-mana
          (loop for modifier in (append *primary-attributes*
                                        *secondary-attributes*
                                        (hash-table-keys *damage-types*))
             append (list modifier (curry #'get-modifier modifier))))))

(defun update-combat (actor &optional keys)
  (send-client-command
   (session actor) "updateCombat"
   (alist-hash-table
    (mapcar #'(lambda (key)
                (cons key (funcall (gethash key *all-combat-keys*) actor)))
            (or keys (hash-table-keys *all-combat-keys*))))))

;;;

(defun skill-rank (avatar skill-key)
  "Returns the rank of `avatar` in the skill with the given key, or nil if the
  skill has not been learned."
  (gethash skill-key (skills avatar)))

(defun match-skills (tokens avatar)
  (keep-if #'(lambda (key)
               (let ((skill (find-skill key)))
                 (when (match-tokens tokens skill)
                   skill)))
           (hash-table-keys (skills avatar))))

(defun increase-skill-rank (avatar skill-key amount)
  (let* ((current-rank (skill-rank avatar skill-key))
         (skill (find-skill skill-key))
         (new-rank (min (max-rank skill)
                        (+ current-rank amount))))
    (setf (gethash skill-key (skills avatar)) new-rank)
    (when (> (floor new-rank)
             (floor current-rank))
      (show-notice avatar "Your rank in ~a has increased to ~d!"
                   (name skill)
                   (floor new-rank))
      (update-skills avatar (list skill-key)))))

(defun update-skills (avatar &optional keys)
  (send-client-command
   (session avatar) "updateSkills"
   (karma avatar)
   (with-slots (skills) avatar
     (mapcar #'(lambda (key)
                 (let ((rank (skill-rank avatar key))
                       (skill (find-skill key)))
                   (list key (name skill) (floor rank) (max-rank skill))))
             (or keys (hash-table-keys skills))))))

;;; Select an avatar's next (auto) attack.

(defproto fist (natural-weapon)
  (brief "a fist")
  (damage-type :crushing)
  (attack-verb "bashes")
  (damage-scale 0.5))

(defparameter *default-attack* (make-instance 'fist))

(defmethod select-attack ((attacker avatar) target)
  (or (gethash :main-hand (equipment attacker))
      *default-attack*))
