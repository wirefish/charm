(in-package :charm)

(defproto race (entity)
  (base-health 15)
  (base-energy 100)
  (base-mana 15)
  (modifiers nil))

(defmethod get-modifier (modifier (entity race))
  (getf (modifiers entity) modifier 0))

(defparameter *equipment-slots*
  (plist-hash-table
   '(;; weapons
     :main-hand #("in your" "main hand")
     :off-hand #("in your" "off-hand")
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

(defproto avatar (combatant creature state-machine)
  (name nil :instance)
  (full nil :instance)
  (race nil :instance)
  (gender nil :instance)
  (session nil :instance)
  (equipment (make-initial-equipment) :instance)
  (skills (make-hash-table) :instance) ; skill-key -> rank

  (level 0 :instance)
  (xp 0 :instance)
  (karma 0 :instance)
  (attitude :friendly :instance)

  (pending-offer nil :instance)
  (active-quests nil :instance) ; alist (quest-key . progress)
  (finished-quests (make-hash-table) :instance) ; quest-key -> completion time
  (tutorials-on t :instance)
  (tutorials-seen (make-hash-table) :instance)) ; key -> t

;;; Control serialization of slots.

(defmethod encode-slot ((name (eql 'location)) value)
  (and value (type-of value)))

(defmethod decode-slot ((name (eql 'location)) value)
  (and value (symbol-value value)))

(defmethod encode-slot ((name (eql 'race)) value)
  (type-of value))

(defmethod decode-slot ((name (eql 'race)) value)
  (symbol-value value))

(defmethod encode-slot ((name (eql 'pending-offer)) value)
  nil)

(defmethod encode-slot ((name (eql 'session)) value)
  nil)

(defmethod encode-slot ((name (eql 'attack-timer)) value)
  nil)

;;; Define how an avatar is described.

(defmethod describe-brief ((subject avatar) &rest args)
  (or (name subject) (apply #'describe-brief (race subject) args)))

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

(defun regenerate (avatar)
  (with-slots (race health max-health energy max-energy mana max-mana) avatar
    (when (or (< health max-health)
              (< energy max-energy)
              (< mana max-mana))
      (let ((regen (floor (/ (get-modifier :resilience avatar) 20))))
        (setf health (min (+ health (base-health race) regen) max-health)
              energy (min (+ energy 10 regen) max-energy)
              mana (min (+ mana (base-mana race) regen) max-mana))
        (update-avatar avatar
                       :health health
                       :energy energy
                       :mana mana)))))

(defmethod do-change-state ((avatar avatar) (state (eql :start)))
  (with-delay (3)
    (regenerate avatar)
    (change-state avatar :start)))
