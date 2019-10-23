(in-package :charm)

;;; Definitions of all damage types.

(defparameter *damage-types*
  (plist-hash-table
   '(:crushing :physical
     :piercing :physical
     :slashing :physical
     ;;
     :fire :elemental
     :cold :elemental
     :acid :elemental
     :electricity :elemental
     ;;
     :arcane :magical
     :shadow :magical)))

(defun damage-group (damage-type)
  (gethash damage-type *damage-types*))

;;; A prototype for an entity that can participate in combat.

(defproto combatant ()
  (base-health 10) ; per level
  (base-energy 100)
  (base-mana 0) ; per level
  (level 0)
  (modifiers (make-hash-table))
  (attitude :neutral)
  ;;
  (max-health nil :instance)
  (health nil :instance)
  (max-energy nil :instance)
  (energy nil :instance)
  (max-mana nil :instance)
  (mana nil :instance)
  (auras nil :instance)
  (attack-target nil :instance)
  (assist-target nil :instance)
  (opponents nil :instance))

;;; Disable encoding of slots non-persistent slots.

(defmethod encode-slot ((name (eql 'attack-target)) value)
  nil)

(defmethod encode-slot ((name (eql 'assist-target)) value)
  nil)

(defmethod encode-slot ((name (eql 'opponents)) value)
  nil)

;;; Primary attributes have a base value that increases automatically with
;;; level. They can be modified by race, equipment, traits, and auras.

(defparameter *primary-attributes*
  '(:strength ; increases outgoing physical damage
    :intellect ; increases outgoing magical and elemental damage
    :vitality ; increases health
    :toughness)) ; increases armor which decreases incoming physical and elemental damage

;;; Secondary attributes have a base value of zero and do not increase with
;;; level. Like primary attributes they can be modified by race, equipment,
;;; traits, and auras.

(defparameter *secondary-attributes*
  '(:willpower ; increases magic resistance which decreases incoming magical damage
    :compassion ; increases healing output
    :precision ; increases chance of scoring a critical hit
    :ferocity ; increases magnitude of critical hits
    :evasion ; increases chance of dodging an atttack
    :accuracy ; reduces the chance that attacks miss
    :spirit ; increases energy
    :insight ; increases mana
    :resilience)) ; increases regeneration rate

(defun base-attribute-value (attribute level)
  (if (find attribute *primary-attributes*)
      (+ 10 (* 5 level))
      0))

;;;

(defmethod get-modifier (modifier (entity combatant))
  "Sums contributions from inherent modifiers and those conferred by auras."
  (apply #'+
         (gethash modifier (modifiers entity) 0)
         (base-attribute-value modifier (level entity))
         (mapcar (curry #'get-modifier modifier) (auras entity))))

;;; Health, energy, and mana are resources used during combat.

(defun compute-max-health (entity)
  (+ (* (1+ (level entity)) (base-health entity))
     (get-modifier :vitality entity)))

(defun compute-max-energy (entity)
  (+ (base-energy entity)
     (get-modifier :spirit entity)))

(defun compute-base-mana (entity)
  "Base mana does not incorporate insight modifiers. It is used as the basis for
  the cost of spells cast by `entity`."
  (* (1+ (level entity)) (base-mana entity)))

(defun compute-max-mana (entity)
  (+ (compute-base-mana entity) (get-modifier :insight entity)))

(defun update-resources (entity)
  (setf (max-health entity) (compute-max-health entity)
        (max-energy entity) (compute-max-energy entity)
        (max-mana entity) (compute-max-mana entity))
  (minf (health entity) (max-health entity))
  (minf (energy entity) (max-energy entity))
  (minf (mana entity) (max-mana entity)))

(defmethod initialize-instance :after ((entity combatant) &key)
  (setf (max-health entity) (compute-max-health entity)
        (health entity) (max-health entity)
        (max-energy entity) (compute-max-energy entity)
        (energy entity) (max-energy entity)
        (max-mana entity) (compute-max-mana entity)
        (mana entity) (max-mana entity)))

;;; Combatants execute attacks in order to damage other combatants. An object
;;; that represents an attack must have corresponding methods for the following
;;; functions:

(defgeneric attack-damage (attack attacker)
  (:documentation "Returns a cons (min . max) representing the inherent damage
    of `attack` when executed by `attacker`."))

(defgeneric attack-delay (attack)
  (:documentation "Returns the number of seconds to delay before executing
    `attack`."))

(defgeneric damage-type (attack)
  (:documentation "Returns the type of damage caused by `attack`."))

;;;

(defgeneric select-attack (attacker target)
  (:documentation "Called to select the next attack that `attacker` will execute
    against `target`.")
  (:method (attacker target)
    nil))

(defgeneric select-target (attacker)
  (:documentation "Called to select the next target for attacks from
    `attacker`.")
  (:method (attacker)
    nil))

;;;

(defgeneric add-opponent (actor opponent)
  (:documentation "Called when `actor` enters combat with `opponent`."))

(defmethod add-opponent ((actor combatant) opponent)
  (setf (opponents actor) (adjoin opponent (opponents actor))))

(defgeneric remove-opponent (actor opponent)
  (:documentation "Called when `actor` is no longer in combat with `opponent`."))

(defmethod remove-opponent (actor opponent)
  (deletef (opponents actor) opponent))

(defun in-combat-p (actor)
  (opponents actor))

;;;

(defun deadp (actor)
  (= (health actor) 0))

(defun xp-value (combatant)
  (* 20 (1+ (level combatant))))

(defmethod neighbor-properties ((entity combatant))
  (let ((properties (call-next-method)))
    (setf (gethash :health properties) (health entity)
          (gethash :max-health properties) (max-health entity))
    properties))
