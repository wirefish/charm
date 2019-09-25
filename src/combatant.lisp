(in-package :charm)

(defstruct aura
  name description icon modifier amount)

(defproto combatant ()
  (base-health 10) ; per level
  (base-energy 100)
  (base-mana 0) ; per level
  (level 0)
  (attacks nil)
  (modifiers (make-hash-table))
  (attitude :neutral)
  ;;
  (max-health nil :instance)
  (health nil :instance)
  (max-energy nil :instance)
  (energy nil :instance)
  (max-mana nil :instance)
  (mana nil :instance)
  (auras nil :instance) ; list of (aura . expiration-time)
  (attack-target nil :instance)
  (assist-target nil :instance)
  (opponents nil :instance)
  (looters nil :instance)
  (attack-timer nil :instance))

;;; Primary attributes have a base value that increases automatically with
;;; level. They can be modified by race, equipment, traits, and auras.

(defparameter *primary-attributes*
  '(:strength ; increases outgoing physical damage
    :intellect ; increases outgoing magical damage
    :vitality ; increases health
    :toughness)) ; increases armor

(defun base-primary-attribute-value (level)
  ;; Scales from 10 at level 0 to 200 at level 50.
  (+ 9 (* 2 level) (round (expt 1.0945 level))))

;;; Secondary attributes have a base value of zero and do not increase with
;;; level. Like primary attributes they can be modified by race, equipment,
;;; traits, and auras.

(defparameter *secondary-attributes*
  '(:willpower ; increases magic resistance
    :compassion ; increases healing output
    :precision ; increases chance of scoring a critical hit
    :ferocity ; increases magnitude of critical hits
    :evasion ; increases chance of dodging an atttack
    :accuracy ; reduces the chance that attacks miss
    :spirit ; increases energy
    :insight ; increases mana
    :resilience)) ; increases regeneration rate

;;;

(defmethod get-modifier (modifier (entity combatant))
  "Sums contributions from inherent modifiers and those conferred by auras."
  (apply #'+
         (gethash modifier (modifiers entity) 0)
         (if (find modifier *primary-attributes*)
             (base-primary-attribute-value (level entity))
             0)
         (keep-if #'(lambda (x)
                      (destructuring-bind (aura . expiry) x
                        (declare (ignore expiry))
                        (when (eq (aura-modifier aura) modifier)
                          (aura-amount aura))))
                  (auras entity))))

;;; Health, energy, and mana are resources used during combat.

(defun compute-max-health (entity)
  (+ (* (1+ (level entity)) (base-health entity))
     (get-modifier :vitality entity)))

(defun compute-max-energy (entity)
  (+ (base-energy entity)
     (get-modifier :spirit entity)))

(defun compute-max-mana (entity)
  (+ (* (1+ (level entity)) (base-mana entity))
     (get-modifier :insight entity)))

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
