(in-package :charm)

(defproto weapon (equipment)
  (damage-type :crushing)
  (attack-verb "hits"))

(defgeneric weapon-delay (item)
  (:documentation "Returns the delay (in seconds) between successive autoattacks
    with a weapon."))

(defgeneric weapon-damage (item)
  (:documentation "Returns the inherent damage range for a weapon, based on its
    level and quality."))

(defgeneric weapon-offhand-penalty (item)
  (:documentation "Returns a factor that scales damage when a weapon is used in
    the :off-hand equipment slot."))

;;; Light weapons are quick, do relatively little damage per hit, and incur a
;;; relatively small penalty when used in the off-hand.

(defproto light-weapon (weapon)
  (slot :main-hand))

(defmethod weapon-delay ((item light-weapon))
  2)

(defmethod weapon-damage ((item light-weapon))
  (let ((level (+ (level item) (quality item))))
    (cons (+ 2 level)
          (+ 4 (* 2 level)))))

(defmethod weapon-offhand-penalty ((item light-weapon))
  2/3)

;;; One-handed weapons are moderate in delay, damage per hit, and off-hand
;;; penalty.

(defproto one-handed-weapon (weapon)
  (slot :main-hand))

(defmethod weapon-delay ((item one-handed-weapon))
  2.5)

(defmethod weapon-damage ((item one-handed-weapon))
  (let ((level (+ (level item) (quality item))))
    (cons (+ 4 (* 2 level))
          (+ 6 (* 3 level)))))

(defmethod weapon-offhand-penalty ((item one-handed-weapon))
  0.5)

;;; Two-handed weapons are slow, do high damage per hit, and cannot be used in
;;; the off-hand since they require both hands.

(defproto two-handed-weapon (weapon)
  (slot :both-hands))

(defmethod weapon-delay ((item two-handed-weapon))
  3)

(defmethod weapon-damage ((item two-handed-weapon))
  (let ((level (+ (level item) (quality item))))
    (cons (+ 6 (* 3 level))
          (+ 10 (* 5 level)))))

(defmethod weapon-offhand-penalty ((item two-handed-weapon))
  0)
