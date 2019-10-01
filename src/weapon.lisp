(in-package :charm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod transform-slot-init-form (def-name (slot-name (eql 'attack-verb)) init-form)
    `(parse-verb ,init-form)))

(defproto weapon (equipment)
  (weapon-type "weapon")
  (damage-type :crushing)
  (attack-verb "hits")
  (attack-delay 3)
  (offhand-penalty 0))

(defgeneric weapon-damage (item)
  (:documentation "Returns the inherent damage range for a weapon, based on its
    level and quality."))

;;; Light weapons are quick, do relatively little damage per hit, and incur a
;;; relatively small penalty when used in the off-hand.

(defproto light-weapon (weapon)
  (slot :main-hand)
  (attack-delay 2)
  (offhand-penalty 2/3))

(defmethod weapon-damage ((item light-weapon))
  (let ((level (+ (level item) (quality item))))
    (cons (+ 2 level)
          (+ 4 (* 2 level)))))

;;; One-handed weapons are moderate in delay, damage per hit, and off-hand
;;; penalty.

(defproto one-handed-weapon (weapon)
  (slot :main-hand)
  (attack-delay 2.5)
  (offhand-penalty 1/2))

(defmethod weapon-damage ((item one-handed-weapon))
  (let ((level (+ (level item) (quality item))))
    (cons (+ 4 (* 2 level))
          (+ 6 (* 3 level)))))

;;; Two-handed weapons are slow, do high damage per hit, and cannot be used in
;;; the off-hand since they require both hands.

(defproto two-handed-weapon (weapon)
  (slot :both-hands)
  (attack-delay 3))

(defmethod weapon-damage ((item two-handed-weapon))
  (let ((level (+ (level item) (quality item))))
    (cons (+ 6 (* 3 level))
          (+ 10 (* 5 level)))))

;;;

(defmethod describe-full ((subject weapon))
  (destructuring-bind (min . max) (weapon-damage subject)
    (format nil
            "~a (~a; damage ~d-~d ~(~a~); DPS ~,1f~{; ~(~a~) ~d~})"
            (call-next-method)
            (weapon-type subject)
            min max
            (damage-type subject)
            (/ (+ min max)
               (attack-delay subject)
               2.0)
            (modifiers subject))))
