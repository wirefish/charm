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

(defmethod attack-message (observer attacker target (attack weapon))
  (format nil "~a ~a ~a with ~a"
          (if (eq observer attacker) "You" (describe-brief attacker :capitalize t))
          (if (eq observer attacker)
              (verb-plural (attack-verb attack))
              (verb-singular (attack-verb attack)))
          (if (eq observer target) "you" (describe-brief target))
          (describe-brief attack)))

;;; Light weapons are quick, do relatively little damage per hit, and incur a
;;; relatively small penalty when used in the off-hand.

(defproto light-weapon (weapon)
  (slot :main-hand)
  (attack-delay 2)
  (offhand-penalty 2/3))

(defmethod attack-damage ((item light-weapon) attacker)
  (let ((level (+ (level item) (quality item))))
    (cons (+ 2 level)
          (+ 4 (* 2 level)))))

;;; One-handed weapons are moderate in delay, damage per hit, and off-hand
;;; penalty.

(defproto one-handed-weapon (weapon)
  (slot :main-hand)
  (attack-delay 2.5)
  (offhand-penalty 1/2))

(defmethod attack-damage ((item one-handed-weapon) attacker)
  (let ((level (+ (level item) (quality item))))
    (cons (+ 4 (* 2 level))
          (+ 6 (* 3 level)))))

;;; Two-handed weapons are slow, do high damage per hit, and cannot be used in
;;; the off-hand since they require both hands.

(defproto two-handed-weapon (weapon)
  (slot :both-hands)
  (attack-delay 3))

(defmethod attack-damage ((item two-handed-weapon) attacker)
  (let ((level (+ (level item) (quality item))))
    (cons (+ 6 (* 3 level))
          (+ 10 (* 5 level)))))

;;; Natural weapons are things like fists, claws, and teeth. Their damage and
;;; delay are moderate like one-handed weapons. They scale based on attacker
;;; level. If `attack-delay` is adjusted, damage is normalized based on the
;;; default delay of 2.5 seconds to maintain the expected DPS. The
;;; `damage-scale` slot allows for easy adjustment of the DPS output.

(defproto natural-weapon (weapon)
  (weapon-type "natural weapon")
  (attack-delay 2.5)
  (damage-scale 1.0))

(defmethod attack-damage ((item natural-weapon) (attacker combatant))
  (with-slots (level) attacker
    (with-slots (attack-delay damage-scale) item
      (let ((k (* damage-scale (/ attack-delay 2.5))))
        (cons (* k (+ 4 (* 2 level)))
              (* k (+ 6 (* 3 level))))))))

(defmethod attack-message (observer attacker target (attack natural-weapon))
  (format nil "~a ~a ~a"
          (if (eq observer attacker) "You" (describe-brief attacker :capitalize t))
          (if (eq observer attacker)
              (verb-plural (attack-verb attack))
              (verb-singular (attack-verb attack)))
          (if (eq observer target) "you" (describe-brief target))))

;;;

(defmethod describe-full ((subject weapon))
  (destructuring-bind (min . max) (attack-damage subject nil)
    ;; FIXME: show quality if > 0
    (format nil
            "~a (level ~d ~a; damage ~d-~d ~(~a~); DPS ~,1f~{; ~(~a~) ~d~})"
            (call-next-method)
            (level subject)
            (weapon-type subject)
            min max
            (damage-type subject)
            (/ (+ min max)
               (attack-delay subject)
               2.0)
            (equipment-modifiers-plist subject))))
