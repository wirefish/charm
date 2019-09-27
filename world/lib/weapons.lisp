(in-package :lib)

;;; Basic weapons share a common proficiency and generally do less damage than
;;; martial weapons. All of them are scaled to do an average base of 1.5 damage
;;; per second with different variances and attack speeds.

(defproto basic-weapon (weapon)
  (proficiency :basic-weapon-proficiency))

;;; Clubs are basic one-handed bludgeoning weapons.

(defproto club (basic-weapon)
  (brief "a club")
  (full "This heavy stick is ideal for bashing things. It is sized to be wielded
    with one hand.")
  (slot :main-hand)
  (damage-type :crushing)
  (base-damage (2 4))
  (attack-verb "smashes")
  (attack-delay 2)
  (mastery :club-mastery))

;;; Daggers are basic one-handled piercing weapons.

(defproto dagger (basic-weapon)
  (brief "a dagger")
  (full "This light weapon has a double-edged blade with a sharp point, a
    leather-wrapped handle, and a full crossguard.")
  (slot :either-hand)
  (damage-type :piercing)
  (base-damage (1 4))
  (attack-verb "pokes")
  (attack-delay 5/3)
  (mastery :dagger-mastery))

;;; Staves are basic two-handed bludgeoning weapons.

(defproto staff (basic-weapon)
  (brief "a sta[ff|ves]")
  (full "This stout wooden pole has ends capped with iron. It requires both
    hands to be wielded effectively.")
  (slot :both-hands)
  (damage-type :crushing)
  (base-damage (3 5))
  (attack-verb "smashes")
  (attack-delay 8/3)
  (mastery :staff-mastery))

;;; Martial weapons each have their own proficiency and have a higher base
;;; damage per second than basic weapons.

;;; Shields are equippable in the :off-hand slot and confer a chance to block
;;; attacks.

(defproto shield (weapon)
  (slot :off-hand))

(defproto small-shield (shield)
  (proficiency :small-shield-proficiency))
