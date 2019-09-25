(in-package :lib)

(defproto dagger (weapon)
  (brief "a dagger")
  (full "This weapon has a double-edged blade with a sharp point, a
    leather-wrapped handle, and a full crossguard.")
  (slot :either-hand)
  (damage-type :piercing)
  (base-damage (1 4))
  (attack-verb "pokes")
  (attack-delay 1.5)
  (proficiency :dagger-proficiency)
  (allow-nonproficient-use 1/2)
  (mastery :dagger-mastery))

(defproto shield (weapon)
  (slot :off-hand))
