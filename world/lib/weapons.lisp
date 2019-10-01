(in-package :lib)

;;;; Defines the available types of weapons and their associated proficiency and
;;;; mastery skills.

;;; Daggers are light piercing weapons. Note that dagger proficiency is free.

(defskill dagger-proficiency
  (name "dagger proficiency")
  (summary "Increases damage done when wielding daggers.")
  (max-rank 100))

(defskill dagger-mastery
  (name "dagger mastery")
  (summary "Further increases damage done when wielding daggers, and allows
    their use in your off hand.")
  (level 20)
  (required-skills '(dagger-proficiency 100))
  (cost '(:karma 10))
  (max-rank 200))

(defproto dagger (light-weapon)
  (brief "a dagger")
  (full "This light weapon has a double-edged blade with a sharp point, a
    leather-wrapped handle, and a full crossguard.")
  (damage-type :piercing)
  (attack-verb "pokes")
  (weapon-type "dagger")
  (proficiency 'dagger-proficiency)
  (mastery 'dagger-mastery))

;;; Wands are light weapons that do magical or elemental damage, depending on
;;; the specific wand.

(defskill wand-proficiency
  (name "wand proficiency")
  (summary "Increases damage done when wielding wands.")
  (cost '(:karma 5))
  (max-rank 100))

(defskill wand-mastery
  (name "wand mastery")
  (summary "Further increases damage done when wielding wands, and allows
    their use in your off hand.")
  (level 20)
  (required-skills '(wand-proficiency 100))
  (cost '(:karma 10))
  (max-rank 200))

(defproto wand (light-weapon)
  (brief "a wand")
  (full "This light weapon appears to be a simple piece of polished wood, but it
    radiates a magical aura.")
  (damage-type :arcane)
  (attack-verb "zaps")
  (weapon-type "wand")
  (proficiency 'wand-proficiency)
  (mastery 'wand-mastery))

;;; Maces are one-handed crushing weapons.

(defskill mace-proficiency
  (name "mace proficiency")
  (summary "Increases damage done when wielding one-handed maces.")
  (cost '(:karma 5))
  (max-rank 100))

(defskill mace-mastery
  (name "mace mastery")
  (summary "Further increases damage done when wielding one-handed maces, and
    allows their use in your off hand.")
  (level 20)
  (required-skills '(mace-proficiency 100))
  (cost '(:karma 10))
  (max-rank 200))

(defproto mace (one-handed-weapon)
  (brief "a mace")
  (full "This weapon has a heavy metal ball attached to a stout wooden handle,
    sized for use in one hand.")
  (damage-type :crushing)
  (attack-verb "smashes")
  (weapon-type "one-handed mace")
  (proficiency 'mace-proficiency)
  (mastery 'mace-mastery))

;;; Two-handed maces are crushing weapons.

(defskill mace-2h-proficiency
  (name "two-handed mace proficiency")
  (summary "Increases damage done when wielding two-handed maces.")
  (cost '(:karma 5))
  (max-rank 100))

(defskill mace-2h-mastery
  (name "two-handed mace mastery")
  (summary "Further increases damage done when wielding two-handed maces.")
  (level 20)
  (required-skills '(mace-2h-proficiency 100))
  (cost '(:karma 10))
  (max-rank 200))

(defproto mace-2h (two-handed-weapon)
  (brief "a mace")
  (full "This weapon has a heavy metal ball attached to a long wooden handle,
    and requires both hands to wield.")
  (damage-type :crushing)
  (attack-verb "smashes")
  (weapon-type "two-handed mace")
  (proficiency 'mace-2h-proficiency)
  (mastery 'mace-2h-mastery))

;;; Swords are one-handed slashing (and sometimes piercing) weapons.

(defskill sword-proficiency
  (name "sword proficiency")
  (summary "Increases damage done when wielding one-handed swords.")
  (cost '(:karma 5))
  (max-rank 100))

(defskill sword-mastery
  (name "sword mastery")
  (summary "Further increases damage done when wielding one-handed swords, and
    allows their use in your off hand.")
  (level 20)
  (required-skills '(sword-proficiency 100))
  (cost '(:karma 10))
  (max-rank 200))

(defproto sword (one-handed-weapon)
  (brief "a sword")
  (full "This weapon has a long metal blade attached to a plain wooden hilt,
    sized for use in one hand.")
  (damage-type :slashing)
  (attack-verb "slashes")
  (weapon-type "one-handed sword")
  (proficiency 'sword-proficiency)
  (mastery 'sword-mastery))

;;; TODO: add two-handed swords, one- and two-handed axes, quarterstaves (2H
;;; crushing), polearms (2H slashing/piercing), rods (1H magical), and staves
;;; (2H magical).
