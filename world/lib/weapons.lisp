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
  (icon 'knives-01)
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
  (icon 'staves-01)
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
  (icon 'bluntweapons-05)
  (full "This weapon has a heavy metal ball attached to a stout wooden handle,
    sized for use in one hand.")
  (damage-type :crushing)
  (attack-verb "smashes")
  (weapon-type "one-handed mace")
  (proficiency 'mace-proficiency)
  (mastery 'mace-mastery))

;;; Two-handed maces are crushing weapons.

(defskill two-handed-mace-proficiency
  (name "two-handed mace proficiency")
  (summary "Increases damage done when wielding two-handed maces.")
  (cost '(:karma 5))
  (max-rank 100))

(defskill two-handed-mace-mastery
  (name "two-handed mace mastery")
  (summary "Further increases damage done when wielding two-handed maces.")
  (level 20)
  (required-skills '(two-handed-mace-proficiency 100))
  (cost '(:karma 10))
  (max-rank 200))

(defproto two-handed-mace (two-handed-weapon)
  (brief "a mace")
  (icon 'bluntweapons-05)
  (full "This weapon has a heavy metal ball attached to a long wooden handle,
    and requires both hands to wield.")
  (damage-type :crushing)
  (attack-verb "smashes")
  (weapon-type "two-handed mace")
  (proficiency 'two-handed-mace-proficiency)
  (mastery 'two-handed-mace-mastery))

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
  (icon 'swords-01)
  (full "This weapon has a long metal blade attached to a plain wooden hilt,
    sized for use in one hand.")
  (damage-type :slashing)
  (attack-verb "slashes")
  (weapon-type "one-handed sword")
  (proficiency 'sword-proficiency)
  (mastery 'sword-mastery))

;;; Two-handed swords are two-handed slashing weapons.

(defskill two-handed-sword-proficiency
  (name "two-handed sword proficiency")
  (summary "Increases damage done when wielding two-handed swords.")
  (cost '(:karma 5))
  (max-rank 100))

(defskill two-handed-sword-mastery
  (name "two-handed sword mastery")
  (summary "Further increases damage done when wielding two-handed swords.")
  (level 20)
  (required-skills '(two-handed-sword-proficiency 100))
  (cost '(:karma 10))
  (max-rank 200))

(defproto two-handed-sword (two-handed-weapon)
  (brief "a sword")
  (icon 'swords-01)
  (full "This weapon has a long, dual-edged blade and requires both hands to
    wield.")
  (damage-type :slashing)
  (attack-verb "slashes")
  (weapon-type "two-handed sword")
  (proficiency 'two-handed-sword-proficiency)
  (mastery 'two-handed-sword-mastery))

;;; Axes are one-handed slashing weapons.

(defskill axe-proficiency
  (name "axe proficiency")
  (summary "Increases damage done when wielding one-handed axes.")
  (cost '(:karma 5))
  (max-rank 100))

(defskill axe-mastery
  (name "axe mastery")
  (summary "Further increases damage done when wielding one-handed axes, and
    allows their use in your off hand.")
  (level 20)
  (required-skills '(axe-proficiency 100))
  (cost '(:karma 10))
  (max-rank 200))

(defproto axe (one-handed-weapon)
  (brief "an axe")
  (icon 'axes-01)
  (full "This weapon has a curved blade attached to one end of a wooden shaft,
    sized for use in one hand.")
  (damage-type :slashing)
  (attack-verb "slashes")
  (weapon-type "one-handed axe")
  (proficiency 'axe-proficiency)
  (mastery 'axe-mastery))

;;; Two-handed axes are two-handed slashing weapons.

(defskill two-handed-axe-proficiency
  (name "two-handed axe proficiency")
  (summary "Increases damage done when wielding two-handed axes.")
  (cost '(:karma 5))
  (max-rank 100))

(defskill two-handed-axe-mastery
  (name "two-handed axe mastery")
  (summary "Further increases damage done when wielding two-handed axes.")
  (level 20)
  (required-skills '(two-handed-axe-proficiency 100))
  (cost '(:karma 10))
  (max-rank 200))

(defproto two-handed-axe (two-handed-weapon)
  (brief "a axe")
  (icon 'axes-01)
  (full "This weapon has a large blade attached to one end of a long wooden
    haft, and requires both hands to wield.")
  (damage-type :slashing)
  (attack-verb "slashes")
  (weapon-type "two-handed axe")
  (proficiency 'two-handed-axe-proficiency)
  (mastery 'two-handed-axe-mastery))

;;; Spears are two-handed piercing weapons.

(defskill spear-proficiency
  (name "spear proficiency")
  (summary "Increases damage done when wielding spears.")
  (cost '(:karma 5))
  (max-rank 100))

(defskill spear-mastery
  (name "spear mastery")
  (summary "Further increases damage done when wielding spears.")
  (level 20)
  (required-skills '(spear-proficiency 100))
  (cost '(:karma 10))
  (max-rank 200))

(defproto spear (two-handed-weapon)
  (brief "a spear")
  (icon 'spears-01)
  (full "This weapon has a pointed blade attached to one end of a long wooden
    haft, and requires both hands to wield.")
  (damage-type :piercing)
  (attack-verb "pierces")
  (weapon-type "spear")
  (proficiency 'spear-proficiency)
  (mastery 'spear-mastery))

;;; Staves are two-handed magical or elemental weapons.

(defskill staff-proficiency
  (name "staff proficiency")
  (summary "Increases damage done when wielding staves.")
  (cost '(:karma 5))
  (max-rank 100))

(defskill staff-mastery
  (name "staff mastery")
  (summary "Further increases damage done when wielding staves.")
  (level 20)
  (required-skills '(staff-proficiency 100))
  (cost '(:karma 10))
  (max-rank 200))

(defproto staff (two-handed-weapon)
  (brief "a staff")
  (icon 'staves-04)
  (full "This two-handed weapon is a polished length of wood that radiates a
    strong magical aura.")
  (damage-type :arcane)
  (attack-verb "zaps")
  (weapon-type "staff")
  (proficiency 'staff-proficiency)
  (mastery 'staff-mastery))
