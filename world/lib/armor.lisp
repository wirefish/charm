(in-package :lib)

;;;; Defines some basic armor types and their associated proficiencies, if any.

;;; Clothing is armor that provides no protection. This includes all belts and
;;; cloaks.

(defproto clothing (armor)
  (armor-type "clothing")
  (armor-value 0))

(defproto clothing-head (clothing)
  (slot :head)
  (icon 'mage1-01))

(defproto clothing-torso (clothing)
  (slot :torso)
  (icon 'mage1-02))

(defproto clothing-hands (clothing)
  (slot :hands)
  (icon 'mage1-05))

(defproto clothing-legs (clothing)
  (slot :legs)
  (icon 'mage1-03))

(defproto clothing-feet (clothing)
  (slot :feet)
  (icon 'mage1-06))

(defproto cloak (clothing)
  (slot :back)
  (icon 'cloaks-01))

(defproto belt (clothing)
  (slot :waist)
  (icon 'belts-01))

;;; Cloth armor is armor that requires no proficiency but provides little
;;; protection.

(defproto cloth-armor (armor)
  (armor-type "cloth armor")
  (armor-value 1/4))

(defproto cloth-armor-head (cloth-armor)
  (slot :head)
  (icon 'mage2-01))

(defproto cloth-armor-torso (cloth-armor)
  (slot :torso)
  (icon 'mage2-02))

(defproto cloth-armor-hands (cloth-armor)
  (slot :hands)
  (icon 'mage2-05))

(defproto cloth-armor-legs (cloth-armor)
  (slot :legs)
  (icon 'mage2-03))

(defproto cloth-armor-feet (cloth-armor)
  (slot :feet)
  (icon 'mage2-06))

;;; Leather armor requires no proficiency but provides a little more protection.

(defproto leather-armor (armor)
  (armor-value 1/2))

(defproto leather-armor-head (leather-armor)
  (slot :head)
  (icon 'leather-01))

(defproto leather-armor-torso (leather-armor)
  (slot :torso)
  (icon 'leather-02))

(defproto leather-armor-hands (leather-armor)
  (slot :hands)
  (icon 'leather-05))

(defproto leather-armor-legs (leather-armor)
  (slot :legs)
  (icon 'leather-03))

(defproto leather-armor-feet (leather-armor)
  (slot :feet)
  (icon 'leather-06))

;;; Mail armor requires proficiency and provides even more protection.

(defskill mail-armor-proficiency
  (name "mail armor proficiency")
  (summary "Allows you to fight effectively while wearing mail armor.")
  (level 10)
  (cost '(:karma 5)))

(defproto mail-armor (armor)
  (armor-type "mail armor")
  (proficiency 'mail-armor-proficiency)
  (armor-value 3/4))

(defproto mail-armor-head (mail-armor)
  (slot :head)
  (icon 'chain-01))

(defproto mail-armor-torso (mail-armor)
  (slot :torso)
  (icon 'chain-02))

(defproto mail-armor-hands (mail-armor)
  (slot :hands)
  (icon 'chain-05))

(defproto mail-armor-legs (mail-armor)
  (slot :legs)
  (icon 'chain-03))

(defproto mail-armor-feet (mail-armor)
  (slot :feet)
  (icon 'chain-06))

;;; Plate armor also requires proficiency and provides maximum protection.

(defskill plate-armor-proficiency
  (name "plate armor proficiency")
  (summary "Allows you to fight effectively while wearing plate armor.")
  (level 25)
  (cost '(:karma 5)))

(defproto plate-armor (armor)
  (armor-type "plate armor")
  (proficiency 'plate-armor-proficiency)
  (armor-value 1))

(defproto plate-armor-head (plate-armor)
  (slot :head)
  (icon 'plate1-01))

(defproto plate-armor-torso (plate-armor)
  (slot :torso)
  (icon 'plate1-02))

(defproto plate-armor-hands (plate-armor)
  (slot :hands)
  (icon 'plate1-05))

(defproto plate-armor-legs (plate-armor)
  (slot :legs)
  (icon 'plate1-03))

(defproto plate-armor-feet (plate-armor)
  (slot :feet)
  (icon 'plate1-06))
