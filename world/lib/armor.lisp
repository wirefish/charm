(in-package :lib)

;;;; Defines some basic armor types and their associated proficiencies, if any.

;;; Clothing is armor that provides no protection.

(defproto clothing (armor)
  (armor-value 0))

;;; Cloth armor is armor that requires no proficiency but provides little
;;; protection.

(defproto cloth-armor (armor)
  (armor-value 1/4))

;;; Leather armor requires no proficiency but provides a little more protection.

(defproto leather-armor (armor)
  (armor-value 1/2))

;;; Mail armor requires proficiency and provides even more protection.

(defskill mail-armor-proficiency
  (name "mail armor proficiency")
  (summary "Allows you to fight effectively while wearing mail armor.")
  (level 10)
  (cost '(:karma 5)))

(defproto mail-armor (armor)
  (proficiency 'mail-armor-proficiency)
  (armor-value 3/4))

;;; Plate armor also requires proficiency and provides maximum protection.

(defskill plate-armor-proficiency
  (name "plate armor proficiency")
  (summary "Allows you to fight effectively while wearing plate armor.")
  (level 25)
  (cost '(:karma 5)))

(defproto plate-armor (armor)
  (proficiency 'plate-armor-proficiency)
  (armor-value 1))
