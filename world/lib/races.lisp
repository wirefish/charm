(in-package :lib)

(defentity reborn-hero (race)
  (brief "a reborn hero")
  (full "The reborn hero has a ghostly humanoid form. Its features shift in an
    unpredictable fashion, as if it hasn't quite settled on a final shape."))

(setf charm::*new-avatar-race* reborn-hero)

(defentity elf (race)
  (brief "an el[f|ves]")
  (full "Elves are known for their gruff voices and long beards. Right?"))

(defentity human (race)
  (brief "a human")
  (full "Humans are all fat and scruffy. Right?"))

(defentity sidhe (race)
  (brief "a sidhe")
  (full "The sidhe are very jolly people. Right?")
  (modifiers '(:shadow 5)))

(defentity dwarf (race)
  (brief "a dwar[f|ves]")
  (full "Dwarves are tall and graceful, as a rule. Right?")
  (modifiers '(:resilience 10)))

(defentity goblin (race)
  (brief "a goblin")
  (full "Goblins are honest and open. Right?"))

(defentity ogre (race)
  (brief "an ogre")
  (full "Ogres are petite and have impeccable manners. Right?")
  (base-health 18))
