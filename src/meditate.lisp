(in-package :charm)

;;;; Meditation has an effect based on the actor's location. Typically it
;;;; regenerates mana and energy at an increased rate for a short time.

(defevent meditate (actor))

(defmethod do-meditate (actor)
  (show-text actor "You begin to meditate.")
  (with-delay (3)
    (show-text actor "Your meditation is complete.")
    (notify-observers (location actor) #'did-meditate actor)))

(defcommand (actor "meditate")
  "Use the meditate command to spend a few moments focusing your mind. This
  action may have side effects depending on your location."
  (do-meditate actor))
