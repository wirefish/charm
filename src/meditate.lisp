(in-package :charm)

;;;; Meditation has an effect based on the actor's location. Typically it
;;;; regenerates mana and energy at an increased rate for a short time.

(defevent meditate (actor))

(defmethod do-meditate (actor)
  (show-text actor "Your meditation is complete."))

(defmethod do-meditate :after (actor)
  (notify-observers (location actor) #'did-meditate actor))

(defbehavior meditate-behavior (actor)
    ()
  (:start
   (show-text actor "You begin to meditate.")
   (start-casting actor 3)
   (change-state :finish 3))
  (:finish
   (do-meditate actor)
   (remove-behavior actor :activity))
  (:stop
   (show-text actor "Your meditation has been interrupted.")
   (stop-casting actor)))

(defcommand (actor "meditate")
  "Use the meditate command to spend a few moments focusing your mind. This
  action may have side effects depending on your location."
  (start-behavior actor :activity #'meditate-behavior))
