;;;; Events and commands related to inspecting entities.

(in-package :charm)

(defevent look (actor target tool))

(defmethod do-look (actor target tool)
  (show-description actor target))

(defcommand (actor "look" "at" target "with" tool)
  "Look at the target."
  (cond
    ;; Look at the actor's location.
    ((null target)
     (show-location actor))
    ;; Look at the actor.
    ((and (= (length target) 1) (string-equal (first target) "self"))
     (show-description actor actor))
    ;; Look within the location and the actor's inventory.
    (t
     (let ((matches (match-objects target
                                   (keep-visible actor (contents (location actor)))
                                   (exits (location actor)))))
       (if (= (length matches) 0)
           (show-text actor "You don't see anything like that.")
           (dolist (m matches)
             (do-look actor m tool)))))))
