(in-package :shadowlands)

(defregion shadowlands
  (name "The Shadowlands")
  (full "This realm lies far beyond the physical plane.")
  (climate :astral))

(deflocation limbo (location)
  (brief "Limbo")
  (full "May your stay here be mercifully brief."))

;;; Define what happens to an avatar after death.

(defproto death-entry (portal))

(defmethod describe-entry (observer actor location (entry death-entry))
  (if (not (eq observer actor))
      (format nil "A cloud of mist begins to gather, and slowly coalesces into the shape of ~a."
              (describe-brief actor))))

(defproto rebirth-exit (portal))

(defmethod describe-exit (observer actor location (exit rebirth-exit))
  (format nil "~a fades away." (describe-brief actor :capitalize t)))

(defproto rebirth-entry (portal))

(defmethod describe-entry (observer actor location (entry rebirth-entry))
  (format nil "A cloud of black smoke appears, and out steps ~a." (describe-brief actor)))

(defmethod do-die :after ((actor avatar))
  (show-notice actor "You are dead.")
  (enter-world actor limbo (make-instance 'death-entry))
  (with-delay (5)
    (show-text actor "Ah, the sweet release of death.")
    (with-delay (5)
      (show-text actor "Unfortunately, your time has not yet come.")
      (with-delay (5)
        (show-text actor "You feel an irresistable pull back toward the physical realm...")
        (with-delay (5)
          (exit-world actor (location actor) (make-instance 'rebirth-exit))
          (enter-world actor
                       (let ((recall (recall-location actor)))
                         (or (and recall (boundp recall) (symbol-value recall))
                             charm::*new-avatar-location*))
                       (make-instance 'rebirth-entry)))))))
