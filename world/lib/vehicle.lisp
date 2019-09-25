(in-package :lib)

(defstruct waypoint
  ;; where the vehicle stops
  location
  ;; portal from the vehicle to location
  exit
  ;; portal to the vehicle from location
  entry)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod transform-slot-init-form (def-name (slot-name (eql 'waypoints)) init-form)
    "Converts a list whose items are (portal-class direction destination) into
    an equal number of `waypoint` structures."
    (when init-form
      `(circular-list ,@(loop for (class dir dest) in init-form
                              collect `(make-waypoint
                                        :location ',dest
                                        :exit (make-instance ',class :direction ,dir :destination ',dest)
                                        :entry (make-instance ',class :direction ,(opposite-direction dir)
                                                                      :destination ',def-name)))))))

(defproto vehicle (location state-machine)
  (initial-state :arrive)
  (waypoints nil))

(defmethod do-change-state ((actor vehicle) (state (eql :arrive)))
  (with-slots (waypoints) actor
    (let* ((prev (first waypoints))
           (prev-location (symbol-value (waypoint-location prev)))
           (curr (nth 1 waypoints))
           (curr-location (symbol-value (waypoint-location curr))))
      (announce actor "~a arrives at ~a."
                (describe-brief actor :capitalize t)
                (describe-brief curr-location))
      (announce curr-location "~a arrives from ~a."
                (describe-brief actor :capitalize t)
                (describe-brief prev-location))
      (add-exit actor (waypoint-exit curr))
      (add-exit curr-location (waypoint-entry curr))
      (update-map-nearby curr-location)
      (pop waypoints)
      (with-delay (20)
        (change-state actor :announce-departure)))))

(defmethod do-change-state ((actor vehicle) (state (eql :announce-departure)))
  (with-slots (waypoints) actor

    (let ((curr-location (symbol-value (waypoint-location (first waypoints))))
          (next-location (symbol-value (waypoint-location (nth 1 waypoints)))))
      (announce actor "~a will depart for ~a in a moment."
                (describe-brief actor :capitalize t)
                (describe-brief next-location))
      (announce curr-location "~a is about to depart for ~a. All aboard!"
                (describe-brief actor :capitalize t)
                (describe-brief next-location))
      (with-delay (5)
        (change-state actor :depart)))))

(defmethod do-change-state ((actor vehicle) (state (eql :depart)))
  (with-slots (waypoints) actor
    (let* ((curr (first waypoints))
           (curr-location (symbol-value (waypoint-location curr)))
           (next (nth 1 waypoints))
           (next-location (symbol-value (waypoint-location next)))
           (msg (format nil "~a departs for ~a."
                        (describe-brief actor :capitalize t)
                        (describe-brief next-location))))
      (announce actor msg)
      (announce curr-location msg)
      (remove-exit actor (direction (waypoint-exit curr)))
      (remove-exit curr-location (direction (waypoint-entry curr)))
      (update-map-nearby curr-location)
      (update-map-nearby actor)
      (with-delay (3)
        (change-state actor :arrive)))))

(defmethod reset-state-machine ((actor vehicle))
  (let ((curr (first (waypoints actor))))
    (remove-exit actor (direction (waypoint-exit curr)))
    (remove-exit (waypoint-location curr) (direction (waypoint-entry curr)))))
