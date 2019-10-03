(in-package :lib)

(defstruct waypoint
  ;; where the vehicle stops
  location
  ;; portal from the vehicle to location
  exit
  ;; portal to the vehicle from location
  entry)

(defun make-waypoints (vehicle waypoint-info)
  (apply #'circular-list
         (loop for (class dir dest) in waypoint-info
               collect (make-waypoint :location dest
                                      :exit (make-instance class
                                                           :direction dir
                                                           :destination dest)
                                      :entry (make-instance class
                                                            :direction (opposite-direction dir)
                                                            :destination (type-of vehicle))))))

(defbehavior move-vehicle (vehicle waypoint-info)
    ((waypoints (make-waypoints vehicle waypoint-info)))
  ;;
  (:start
   (let ((curr (first waypoints)))
     (remove-exit vehicle (direction (waypoint-exit curr)))
     (remove-exit (symbol-value (waypoint-location curr)) (direction (waypoint-entry curr))))
   (change-state :arrive))
  ;;
  (:arrive
   (let* ((prev (first waypoints))
          (prev-location (symbol-value (waypoint-location prev)))
          (curr (nth 1 waypoints))
          (curr-location (symbol-value (waypoint-location curr))))
     (announce vehicle "~a arrives at ~a."
               (describe-brief vehicle :capitalize t)
               (describe-brief curr-location))
     (announce curr-location "~a arrives from ~a."
               (describe-brief vehicle :capitalize t)
               (describe-brief prev-location))
     (add-exit vehicle (waypoint-exit curr))
     (add-exit curr-location (waypoint-entry curr))
     (update-map-nearby curr-location)
     (pop waypoints)
     (change-state :announce-departure 20)))
  ;;
  (:announce-departure
   (let ((curr-location (symbol-value (waypoint-location (first waypoints))))
         (next-location (symbol-value (waypoint-location (nth 1 waypoints)))))
     (announce vehicle "~a will depart for ~a in a moment."
               (describe-brief vehicle :capitalize t)
               (describe-brief next-location))
     (announce curr-location "~a is about to depart for ~a. All aboard!"
               (describe-brief vehicle :capitalize t)
               (describe-brief next-location))
     (change-state :depart 5)))
  ;;
  (:depart
   (let* ((curr (first waypoints))
          (curr-location (symbol-value (waypoint-location curr)))
          (next (nth 1 waypoints))
          (next-location (symbol-value (waypoint-location next)))
          (msg (format nil "~a departs for ~a."
                       (describe-brief vehicle :capitalize t)
                       (describe-brief next-location))))
     (announce vehicle msg)
     (announce curr-location msg)
     (remove-exit vehicle (direction (waypoint-exit curr)))
     (remove-exit curr-location (direction (waypoint-entry curr)))
     (update-map-nearby curr-location)
     (update-map-nearby vehicle)
     (change-state :arrive 3))))
