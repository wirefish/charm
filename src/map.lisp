(in-package :charm)

(defun walk-map (origin radius &optional observer)
  (let* ((result nil)
         (visited (make-hash-table)))
    (labels ((recursive-walk (x y location)
               (push (list x y location) result)
               (setf (gethash location visited) t)
               (with-slots (domain exits) location
                 (when (eq domain (domain origin))
                   (dolist (exit exits)
                     (destructuring-bind (dx dy dz) (direction-offset (direction exit))
                       (when (and (= dz 0)
                                  (or (/= dx 0) (/= dy 0))
                                  (<= (max (abs (+ x dx)) (abs (+ y dy))) radius))
                         (let ((dest (symbol-value (and (or (null observer) (visible-p exit observer))
                                                        (boundp (destination exit))
                                                        (destination exit)))))
                           (when (and dest (null (gethash dest visited)))
                             (recursive-walk (+ x dx) (+ y dy) dest))))))))))
      (recursive-walk 0 0 origin)
      result)))

(defun portal-visible-p (portal observer)
  (or (visible-p portal observer)
      (and (boundp (destination portal))
           (eq (symbol-value (destination portal)) (location observer)))))

(defun show-map (avatar &optional (radius 3))
  (let ((locations (walk-map (location avatar) radius avatar)))
    (send-client-command
     (session avatar) "updateMap"
     (describe-brief (location avatar))
     (region-name (region (location avatar)))
     radius
     (mapcar #'(lambda (info)
                 (destructuring-bind (x y location) info
                   (list x y
                         (describe-brief location)
                         (describe-icon location)
                         :false ; (quest-state avatar location)
                         :false ; (npc-state location)
                         (mapcar #'direction
                                 (remove-if-not #'(lambda (x) (portal-visible-p x avatar)) (exits location)))
                         (surface location)
                         (domain location))))
             (remove-if-not #'(lambda (loc)
                                (= (z-offset (third loc)) (z-offset (location avatar))))
                            locations)))))

(defun update-map-nearby (location &optional (radius 3))
  "Calls `show-map` for every avatar within `radius` of `location`. This is used
  when the map changes; for example, when a vehicle moves."
  (dolist (x (walk-map location radius))
    (destructuring-bind (x y loc) x
      (declare (ignore x y))
      (dolist (entity (contents loc))
        (when (typep entity 'avatar)
          (show-map entity radius))))))
