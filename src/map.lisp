(in-package :charm)

(defun direction-offset (direction)
  (case direction
    (:north '(0 -1 0))
    (:northeast '(1 -1 0))
    (:east '(1 0 0))
    (:southeast '(1 1 0))
    (:south '(0 1 0))
    (:southwest '(-1 1 0))
    (:west '(-1 0 0))
    (:northwest '(-1 -1 0))
    (:up '(0 0 1))
    (:down '(0 0 -1))
    (t '(0 0 0))))

(defun walk-map (origin radius)
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
                                  (<= (max (abs (+ x dx)) (abs (+ y dy))) radius))
                         (let ((dest (symbol-value (and (boundp (destination exit)) (destination exit)))))
                           (when (and dest (null (gethash dest visited)))
                             (recursive-walk (+ x dx) (+ y dy) dest))))))))))
      (recursive-walk 0 0 origin)
      result)))

(defun show-map (avatar &optional (radius 3))
  (let ((locations (walk-map (location avatar) radius)))
    (send-client-command
     (session avatar) "updateMap"
     (describe-brief (location avatar))
     "A Zone" ; FIXME:
     radius
     (mapcar #'(lambda (info)
                 (destructuring-bind (x y location) info
                   (list x y
                         (describe-brief location)
                         :false ; icon
                         :false ; (quest-state avatar location)
                         :false ; (npc-state location)
                         (mapcar #'direction
                                 (remove-if-not #'(lambda (x) (is-visible-p x avatar)) (exits location)))
                         (surface location)
                         (domain location))))
             locations))))

(defun update-map-nearby (location &optional (radius 3))
  "Calls `show-map` for every avatar within `radius` of `location`. This is used
  when the map changes; for example, when a vehicle moves."
  (dolist (x (walk-map location radius))
    (destructuring-bind (x y loc) x
      (declare (ignore x y))
      (dolist (entity (contents loc))
        (when (typep entity 'avatar)
          (show-map entity radius))))))
