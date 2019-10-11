(in-package :charm)

;;;

(defevent exit-world (actor location))

(defmethod do-exit-world (actor location)
  (remove-from-container actor location)
  (stop-all-behaviors actor)
  (notify-observers location #'did-exit-location actor location nil))

(defmethod do-exit-world ((actor location) location)
  (let ((contents (copy-list (contents actor))))
    (dolist (entity contents)
      (exit-world entity actor))
    (setf (contents actor) contents))
  (stop-all-behaviors actor))

;;;

(defevent enter-world (actor location))

(defmethod do-enter-world (actor location)
  (do-enter-location actor location nil))

(defmethod do-enter-world ((actor location) location)
  (let ((contents (contents actor)))
    (setf (contents actor) nil)
    (dolist (entity contents)
      (enter-world entity actor))))

(defmethod do-enter-world ((actor avatar) location)
  ;; TODO: Show intro if new player. Or maybe intro, delay, then enter world.
  (update-avatar actor :name (name actor)
                       :level (level actor)
                       :race (describe-brief (race actor) :article nil)
                       :health (health actor)
                       :max-health (max-health actor)
                       :energy (energy actor)
                       :max-energy (max-energy actor)
                       :mana (mana actor)
                       :max-mana (max-mana actor)
                       :xp (xp actor)
                       :xp-required (xp-required-for-next-level actor))
  (call-next-method))

;;;

(defevent enter-location (actor location entry))

(defevent exit-location (actor location exit))

(defun resolve-destination (portal)
  (let ((dest (destination portal)))
    (and dest (boundp dest) (symbol-value dest))))

(defun find-entry-portal (exit dest)
  (when dest
    (let ((entry-dir (opposite-direction (direction exit))))
      (find-if #'(lambda (x) (eq (direction x) entry-dir)) (exits dest)))))

(defun respawn (entity location)
  (when-let ((delay (respawn-delay entity)))
    (when (listp delay)
      (setf delay (apply #'uniform-random delay)))
    (with-delay (delay)
      (do-enter-location (make-instance (type-of entity)) location nil))))

(defmethod do-exit-location (actor location exit)
  (stop-behavior actor :activity)
  (remove-from-container actor location))

(defmethod do-exit-location :around (actor location exit)
  (let* ((dest (resolve-destination exit))
         (entry (find-entry-portal exit dest)))
    (cond
      ;; Check that the portal leads somewhere.
      ((not dest)
       (show-text actor "You cannot go that way."))
      ;; Check that the actor is no larger than the portal.
      ((size> (size actor) (size exit))
       (show-text actor "You are too large to go that way."))
      ;; Check that observers allow the action. Note that this is special
      ;; because the exit and entry portals are additional observers, the latter
      ;; for the can-enter-location event phase.
      ((and (query-observers location #'can-exit-location actor location exit)
            (can-exit-location exit actor location exit)
            (can-enter-location entry actor dest entry))
       (call-next-method)
       (values dest entry)))))

(defmethod do-exit-location :before (actor location exit)
  (notify-observers location #'will-exit-location actor location exit)
  (will-exit-location exit actor location exit))

(defmethod do-exit-location :after (actor location exit)
  (notify-observers location #'did-exit-location actor location exit)
  (did-exit-location exit actor location exit))

(defmethod do-exit-location :after ((actor combatant) location exit)
  ;; FIXME: where should this go? Should target/opponents be part of the
  ;; autoattack/combat behavior?
  (when (opponents actor)
    (show-text actor "You are no longer in combat.")
    (setf (opponents actor) nil)))

(defmethod did-exit-location ((observer combatant) actor location exit)
  (when (member actor (opponents observer))
    (deletef (opponents observer) actor)
    (when (eq actor (attack-target observer))
      ;; FIXME: should be more general so monsters can switch targets and keep
      ;; attacking.
      (stop-behavior observer :activity))))

(defmethod did-exit-location ((observer avatar) actor location exit)
  ;; TODO: handle exit-specific messages.
  (when (and (not (eq observer actor))
             (visible-p actor observer))
    (show-text observer (describe-exit observer actor location exit))
    (remove-neighbor observer actor))
  (call-next-method))

;;;

(defmethod do-enter-location (actor location entry)
  (add-to-container actor location))

(defmethod do-enter-location ((actor avatar) location entry)
  (call-next-method)
  (show-location actor)
  (show-map actor)
  (set-neighbors actor)
  (when (tutorial location)
    (maybe-show-tutorial actor (type-of location) (tutorial location))))

(defmethod do-enter-location :before (actor location entry)
  (notify-observers location #'will-enter-location actor location entry)
  (when entry
    (will-enter-location entry actor location entry)))

(defmethod do-enter-location :after (actor location entry)
  (notify-observers location #'did-enter-location actor location entry)
  (when entry
    (did-enter-location entry actor location entry)))

(defmethod did-enter-location ((observer avatar) actor location entry)
  ;; TODO: handle entry-specific messages.
  (when (and (not (eq observer actor))
             (visible-p actor observer))
    (show-text observer (describe-entry observer actor location entry))
    (update-neighbor observer actor)))

(defun traverse-portal (actor exit)
  "This is a wrapper around the exit-location and enter-location events."
  (multiple-value-bind (dest entry) (do-exit-location actor (location actor) exit)
    (when dest
      (do-enter-location actor dest entry))))

(defcommand (actor "go" direction)
  "Move in a specified direction; for example, `go north`. See `help:movement`
  for more information."
  (let ((matches (match-objects direction
                                (keep-visible actor (contents (location actor)))
                                (exits (location actor)))))
    (case (length matches)
      (0 (show-text actor "You can't move in that direction."))
      (1 (traverse-portal actor (first matches)))
      (otherwise (show-text actor
                            "Do you want to go ~a?"
                            (format-list (mapcar #'describe-brief matches)))))))

(maphash-keys #'(lambda (dir)
                  (let ((command (format nil "go ~a" (direction-name dir))))
                    (make-alias (direction-name dir) command)
                    (when-let ((abbrev (direction-abbrev dir)))
                      (make-alias abbrev command))))
              *directions*)
