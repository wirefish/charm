(in-package :charm)

;;; Return the unadjusted amount damage done by an attack.

(defgeneric damage-amount (attack))

(defmethod damage-amount ((attack weapon))
  (with-slots (level base-damage) attack
    (uniform-random (* (1+ level) (first base-damage))
                    (* (1+ level) (second base-damage)))))

;;; Functions that determine the actual effect of combat modifiers when applied
;;; to an attack.

(defun offense-modifier (attacker attack)
  "Returns the amount by which damage from `attack` is increased based on the
  associated attribute of `attacker`."
  (case (damage-group (damage-type attack))
    ((:physical) (/ (get-modifier :strength attacker) 20))
    ((:elemental :magical) (/ (get-modifier :intellect attacker) 20))
    (t 0)))

(defun defense-modifier (defender attack)
  "Returns the amount by which damage from `attack` is decreased based on the
  associated attribute of `defender` and any immunity to the damage type."
  (with-slots (damage-type) attack
    (+ (get-modifier damage-type defender)
       (case (damage-group damage-type)
         ((:physical :elemental) (/ (get-modifier :armor defender) 20))
         ((:magical) (/ (get-modifier :willpower defender) 20))
         (t 0)))))

(defun compute-damage (attacker target attack)
  "Returns the damage done when `attacker` attacks `target` using `attack`, not
  accounting for critical hits."
  (destructuring-bind (min . max) (attack-damage attack attacker)
    (round-random
     (max 0
          (+ min
             (* (random 1.0) (- max min))
             (offense-modifier attacker attack)
             (- (defense-modifier target attack)))))))

(defun diminish (x)
  "Applies diminishing returns in order to keep a value in the range [0, 1].
  There are many functions that can be used here; this one returns the
  unmodified input in the range [0, 0.5] and 1 for inputs >= 1.5."
  (cond
    ((<= x 0.5) x)
    ((< x 1.5)
     (let ((y (- x 0.5)))
       (+ 0.5 (* y (- 1 (* 0.5 y))))))
    (t 1)))

(defun critical-chance (precision)
  "Returns the probability of a critical hit for an attacker with a given
  precision."
  ;; base 5%, 20 precision -> 1% increase, up to 25% at 600 precision
  (+ 0.05 (* 0.2 (diminish (* 0.0025 precision)))))

(defun critical-multiplier (ferocity)
  "Returns a damage multiplier for a critical hit by an attacker with a given
  ferocity."
  ;; base 1.5, 20 ferocity -> 0.1 increase, up to 2.5 max at 300 ferocity
  (+ 1.5 (diminish (* 0.005 ferocity))))

(defun miss-chance (accuracy)
  "Returns the probability of a miss for an attacker with the given accuracy."
  ;; base 5%, 20 accuracy -> 1% decrease, down to 0% at 150 accuracy
  (* 0.05 (- 1 (diminish (* 0.01 accuracy)))))

(defun evade-chance (evasion)
  "Returns the probability that a target with the given evasion dodges an
  attack."
  ;; base 5%, 20 evasion -> 1% increase, up to 10% at 150 evasion
  (* 0.05 (1+ (diminish (* 0.01 evasion)))))

;;; Functions that announce combat events to observers.

(defun announce-miss (attacker target attack)
  (dolist (observer (contents (location target)))
    (when (has-session-p observer)
      (show-text observer
                 "~a and ~a!"
                 (attack-message observer attacker target attack)
                 (if (eq observer attacker) "miss" "misses")))))

(defun announce-evade (attacker target attack)
  (dolist (observer (contents (location target)))
    (when (has-session-p observer)
      (show-text observer
                 "~a, but ~a ~a aside!"
                 (attack-message observer attacker target attack)
                 (if (eq observer target) "you" (describe-brief target :article :definite))
                 (if (eq observer target) "leap" "leaps")))))

(defun announce-resist (attacker target attack)
  (dolist (observer (contents (location target)))
    (when (has-session-p observer)
      (show-text observer
                 "~a, but the attack has no effect!"
                 (attack-message observer attacker target attack)))))

(defun announce-hit (attacker target attack amount)
  (dolist (observer (contents (location target)))
    (when (has-session-p observer)
      (show-text observer
                 "~a for ~d damage!"
                 (attack-message observer attacker target attack)
                 amount))))

;;;

(defun exit-combat-with-target (attacker target)
  (remove-opponent attacker target)
  (format-log :info "exit-combat ~a ~a ~a" attacker target (opponents attacker))
  (if (opponents attacker)
      (progn
        (setf (attack-target attacker) (select-target attacker))
        (change-behavior-state attacker :activity :select-attack))
      (stop-behavior attacker :activity)))

;;; When a monster dies, its loot is generated and distributed among live
;;; avatars in the same location who ended with at least X% of maximum threat.
;;; Loot that cannot be distributed is dropped on the ground. When an avatar
;;; dies, it becomes a corpse and cannot do anything except wait for a
;;; resurrection or use the `recall` command.

(defevent die (actor))

(defmethod do-die ((actor combatant))
  (setf (attack-target actor) nil
        (assist-target actor) nil
        (opponents actor) nil)
  (stop-all-behaviors actor)
  (notify-observers (location actor) #'did-die actor))

(defmethod do-die ((actor monster))
  ;; TODO: generate and distribute loot.
  (let ((location (location actor)))
    (call-next-method)
    (award-xp actor)
    (award-loot actor)
    (exit-world actor location nil)
    (respawn actor location)))

(defmethod do-die ((actor avatar))
  (call-next-method)
  (show-notice actor "You are dead. You can wait for an ally to revive you, or
  use the `recall` command to reincarnate at your recall point."))

(defmethod did-die ((observer avatar) actor)
  (if (eq observer actor)
      (show-text observer "You die.")
      (show-text observer "~a dies." (describe-brief actor :capitalize t))))

;;; Several events are associated with combat. The `kill` event occurs when
;;; `actor` kills `victim`. This can be via direct damage or condition damage,
;;; e.g. damage over time associated with an aura. Note that `did-kill`
;;; notifications are passed the corpse of the victim, not the victim itself.

(defevent kill (actor victim))

(defmethod do-kill :around (actor victim)
  (notify-observers (location actor) #'will-kill actor victim)
  (call-next-method)
  (notify-observers (location actor) #'did-kill actor victim)
  (die victim))

(defmethod do-kill (attacker victim)
  (exit-combat-with-target attacker victim))

(defmethod will-kill ((observer avatar) actor victim)
  (show-text observer "~a killed ~a!"
             (if (eq observer actor) "You" (describe-brief actor :capitalize t))
             (describe-brief victim)))

;;; The `inflict-damage` event occurs when `actor` causes `amount` damage to
;;; `target` due to `attack`.

(defevent inflict-damage (actor target attack amount))

(defmethod do-inflict-damage (actor target attack amount)
  (announce-hit actor target attack amount)
  (setf (health target) (max 0 (- (health target) amount)))
  (when (deadp target)
    (kill actor target)))

(defmethod do-inflict-damage :after (actor target attack amount)
  (when (location target)
    (notify-observers (location target) #'did-inflict-damage actor target attack amount)))

(defmethod do-inflict-damage :after (actor (target avatar) attack amount)
  (update-avatar target :health (health target)))

(defmethod did-inflict-damage ((observer avatar) actor target attack amount)
  (when (not (eq target observer))
    (update-neighbor observer target :health (health target))))

;;; The `attack` event occurs when `actor` uses a harmful `attack` against
;;; `target`.

(defun attackable-p (actor target)
  "Returns t if `actor` can attack `target`."
  (and (not (eq actor target))
       (typep target 'combatant)
       (not (deadp target))
       (eq (location target) (location actor))
       (or (eq (attitude actor) :neutral)
           (not (eq (attitude actor) (attitude target))))))

(defevent attack (actor target attack))

(defun resolve-attack (attacker target attack &key (missable t) (evadable t))
  ;; TODO: critical damage.
  (cond
    ;; Check for a miss.
    ((< (random 1.0) (if missable
                         (miss-chance (get-modifier :accuracy attacker))
                         0))
     (announce-miss attacker target attack))
    ;; Check for a dodge.
    ((< (random 1.0) (if evadable
                         (evade-chance (get-modifier :evasion target))
                         0))
     (announce-evade attacker target attack))
    ;; Handle a hit.
    (t
     (let* ((amount (compute-damage attacker target attack)))
       (if (= amount 0)
           ;; A hit for zero damage is a resist.
           (announce-resist attacker target attack)
           ;; Apply damage to the target.
           (inflict-damage attacker target attack amount))))))

(defmethod do-attack :around (actor target attack)
  (when (query-observers (location actor) #'can-attack actor target attack)
    (notify-observers (location actor) #'will-attack actor target attack)
    (call-next-method)
    (notify-observers (location actor) #'did-attack actor target attack)))

(defmethod do-attack :before (actor target attack)
  (notify-observers (location actor) #'will-attack actor target attack))

(defmethod do-attack :after (actor target attack)
  (notify-observers (location actor) #'did-attack actor target attack))

(defmethod do-attack (actor target attack)
  (resolve-attack actor target attack))

;;; The combat behavior is running whenever a combatant is in combat.

(defbehavior combat (actor)
    (next-attack)

  ;; Select the next attack to execute against the current target. If there is
  ;; no target or no attack, re-enter this state after a short delay.
  (:select-attack
   (let ((target (attack-target actor)))
     (setf next-attack (and target (select-attack actor target)))
     (if next-attack
         (change-state :attack (attack-delay next-attack))
         (change-state :select-attack 5))))

  ;; Attacks the current target with the previously-selected attack, then
  ;; selects the next attack.
  (:attack
   (when-let ((target (attack-target actor)))
     (attack actor target next-attack))
   (change-state :select-attack))

  ;; Exit combat.
  (:stop
   (setf (attack-target actor) nil (opponents actor) nil)
   (show-text actor "You are no longer in combat.")))

;;;

(defmethod do-attack :after (actor (target monster) attack)
  (when (and (not (deadp target))
             (not (in-combat-p target)))
    (add-opponent target actor)
    (setf (attack-target target) actor)
    (start-activity target #'combat)))

;;; The `attack` command sets the default target for subsequent attacks and
;;; begins autoattacking the target.

(defcommand (actor ("attack" "at") target)
  "Begin attacking an enemy and make it your default target for special attacks.
  You will enter combat with the enemy and its allies. See `help:combat` for
  details."
  (let ((matches (match-objects target
                                (remove-if-not #'(lambda (x) (attackable-p actor x))
                                               (contents (location actor))))))
    (case (length matches)
      (0 (show-text actor "You don't see anything like that to attack."))
      (1
       (let ((target (first matches)))
         (if (eq target (attack-target actor))
             (show-text actor "You are already attacking ~a." (describe-brief target))
             (progn
               (show-text actor "You begin attacking ~a." (describe-brief target))
               (add-opponent actor target)
               (let ((prev-target (attack-target actor)))
                 (setf (attack-target actor) target)
                 (if prev-target
                     (change-behavior-state actor :activity :select-attack)
                     (start-behavior actor :activity #'combat)))))))
      (t (show-text actor "Do you want to attack ~a?"
                    (format-list (mapcar #'describe-brief matches) :conjunction "or"))))))

;;; The `assist` event occurs when `actor` uses a helpful `effect` on `target`.

(defun assistable-p (actor target)
  (and (not (eq actor target))
       (typep target 'combatant)
       (or (eq (attitude actor) :neutral)
           (eq (attitude actor) (attitude target)))))

(defevent assist (actor target effect))

(defmethod do-assist :around (actor target effect)
  (when (query-observers (location actor) #'can-assist actor target effect)
    (notify-observers (location actor) #'will-assist actor target effect)
    (call-next-method)
    (notify-observers (location actor) #'did-assist actor target effect)))

(defmethod do-assist (actor target effect)
  ;; TODO:
  nil)

;;; The `assist` command sets the default target for subsequent helpful actions.

(defun set-assist-target (actor target)
  (setf (assist-target actor) target)
  (show-text actor "You begin assisting ~a." (describe-brief target)))

(defcommand (actor ("assist" "as") target)
  "Begin assisting an ally, making it your default target for subsequent helpful
  actions. You will enter combat with any enemies currently attacking your ally.
  See `help:combat` for details."
  (let ((matches (match-objects target
                                (keep-if (curry #'assistable-p actor) (contents (location actor))))))
    (case (length matches)
      (0 (show-text actor "You don't see anyone matching \"~{~a~^ ~}\" that you can assist." target))
      (1 (set-assist-target actor (first matches)))
      (t (show-text actor "Do you want to assist ~a?"
                    (format-list (mapcar #'describe-brief matches) :conjunction "or"))))))

;;;

(defproto recall-portal (portal))

(defmethod describe-exit (observer actor location (exit recall-portal))
  (format nil "~a disappears in a puff of white smoke."
          (describe-brief actor :capitalize t)))

(defmethod describe-entry (observer actor location (entry recall-portal))
  (format nil "~a appears in a puff of white smoke."
          (describe-brief actor :capitalize t)))

(defbehavior recall (actor location)
    ()
  (:start
   (show-text actor "You search for a way back to the realm of the living...")
   (start-casting actor 10)
   (change-state :finish 10))
  (:finish
   (show-text actor "Ah! You've done it!")
   (finish)
   (traverse-portal actor (make-instance 'recall-portal :destination location)))
  (:stop
   (show-text actor "Your recall attempt has been interrupted.")
   (stop-casting actor)))

(defcommand (actor "recall")
  "If you die, you can use this command to return to the location where you last
  bound your soul to a lifestone."
  (setf (health actor) 1)
  (start-behavior actor :regenerate #'regenerate)
  (let ((dest (or (recall-location actor)
                  (type-of *new-avatar-location*))))
    (start-activity actor #'recall dest)))

;;; FIXME: for testing

(defcommand (actor "opp" subject)
  (let ((matches (if subject
                     (match-objects subject (remove-if-not #'(lambda (x) (typep x 'combatant))
                                                           (contents (location actor))))
                     (list actor))))
    (dolist (entity matches)
      (with-slots (attack-target assist-target opponents) entity
      (show-text actor "~a (#~d): attack-target = ~a, assist-target = ~a, opponents = ~a"
                 (describe-brief entity)
                 (id entity)
                 (and attack-target (describe-brief attack-target))
                 (and assist-target (describe-brief assist-target))
                 (mapcar #'describe-brief opponents))))))

(defcommand (actor "ooc")
  (setf (attack-target actor) nil
        (assist-target actor) nil
        (opponents actor) nil)
  (show-text actor "Targets and opponents have been cleared."))
