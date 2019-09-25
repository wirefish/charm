(in-package :charm)

;;; Definitions of all damage types.

(defparameter *damage-types*
  (plist-hash-table
   '(:crushing :physical
     :piercing :physical
     :slashing :physical
     ;;
     :fire :elemental
     :cold :elemental
     :acid :elemental
     :electricity :elemental
     ;;
     :arcane :magical
     :shadow :magical)))

(defun damage-group (damage-type)
  (gethash damage-type *damage-types*))

;;; Return the unadjusted amount damage done by an attack.

(defgeneric damage-amount (attack))

(defmethod damage-amount ((attack weapon))
  (with-slots (level base-damage) attack
    (uniform-random (* (1+ level) (first base-damage))
                    (* (1+ level) (second base-damage)))))

;;; Functions that determine the actual effect of combat modifiers when applied
;;; to an attack.

(defun relative-level-ratio (attacker-level target-level)
  (- 1
     (/ (- target-level attacker-level)
        (max 1 (+ target-level attacker-level)))))

(defun opposed-modifier-ratio (attack-modifier defend-modifier)
  (- 1
     (/ (- defend-modifier attack-modifier)
        (max 1 (+ defend-modifier attack-modifier)))))

(defun compute-damage (attacker target attack)
  "Returns the base damage done when `attacker` attacks `target` using
  `attack`."
  (let ((group (damage-group (damage-type attack))))
    (round-random
     (max 0
          (- (* (damage-amount attack)
                (relative-level-ratio (level attacker) (level target))
                (opposed-modifier-ratio
                 (case group
                   (:physical (get-modifier :strength attacker))
                   ((:elemental :magical) (get-modifier :intellect attacker))
                   (t 1))
                 (case group
                   ((:physical :elemental) (get-modifier :armor target))
                   (:magical (get-modifier :willpower target))
                   (t 1))))
             (get-modifier (damage-type attack) target))))))

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

(defun reduce-health (target amount)
  (setf (health target)
        (max 0 (- (health target) amount))))

(defun deadp (target)
  (= (health target) 0))

;;; Functions that announce combat events to observers.

(defun announce-miss (attacker target attack)
  (dolist (observer (contents (location attacker)))
    (when (has-session-p observer)
      (show-text observer
                 "~a ~a ~a with ~a and ~a!"
                 (if (eq observer attacker) "You" (describe-brief attacker :capitalize t))
                 (if (eq observer attacker)
                     (verb-plural (attack-verb attack))
                     (verb-singular (attack-verb attack)))
                 (if (eq observer target) "you" (describe-brief target))
                 (describe-brief attack)
                 (if (eq observer target) "miss" "misses")))))

(defun announce-evade (attacker target attack)
  (dolist (observer (contents (location attacker)))
    (when (has-session-p observer)
      (show-text observer
                 "~a ~a ~a with ~a, but ~a ~a aside!"
                 (if (eq observer attacker) "You" (describe-brief attacker :capitalize t))
                 (if (eq observer attacker)
                     (verb-plural (attack-verb attack))
                     (verb-singular (attack-verb attack)))
                 (if (eq observer target) "you" (describe-brief target))
                 (describe-brief attack)
                 (if (eq observer target) "you" (describe-brief target :article :definite))
                 (if (eq observer target) "leap" "leaps")))))

(defun announce-resist (attacker target attack)
  (dolist (observer (contents (location attacker)))
    (when (has-session-p observer)
      (show-text observer
                 "~a ~a ~a with ~a, but the attack has no effect!"
                 (if (eq observer attacker) "You" (describe-brief attacker :capitalize t))
                 (if (eq observer attacker)
                     (verb-plural (attack-verb attack))
                     (verb-singular (attack-verb attack)))
                 (if (eq observer target) "you" (describe-brief target))
                 (describe-brief attack)))))

(defun announce-hit (attacker target attack amount)
  (dolist (observer (contents (location attacker)))
    (when (has-session-p observer)
      (show-text observer
                 "~a ~a ~a with ~a for ~d damage!"
                 (if (eq observer attacker) "You" (describe-brief attacker :capitalize t))
                 (if (eq observer attacker)
                     (verb-plural (attack-verb attack))
                     (verb-singular (attack-verb attack)))
                 (if (eq observer target) "you" (describe-brief target))
                 (describe-brief attack)
                 amount))))

;;; Called to allow `attacker` to select the next attack to use against `target`.

(defgeneric select-attack (attacker target))

(defproto fist (weapon)
  (brief "a fist")
  (damage-type :crushing)
  (base-damage (1 4))
  (attack-verb "bashes")
  (attack-delay 3))

(defparameter *default-attack* (make-instance 'fist))

(defmethod select-attack ((attacker combatant) target)
  (or (first (attacks attacker))
      *default-attack*))

(defmethod select-attack ((attacker avatar) target)
  (or (gethash :main-hand (equipment attacker))
      *default-attack*))

;;; Several events are associated with combat. The `kill` event occurs when
;;; `actor` kills `victim`. This can be via direct damage or condition damage,
;;; e.g. damage over time associated with an aura. Note that `did-kill`
;;; notifications are passed the corpse of the victim, not the victim itself.

(defevent kill (actor victim))

(defmethod do-kill :around (actor victim)
  (notify-observers (location actor) #'will-kill actor victim)
  (call-next-method)
  (let ((corpse (make-corpse victim)))
    (replace-in-contents (location actor) victim corpse)
    (notify-observers (location actor) #'did-kill actor corpse)))

(defmethod do-kill (actor victim)
  (when (eq (attack-target actor) victim)
    ;; TODO: Select another opponent if possible.
    (setf (attack-target actor) nil))
  (setf (attack-target victim) nil)
  (setf (assist-target victim) nil)
  (setf (opponents victim) nil))

(defmethod do-kill ((actor avatar) victim)
  (call-next-method)
  (gain-xp actor (* 20 (1+ (level victim)))))

(defmethod will-kill ((observer avatar) actor victim)
  (show-text observer "~a killed ~a!"
             (if (eq observer actor) "You" (describe-brief actor :capitalize t))
             (describe-brief victim)))

(defmethod did-kill ((observer avatar) actor corpse)
  (update-neighbor observer corpse))

;;; The `attack` event occurs when `actor` uses a harmful `attack` against
;;; `target`.

(defun attackable-p (actor target)
  "Returns t if `actor` can attack `target`."
  (and (not (eq actor target))
       (typep target 'combatant)
       (not (deadp target))
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
           (let ((amount (round-random amount)))
             (announce-hit attacker target attack amount)
             (reduce-health target amount)
             (when (deadp target)
               (kill attacker target))))))))

(defmethod do-attack :around (actor target attack)
  (when (query-observers (location actor) #'can-attack actor target attack)
    (notify-observers (location actor) #'will-attack actor target attack)
    (call-next-method)
    (notify-observers (location actor) #'did-attack actor target attack)))

(defmethod do-attack (actor target attack)
  (resolve-attack actor target attack))

;;; The `attack` command sets the default target for subsequent attacks and
;;; begins autoattacking the target.

(defun autoattack (actor)
  (when-let ((target (attack-target actor)))
    (attack actor target (select-attack actor target))
    (setf (attack-timer actor) (as:delay #'(lambda () (autoattack actor)) :time 3))))

(defun begin-attacking (actor target)
  "Called when `actor` switches its attack-target to `target`."
  (if (eq target (attack-target actor))
      (show-text actor "You are already attacking ~a." (describe-brief target))
      (progn
        (setf (attack-target actor) target)
        (setf (opponents actor) (adjoin target (opponents actor)))
        (when (and (attack-timer actor) (not (as:event-freed-p (attack-timer actor))))
          (as:free-event (attack-timer actor)))
        (setf (attack-timer actor) (as:delay #'(lambda () (autoattack actor)) :time 3))
        (show-text actor "You begin attacking ~a." (describe-brief target)))))

(defcommand (actor ("attack" "at") target)
  "Begin attacking an enemy and make it your default target for special attacks.
  You will enter combat with the enemy and its allies. See `help:combat` for
  details."
  (let ((matches (match-objects actor target
                                (keep-if #'(lambda (x) (when (attackable-p actor x) x))
                                         (contents (location actor))))))
    (case (length matches)
      (0 (show-text actor "You don't see anything like that to attack."))
      (1 (begin-attacking actor (first matches)))
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
  (let ((matches (match-objects actor target
                                (keep-if (curry #'assistable-p actor) (contents (location actor))))))
    (case (length matches)
      (0 (show-text actor "You don't see anyone matching \"~{~a~^ ~}\" that you can assist." target))
      (1 (set-assist-target actor (first matches)))
      (t (show-text actor "Do you want to assist ~a?"
                    (format-list (mapcar #'describe-brief matches) :conjunction "or"))))))
