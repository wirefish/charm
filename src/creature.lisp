(in-package :charm)

;;; FIXME: reorganize this

(defproto creature (entity)
  (icon 'head-silhouette)
  (attitude :neutral)
  (begins-quests nil)
  (ends-quests nil))

;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod transform-slot-init-form (def-name (slot-name (eql 'attacks)) init-form)
    `(list ,@(mapcar #'(lambda (class) `(make-instance ',class)) init-form))))

(defproto monster (combatant creature)
  (attacks nil)
  (loot nil)
  (threat nil :instance))

(defmethod select-attack ((attacker monster) target)
  ;; FIXME: select an attack based on probabilities and cooldowns
  (first (attacks attacker)))

;;;

(defmethod do-inflict-damage :after (actor (target monster) attack amount)
  ;; Update threat, which is a mapping from opponent id to total damage done by
  ;; that opponent.
  (let ((entry (assoc (id actor) (threat target))))
    (if entry
        (incf (cdr entry) amount)
        (push (cons (id actor) amount) (threat target)))))

(defun generate-loot (loot-table)
  "Generates random loot from `loot-table`, which is a list of (probability .
  items). Each item in items is either a type, or a list (min max type)."
  (do (loot)
      ((null loot-table) (nreverse loot))
    (destructuring-bind (probability . items) (pop loot-table)
      (when (< (random 1.0) probability)
        (let ((item (random-elt items)))
          (if (listp item)
              (destructuring-bind (min max type) item
                (push (make-instance type :stack-size (uniform-random min max))
                      loot))
              (push (make-instance item) loot)))))))

(defun award-loot (monster)
  (when-let ((targets (keep-if
                       #'(lambda (x)
                           (when-let ((opponent (find-id-in-container (car x)
                                                                      (location monster))))
                             (when (and (typep opponent 'avatar)
                                        (not (deadp opponent)))
                               opponent)))
                       (threat monster))))
    (do ((loot (generate-loot (loot monster))))
        ((not loot))
      (give-item monster (pop loot) (random-elt targets)))))

(defmethod format-give-item ((actor monster) item)
  (format nil "You receive ~a." (describe-brief item)))

(defun award-xp (monster)
  "Award experience for killing a monster. Every avatar that caused threat
  (e.g. by damaging the monster or healing one of its opponents) and is alive
  and at the monster's location receives a share of experience in proportion to
  the threat they caused."
  (when-let ((targets (keep-if
                       #'(lambda (x)
                           (when-let ((opponent (find-id-in-container (car x)
                                                                      (location monster))))
                             (when (and (typep opponent 'avatar)
                                        (not (deadp opponent)))
                               (cons opponent (cdr x)))))
                       (threat monster))))
    (let ((total-threat (apply #'+ (mapcar #'cdr targets)))
          (total-xp (xp-value monster)))
      (loop for (opponent . threat) in targets do
           (let ((xp (round (* total-xp (/ threat total-threat)))))
             (when (> xp 0)
               (gain-xp opponent xp)))))))

;;;

(defproto npc (creature)
  (attitude :friendly))

(defproto vendor (npc)
  (sells nil))

(defproto trainer (npc)
  (teaches nil))
