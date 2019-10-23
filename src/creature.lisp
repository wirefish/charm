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
  ;; Update threat.
  (let ((entry (assoc actor (threat target))))
    (if entry
        (incf (cdr entry) amount)
        (push (cons actor amount) (threat target)))))

;;; A loot table is a list of (probability . items), where each item is either a
;;; type or (min max type).

(defun create-loot (loot-table)
  (do (loot)
      ((null loot-table) (nreverse loot))
    (destructuring-bind (probability . items) (pop loot-table)
      (when (< (random 1.0) probability)
        (let ((item (random-elt items)))
          (if (listp item)
              (destructuring-bind (min max type) item
                (push (make-instance type :stack-count (uniform-random min max))
                      loot))
              (push (make-instance item) loot)))))))

(defun award-xp (monster)
  (when-let ((targets (remove-if-not #'(lambda (x)
                                         (let ((opponent (car x)))
                                           (and (typep opponent 'avatar)
                                                (not (deadp opponent))
                                                (same-location-p monster opponent))))
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
