(in-package :charm)

;;;; An aura is a temporary effect placed on an entity.

(defproto aura ()
  (name nil)
  (full nil)
  (icon nil)
  (modifiers nil) ; plist
  (duration nil)
  (tick-interval nil)
  (owner-id nil :instance)
  (behaviors nil :instance))

(defmethod get-modifier (modifier (subject aura))
  (getf (modifiers subject) modifier 0))

(defmethod describe-brief ((subject aura) &rest args)
  (if (getf args :capitalize)
      (string-capitalize (name subject))
      (name subject)))

(defmethod describe-full ((subject aura))
  (full subject))

(defmethod describe-icon ((subject aura))
  (icon subject))

;;;

(defgeneric setup-aura (actor aura)
  (:method (actor aura)
    (setf (owner-id aura) (id actor))))

;;;

(defun show-aura (avatar aura)
  (when-let ((session (session avatar)))
    (send-client-command session "showAura" (type-of aura) (icon aura))))

(defun hide-aura (avatar aura)
  (when-let ((session (session avatar)))
    (send-client-command session "hideAura" (type-of aura))))

;;; The `apply-aura` event occurs when `actor` causes a new instance of type
;;; `aura` to be applied to `target`.

(defevent apply-aura (actor aura target)
  (let ((aura (make-instance aura)))
    (setup-aura actor aura)
    (do-apply-aura actor aura target)))

;;; The `remove-aura` event occurs when `actor` causes `aura` to be removed from
;;; target. When `aura` naturally expires, `actor` is nil.

(defevent remove-aura (actor aura target))

;;; The `tick-aura` event occurs when `aura` is applied to `target` and defines
;;; a non-nil `tick-interval`.

(defevent tick-aura (aura target))

;;; An active aura has a behavior that allows it to periodically tick and
;;; automatically remove itself when it expires.

(defbehavior aura-behavior (aura target)
    ((time-remaining (duration aura)))
  (:start
   (if (tick-interval aura)
       (change-state :tick (tick-interval aura))
       (change-state :expire time-remaining)))
  (:tick
   (tick-aura aura target)
   (decf time-remaining (tick-interval aura))
   (if (>= time-remaining (tick-interval aura))
       (change-state :tick (tick-interval aura))
       (change-state :expire time-remaining)))
  (:expire
   (remove-aura nil aura target))
  (:stop))

(defmethod do-apply-aura (actor aura target)
  (with-slots (auras) target
    (when-let ((prev (find-if #'(lambda (x) (eq (type-of aura) (type-of x))) auras)))
      (stop-behavior prev :aura)
      (deletef auras prev))
    (push aura auras)
    (start-behavior aura :aura #'aura-behavior target)))

(defmethod do-apply-aura :after (actor aura target)
  (notify-observers (location target) #'did-apply-aura actor aura target))

(defmethod do-apply-aura :after (actor aura (target avatar))
  (show-text target "~a has applied ~a to you."
             (describe-brief actor :capitalize t)
             (describe-brief aura))
  (show-aura target aura))

(defmethod do-remove-aura (actor aura target)
  (deletef (auras target) aura)
  (stop-behavior aura :aura))

(defmethod do-remove-aura :after (actor aura target)
  (notify-observers (location target) #'did-remove-aura actor aura target))

(defmethod do-remove-aura :after (actor aura (target avatar))
  (if actor
      (show-text target "~a has removed ~a from you."
                 (describe-brief actor :capitalize t)
                 (describe-brief aura))
      (show-text target "~a has faded."
                 (describe-brief aura :capitalize t)))
  (hide-aura target aura))

;;;

(defproto damage-aura (aura)
  (damage-type nil)
  (attack-verb "hits")
  (total-damage nil))

(defmethod attack-damage ((attack damage-aura) attacker)
  (with-slots (duration tick-interval total-damage) attack
    (let ((tick-damage (* total-damage (/ tick-interval duration))))
      (cons (floor tick-damage)
            (ceiling tick-damage)))))

(defmethod attack-message ((observer avatar) attacker target (attack damage-aura))
  (format nil "~a ~a ~a"
          (describe-brief attack :capitalize t)
          (verb-singular (attack-verb attack))
          (if (eq observer target) "you" (describe-brief target)

(defmethod setup-aura (actor (aura damage-aura))
  ;; TODO: allow actor's modifiers to influence the aura damage?
  (call-next-method))

(defmethod do-tick-aura ((aura damage-aura) target)
  (resolve-attack aura target aura :missable nil :evadable nil))
