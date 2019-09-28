(in-package :charm)

#|
A quest represents a task set for the player by an inhabitant of the game world.

Four events occur during the lifetime of a quest:

- `offer-quest` occurs when the player interacts with an entity that has a quest
  for which the player is eligible.

- `accept-quest` occurs when the player accepts the most recently offered quest.
  It typically provides more details to the player and provides any needed items
  or auras.

- `advise-quest` occurs when the player interacts with an entity that begins a
  quest which the player has accepted but not yet completed. It might provide
  hints to the player to help complete the quest, refresh required auras, etc.

- `finish-quest` occurs when the player interacts with an entity that ends a
  quest for which the player has completed all objectives. It provides any
  rewards earned by completing the quest."
|#

;;; Each quest is represented as an instance of class `quest`.

(defclass quest ()
  ((key :initarg :key :reader key)
   (name :initform "An Unnamed Quest" :initarg :name :reader name)
   (summary :initform nil :initarg :summary :reader summary)
   (details :initform nil :initarg :details :reader details)
   (level :initform 0 :initarg :level :reader level)
   (required-quests :initform nil :initarg :required-quests :reader required-quests)))

(defmethod match-tokens (tokens (target quest))
  (match-tokens tokens (name target)))

;;; The `defquest` macro creates an instance of class `quest`.

(defmacro defquest (name &body slots)
  (let ((args (loop for (name init-form) in slots
                     append `(,(make-keyword name) ,init-form))))
    `(defparameter ,name (make-instance 'quest :key ',name ,@args))))

(defun find-quest (quest-key)
  (symbol-value quest-key))

;;; A `quest-item` is an item that exists only as part of completing a quest.

(defproto quest-item (item)
  (quest nil) ; key of associated quest
  (bound t))

(defmethod visible-p ((subject quest-item) (observer avatar))
  (quest-incomplete-p observer (find-quest (quest subject))))

;;;

(defun quest-xp-reward (quest)
  "Computes the amount of experience awarded upon finishing `quest`."
  (+ 200 (* (level quest) 100)))

(defun remove-quest-items (avatar quest &key npc (message "~a is destroyed."))
  "Removes all items associated with `quest` from the inventory of `avatar`. If
  `npc` is not nil, makes it appear that items are given to `npc`; otherwise,
  `message` is used to construct feedback to the player."
  (let ((items (remove-items-if avatar
                                #'(lambda (item)
                                    (and (typep item 'quest-item)
                                         (eq (quest item) (key quest)))))))
    (dolist (item items)
      (if npc
          (show-text avatar "You give ~a to ~a."
                     (describe-brief item)
                     (describe-brief npc :article :definite))
          (show-text avatar message
                     (describe-brief item :capitalize t :article :definite))))))

(defun remove-active-quest (actor quest)
  "Removes `quest` from the set of active quests for `actor`."
  (with-slots (key) quest
    (setf (active-quests actor)
          (delete-if #'(lambda (q) (eq (car q) key))
                     (active-quests actor)))))

;;; An event that triggers when `actor` is offered `quest` by interacting with
;;; `npc`.

(defevent offer-quest (actor quest npc))

(defmethod do-offer-quest :around (avatar quest npc)
  (notify-observers (location avatar) #'will-offer-quest avatar quest npc)
  (setf (pending-offer avatar) (list quest npc))
  (call-next-method)
  (show-notice avatar "~a has offered you the level ~d quest ~s. Type `accept` to accept it."
               (describe-brief npc :capitalize t :article :definite)
               (level quest)
               (name quest))
  (notify-observers (location avatar) #'did-offer-quest avatar quest npc))

;;; An event that triggers when `actor` begins `quest` by accepting it.

(defevent accept-quest (actor quest npc))

(defmethod do-accept-quest :around (avatar quest npc)
  (push (cons (key quest) 0) (active-quests avatar))
  (show-notice avatar "You have accepted the quest ~s." (name quest))
  (call-next-method))

;;; An event that triggers when `actor` talks to `npc` while `quest` is
;;; incomplete.

(defevent advise-quest (actor quest npc))

;;; An event that triggers when `actor` finishes `quest` by interacting with
;;; `npc`.

(defevent finish-quest (actor quest npc))

(defmethod do-finish-quest :around (actor quest npc)
  (remove-active-quest actor quest)
  (remove-quest-items actor quest :npc npc)
  (call-next-method)
  (show-notice actor "You have finished the quest ~s." (name quest))
  (setf (gethash (key quest) (finished-quests actor)) (get-universal-time))
  (gain-xp actor (quest-xp-reward quest)))

(defmethod do-finish-quest :before (actor quest npc)
  (notify-observers (location actor) #'will-finish-quest actor quest npc))

(defmethod do-finish-quest :after (actor quest npc)
  (notify-observers (location actor) #'did-finish-quest actor quest npc))

;;;

(defun quest-active-p (avatar quest)
  "Returns t if `avatar` has accepted but not yet finished (turned-in) `quest`."
  (assoc (key quest) (active-quests avatar)))

(defun quest-incomplete-p (avatar quest)
  "Returns t if `avatar` has accepted but not yet completed the objectives for
  `quest`."
  (let ((q (quest-active-p avatar quest)))
    (and q (< (cdr q) 1))))

(defun quest-complete-p (avatar quest)
  "Returns t if `avatar` has accepted and completed the objectives for `quest`
  but not yet turned it in."
  (let ((q (quest-active-p avatar quest)))
    (and q (>= (cdr q) 1))))

(defun quest-finished-p (avatar quest)
  "Returns the timestamp at which `avatar` last turned in `quest`, or nil if the
  quest has never been finished."
  (gethash (key quest) (finished-quests avatar)))

;;; The `quest-available-p` generic function is a predicate that returns t if
;;; `avatar` can currently accept `quest`.

(defgeneric quest-available-p (avatar quest))

(defmethod quest-available-p (avatar quest)
  (and (not (quest-active-p avatar quest))
       (not (quest-finished-p avatar quest)) ; TODO: allow repeatable quests?
       (>= (level avatar) (level quest))
       (every #'(lambda (x) (gethash x (finished-quests avatar))) (required-quests quest))))

(defun find-available-quest (avatar npc)
  (some #'(lambda (key)
            (let ((quest (find-quest key)))
              (when (quest-available-p avatar quest) quest)))
        (begins-quests npc)))

(defun find-incomplete-quest (avatar npc)
  (some #'(lambda (key)
            (let ((quest (find-quest key)))
              (when (quest-incomplete-p avatar quest) quest)))
        (begins-quests npc)))

(defun find-complete-quest (avatar npc)
  (some #'(lambda (key)
            (let ((quest (find-quest key)))
              (when (quest-complete-p avatar quest) quest)))
        (ends-quests npc)))

(defun advance-quest (avatar quest &optional (amount 1))
  (let ((state (quest-active-p avatar quest)))
    (when (and state (>= (incf (cdr state) amount) 1))
      (show-notice avatar "You have completed the objectives for the quest ~s!" (name quest))
      t)))

(defmethod accept-offer (avatar (quest quest) npc)
  (accept-quest avatar quest npc))

(defun summarize-quest-state (quest-state)
  (destructuring-bind (key . progress) quest-state
    (let ((quest (find-quest key)))
      (format nil "~a (level ~d, ~d% complete)~a"
              (name quest)
              (level quest)
              (floor (* progress 100))
              (if (>= progress 1) "&emsp;&#10004;" "")))))

(defun match-active-quests (avatar tokens)
  (loop for (key . progress) in (active-quests avatar)
        if (match-tokens tokens (find-quest key))
          collect (cons (find-quest key) progress)))

(defcommand (actor ("quest" "qu") :word subcommand :rest quest-name)
  "Display information about your active quests. This command has several
  subcommands:

  - `quest` or `quest list` will display a summary of your active quests.

  - `quest info *quest-name*` displays complete information for the named quest.

  - `quest drop *quest-name*` drops the named quest. You will lose all progress
    toward completion of the quest and any items associated with the quest."
  (cond
    ((or (null subcommand) (string= subcommand "list"))
     (with-slots (active-quests) actor
       (if (null active-quests)
           (show-text actor "You are not on any quests.")
           (show-text actor "You are currently on the following quests:~%~%~{- ~a~%~%~}"
                      (mapcar #'summarize-quest-state active-quests)))))
    ((string= subcommand "info")
     (if quest-name
         (let ((quests (match-active-quests actor quest-name)))
           (if (null quests)
               (show-text actor "You do not have any active quests matching that description.")
               (show-text actor "~{- ~a: ~a (level ~d, ~d% complete)~%~%~}"
                          (loop for (quest . progress) in quests
                                append (list (name quest) (summary quest) (level quest) progress)))))
         (show-text actor "For which quest do you want to see more information?")))
    ((string= subcommand "drop")
     (if quest-name
         (let ((quests (match-active-quests actor quest-name)))
           (case (length quests)
             (0 (show-text actor "You do not have any active quests matching that description."))
             (1
              (destructuring-bind (quest . progress) (first quests)
                (remove-active-quest actor quest)
                (remove-quest-items actor quest)
                (show-notice actor "You are no longer on the quest ~s." (name quest))))
             (t (show-text actor "Which quest do you want to drop: ~a?"
                           (format-list (mapcar #'car quests) :conjunction "or")))))
         (show-text actor "Which quest do you want to drop?")))
    (t
     (show-text actor "Unknown subcommand. Type `help quest` for help."))))
