(in-package :charm)

;;; Each skill is represented by an instance of class `skill`.

(defclass skill ()
  ((key
    :initarg :key :reader key
    :documentation "Symbol whose value is the skill instance.")
   (name
    :initarg :name :reader name
    :documentation "The name of the skill.")
   (summary
    :initarg :summary :reader summary
    :documentation "A brief description of what the skill does.")
   (level
    :initform 0 :initarg :level :reader level
    :documentation "The minimum level required to learn the skill.")
   (required-race
    :initform nil :initarg :required-race :reader required-race
    :documentation "A symbol whose value is the race required to learn the
      skill, if any.")
   (required-quests
    :initform nil :initarg :required-quests :reader required-quests
    :documentation "A list of symbols denoting quests that must have been
      completed before learning the skill.")
   (required-skills
    :initform nil :initarg :required-skills :reader required-skills
    :documentation "A plist of where each key denotes a skill and each value is
      the minimum rank that must be achieved in that skill before learning this
      skill.")
   (exclusive-skills
    :initform nil :initarg :exclusive-skills :reader exclusive-skills
    :documentation "A list of symbols denoting skills that, if known, prevent
      learning this skill.")
   (cost
    :initform nil :initarg :cost :reader cost
    :documentation "A plist where each key is either :karma or a symbol denoting
      a currency type, and each value is the amount of that resource needed to
      learn the skill.")
   (max-rank
    :initform 0 :initarg :max-rank :reader max-rank
    :documentation "If positive, each time the skill is used it may increase the
      avatar's rank in the skill up to this limit.")
   (modifiers
    :initform nil :initarg :modifiers :reader modifiers
    :documentation "Modifiers conferred upon the avatar when this skill is
      known.")
   (abilities
    :initform nil :initarg :abilities :reader abilities
    :documentation "Abilities enabled when this skill is known. This is a list
      of lists. Each sublist can be one of (:command symbol &optional
      rank), (:spell symbol &optional rank), or (:recipe item materials-plist
      &optional rank).")))

;;; The `defskill` macro creates an instance of class `skill`.

(defmacro defskill (name &body slots)
  (let ((args (loop for (name init-form) in slots
                    append `(,(make-keyword name) ,init-form))))
    `(progn
       (defparameter ,name (make-instance 'skill :key ',name ,@args))
       (export ',name))))

(defun find-skill (key)
  (symbol-value key))

(defmethod match-tokens (tokens (target skill))
  (match-tokens tokens (name target)))

;;; An ability is something an avatar becomes able do upon learning a skill and
;;; optionally achieving a required rank. Abilities fall into three categories:
;;; actions (standalone commands), spells (for use with the `cast` command), and
;;; recipes (for use with the `craft` command).

(defclass ability ()
  ((skill
    :initarg :skill :reader skill
    :documentation "The key of the skill required in order to use the ability.")
   (rank
    :initform nil :initarg :rank :reader rank
    :documentation "If non-nil, the minimum rank required in order to use the
      ability.")))

(defgeneric command (ability)
  (:documentation "Returns the command object associated with `ability`."))

(defclass action (ability)
  ((command
    :initarg :command :reader command
    :documentation "The command enabled by the ability.")))

;;; TODO: move this

(defclass spell (ability)
  ())

(defmethod command ((ability spell))
  (find-command "cast"))
