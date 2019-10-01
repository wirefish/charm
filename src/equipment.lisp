(in-package :charm)

;;;

(defproto equipment (item)
  (level 0)
  (slot nil)
  (proficiency nil)
  (mastery nil)
  (modifiers nil)
  (quality 0 :instance)
  (affixes nil :instance)
  (inscription nil :instance))

(defmethod get-modifier (modifier (entity equipment))
  (getf (modifiers entity) modifier 0))

;;; Each piece of equipment uses `slot` to describe where it can be equipped by
;;; an avatar. (This slot *does not* map one-to-one to the avatar's equipment
;;; slots.) The equipment's slot determines a weight that scales its attribute
;;; budget. NOTE: The weights sum to one after accounting for (1) :both-hands
;;; being exclusive of :main-hand and :off-hand, (2) :main-hand can apply twice
;;; if dual-wielding, but is then exclusive of :off-hand; and (3) :finger can
;;; apply twice for :left-finger and :right-finger.

(defparameter *equipment-slot-weights*
  (plist-hash-table
   '(;; weapons
     :both-hands 4/20 ; two-handed weapons
     :main-hand 2/20 ; light and one-handed weapons
     :off-hand 2/20 ; shields
     ;; armor
     :head 2/20
     :torso 2/20
     :back 1/20
     :hands 1/20
     :waist 1/20
     :legs 2/20
     :feet 1/20
     ;; accessories
     :ears 1/20
     :neck 1/20
     :wrists 1/20
     :finger 1/20 ; applies twice
     ;; containers
     :backpack 1/20
     :in-hands 0)))

(defparameter *attribute-budget-per-level* 20
  "The total attribute budget across all slots per item level. For example, an
  avatar fully-equipped in level 50 gear would have a total item budget of 50
  times this value.")

(defun equipment-attribute-budget (slot level)
  (* level
     *attribute-budget-per-level*
     (gethash slot *equipment-slot-weights*)))

(defparameter *equipment-slots*
  (mapcar #'(lambda (spec)
              (destructuring-bind (key pose name weight) spec
                (cons key (make-equipment-slot :pose pose :name name :weight weight))))
          '(;; weapons
            (:main-hand "in your" "main hand" 5/4)
            (:off-hand "in your" "off-hand" 5/4)
            ;; armor
            (:head "on your" "head" 1)
            (:torso "on your" "body" 1)
            (:back "across your" "shoulders" 1/2)
            (:hands "on your" "hands" 1/2)
            (:waist "around your" "waist" 1/2)
            (:legs "on your" "legs" 1)
            (:feet "on your" "feet" 1/2)
            ;; accessories
            (:ears "on your" "ears" 1/2)
            (:neck "around your" "neck" 1/2)
            (:wrists "on your" "wrists" 1/2)
            (:left-finger "on your" "left finger" 1/2)
            (:right-finger "on your" "right finger" 1/2)
            ;; containers
            (:backpack "on your" "back" 0)
            (:sack "over your" "shoulder" 0)
            (:in-hands "in your" "hands" 0))))
