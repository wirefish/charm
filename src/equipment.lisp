(in-package :charm)

;;;

(defproto equipment (item)
  (level 0)
  (slot nil)
  (proficiency nil)
  (mastery nil)
  (modifiers nil) ; plist
  (quality 0 :instance)
  (affix nil :instance) ; enchant, etc.
  (inscription nil :instance)) ; personalization text

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
     :off-hand 2/20 ; shields, lamps, ...
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

;;; The `quality` of an piece of equipment is an integer that adds to the item's
;;; effective level when computing its attributes, but does not increase the
;;; level required to use the item. Each quality modifier greater than zero has
;;; a name that appears when describing the item.

(defparameter *quality-names*
  (plist-hash-table
   (list 1 (parse-noun "a fine")
         2 (parse-noun "a masterwork")
         3 (parse-noun "an exceptional")
         4 (parse-noun "an exquisite")
         5 (parse-noun "a legendary"))))

(defmethod describe-brief ((subject equipment) &rest args)
  (if-let ((quality-name (gethash (quality subject) *quality-names*)))
    (format nil "~a ~a"
            (apply #'format-noun quality-name args)
            (call-next-method subject :article nil))
    (call-next-method)))
