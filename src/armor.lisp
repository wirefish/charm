(in-package :charm)

;;;; The `armor` attribute on equipment is treated separated from other
;;;; attributes: it typically appears only on certain slots and has its own
;;;; budget which depends on the armor group, one of :clothing, :light-armor,
;;;; :medium-armor, or :heavy-armor.

(defparameter *armor-slots*
  (plist-hash-table
   '(:head 2
     :torso 4
     :hands 2
     :back 1
     :waist 1
     :legs 3
     :feet 2))
  "Base amount of armor granted per level, by slot, for items with an
  armor-value of one.")

(defproto armor (equipment)
  (armor-value 1))

(defgeneric base-armor-attribute (item)
  (:documentation "Returns the base armor of a piece of equipment.")
  (:method (item)
    0))

(defmethod base-armor-attribute ((item armor))
  (with-slots (level quality slot armor-value) item
    (floor (* (+ level quality)
              (gethash slot *armor-slots* 0)
              armor-value))))

(defmethod get-modifier (modifier (entity armor))
  (+ (call-next-method)
     (if (eq modifier :armor) (base-armor-attribute entity) 0)))

;; TODO: Does heavy armor (i.e. high armor-value) increase cast times?
