(in-package :charm)

;;;; The `armor` attribute on equipment is treated separated from other
;;;; attributes: it typically appears only on certain slots and has its own
;;;; budget which depends on the armor group, one of :clothing, :light-armor,
;;;; :medium-armor, or :heavy-armor.

(defparameter *armor-slots*
  (plist-hash-table
   '(:head 4
     :torso 5
     :hands 3
     :legs 5
     :feet 3))
  "Base amount of armor granted per level, by slot, for items with an
  armor-value of one.")

(defproto armor (equipment)
  (armor-type "armor")
  (armor-value 0))

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

(defmethod describe-full ((subject armor))
  ;; FIXME: show quality if > 0
  (format nil
          "~a (level ~d ~a; armor ~d~{; ~(~a~) ~d~})"
          (call-next-method)
          (level subject)
          (armor-type subject)
          (base-armor-attribute subject)
          (equipment-modifiers-plist subject)))
