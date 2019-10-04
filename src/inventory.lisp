;;;; Support for examining the contents of the player's inventory and moving
;;;; items within, into, or out of inventory.
;;;;
;;;; get: environment -> inventory
;;;; put: inventory -> inventory or environment
;;;; drop: inventory -> location (specialized put)
;;;; discard: inventory -> gone
;;;; equip: inventory -> equipment
;;;; unequip: equipment -> inventory
;;;; give: NPC -> inventory

(in-package :charm)

;;;

(defparameter *inventory-slot-order* '(:backpack :sack :in-hands))

(defun find-inventory-slot (actor item &key preferred-slot)
  ;; Find a slot that can contain the item.
  (let ((slots (if preferred-slot
                   (cons preferred-slot (remove preferred-slot *inventory-slot-order*))
                   *inventory-slot-order*)))
    (find-if #'(lambda (slot)
                 (let ((container (gethash slot (equipment actor))))
                   (and container
                        (typep container 'container)
                        (can-contain-item container item))))
             slots)))

(defun find-items-if (avatar pred)
  (loop for slot in *inventory-slot-order*
        append (let ((container (gethash slot (equipment avatar))))
                 (when container
                   (mapcar pred (contents container))))))

(defun remove-items-if (avatar pred)
  "Removes all items from the inventory of `avatar` for which `pred` evaluates
  to t, and returns a list of all removed items."
  (loop for slot in *inventory-slot-order*
        nconc (let ((container (gethash slot (equipment avatar))))
                (when container
                  (remove-from-contents-if container pred)))))

(defun match-in-inventory (avatar tokens &optional filter-fn)
  "Returns a list of (slot . item) for all items in inventory that match
  `tokens`."
  (let ((matches nil))
    (dolist (slot *inventory-slot-order*)
      (when-let ((container (gethash slot (equipment avatar))))
        (dolist (item (contents container))
          (when (and (or (null filter-fn) (funcall filter-fn item))
                     (match-tokens tokens item))
            (push (cons slot item) matches)))))
    matches))

(defun remove-from-inventory (avatar item slot)
  (remove-from-contents (gethash slot (equipment avatar)) item))

(defun match-equipment (avatar tokens)
  (keep-if #'(lambda (slot)
               (when-let ((item (gethash slot (equipment avatar))))
                 (when (match-tokens tokens item)
                   slot)))
           (remove :in-hands (hash-table-keys *equipment-slots*))))

;;;

(defevent take-item (actor item origin))

(defmethod do-take-item :around ((actor avatar) item origin)
  (cond
    ((not (typep item 'item))
     (show-text actor "You cannot take ~a." (describe-brief item :article :definite)))
    ((size> (size item) (size actor))
     (show-text actor "~a is too large for you to carry."
                (describe-brief item :article :definite :capitalize t)))
    ((not (find-inventory-slot actor item))
     (show-text actor "You have no space in your inventory for ~a."
                (describe-brief item :article :definite)))
    ;; TODO: check item requirements, uniqueness, ...
    ((query-observers (location actor) #'can-take-item actor item origin)
     (call-next-method)
     (notify-observers (location actor) #'did-take-item actor item origin))))

(defmethod do-take-item ((actor avatar) item origin)
  (let* ((slot (find-inventory-slot actor item))
         (container (gethash slot (equipment actor))))
    (remove-from-contents origin item)
    (add-to-contents container item)
    (show-text actor "You place ~a in your ~a."
               (describe-brief item)
               (describe-brief container :article nil))))

(defmethod do-take-item :after (actor (item item) origin)
  (respawn item origin))

(defmethod did-take-item ((observer avatar) actor item origin)
  (when (visible-p item observer)
    (when (not (eq observer actor))
      (show-text observer "~a takes ~a."
                 (describe-brief actor :capitalize t)
                 (describe-brief item))))
  (remove-neighbor observer item))

;;;

(defgeneric format-give-item (actor item)
  (:method (actor item)
    (format nil "~a gives you ~a."
            (describe-brief actor :article :definite :capitalize t)
            (describe-brief item))))

(defevent give-item (actor item recipient))

(defmethod do-give-item :around (actor item (recipient avatar))
  (cond
    ((size> (size item) (size recipient))
     (show-text recipient "~a tries to give you ~a, but it is too large for you to carry."
                (describe-brief actor :article :definite :capitalize t)
                (describe-brief item)))
    ((not (find-inventory-slot recipient item))
     (show-text recipient "~a tries to give you ~a, but you have no space in your inventory."
                (describe-brief actor :article :definite :capitalize t)
                (describe-brief item)))
    ((query-observers (location recipient) #'can-give-item actor item recipient)
     (call-next-method))))

(defmethod do-give-item (actor item (recipient avatar))
  (let* ((slot (find-inventory-slot recipient item))
         (container (gethash slot (equipment recipient))))
    (add-to-contents container item)
    (show-text recipient
               (concatenate
                'string
                (format-give-item actor item)
                (if (eq slot :in-hands)
                    ""
                    (format nil " You place ~a in your ~a."
                            (if (= 1 (stack-size item)) "it" "them")
                            (describe-brief container :article nil)))))
    item))

;;;

(defun find-item-of-same-type (actor item)
  (loop for slot in *inventory-slot-order*
        do (let* ((container (gethash slot (equipment actor)))
                  (match (find-item-of-same-type-in-container container item)))
             (when match
               (return (values container match))))))

(defun consume-item-of-same-type (actor item)
  (multiple-value-bind (container match) (find-item-of-same-type-in-container actor item)
    (remove-from-contents container match)))

;;;

(defcommand (actor ("get" "take") item "from" source ("into" "to") destination)
  "Take an item from your environment and place it into your inventory. The
  source of the item may be your location or a container within your location,
  such as a table or box. The destination is a container in your inventory, such
  as your backpack."
  (declare (ignore destination)) ; TODO:
  (let (origin)
    (if source
        (let ((matches (match-objects source
                                      (remove-if-not #'(lambda (x) (and (visible-p x actor)
                                                                        (typep x 'container)))
                                                     (contents (location actor))))))
          (case (length matches)
            (0 (show-text actor "You don't see anything matching \"~{~a~^ ~}\" that might contain items."
                          source))
            (1 (setf origin (first matches)))
            (otherwise
             (progn
               (show-text actor "Do you want to take something from ~a?"
                          (format-list (mapcar #'describe-brief matches) :conjunction "or"))))))
        (setf origin (location actor)))
    (when origin
      (let ((matches (match-objects item (keep-visible actor (contents origin)))))
        (if (= 0 (length matches))
            (show-text actor "You don't see anything matching \"~{~a~^ ~}\" that you can take." item)
            (dolist (item matches)
              (do-take-item actor item origin)))))))

;;; Drop one or more items.

(defevent drop-item (actor item inventory-slot))

(defmethod do-drop-item (actor item inventory-slot)
  (let ((container (gethash inventory-slot (equipment actor)))
        (location (location actor)))
    (remove-from-contents container item)
    (show-text actor "You drop ~a." (describe-brief item))
    ;; TODO: need a proper entry message.
    (enter-location item location nil)))

(defcommand (actor "drop" item)
  "Drop one or more items, removing them from your inventory and placing them on
  the ground at your current location. If *item* is not specified, you drop
  everything you are holding in your hands."
  (if item
      ;; Drop matching items.
      (let ((matches (match-in-inventory actor item)))
        (case (length matches)
          (0 (show-text actor "You don't have anything matching \"~{~a~^ ~}\" that you can drop."
                        item))
          (1 (destructuring-bind (slot . item) (first matches)
               (drop-item actor item slot)))
          (otherwise
           (show-text actor "Do you want to drop ~a?"
                      (format-list (mapcar #'(lambda (x) (describe-brief (cdr x))) matches)
                                   :conjunction "or")))))
      ;; Drop all items carried in hands.
      (let ((container (gethash :in-hands (equipment actor))))
        (loop while (contents container)
              do
                 (drop-item actor (first (contents container)) :in-hands)))))

;;; Display items in the avatar's inventory, which is the contents of any
;;; containers in its :backpack, :in-hands, and :coin-purse slots.

(defcommand (actor ("inventory" "inv") container)
  "Display a list of the items you are carrying in your inventory. This includes
  items in your backpack as well as anything you are carrying in your hands. If
  *container* is specified, show only items in that container."
  (let ((containers (if container
                        (list container) ; FIXME: match container slots
                        (remove-if #'null
                                   (mapcar #'(lambda (slot)
                                               (gethash slot (equipment actor)))
                                           *inventory-slot-order*)))))
    (dolist (container containers)
      (if (contents container)
          (show-text actor "Your ~a ~@?"
                     (describe-brief container :article nil)
                     (nonempty-pose container)
                     (format-list (mapcar #'describe-brief (contents container))))
          (show-text actor "Your ~a ~a"
                     (describe-brief container :article nil)
                     (empty-pose container))))))

;;; Equip an item, moving it from inventory to an equipment slot.

(defevent equip-item (actor item inventory-slot equipment-slot))

(defmethod do-equip-item :around (actor item inventory-slot equipment-slot)
  (cond
    ;; Check level.
    ((< (level actor) (level item))
     (show-text actor "You are not high enough level to equip ~a."
                (describe-brief item :article :definite)))
    ;; Check proficiency.
    ((and (proficiency item)
          (null (gethash (proficiency item) (skills actor))))
     (show-text actor "You are not proficient in the use of ~a."
                (describe-brief item :article :definite)))
    ;; Check observers.
    ((query-observers (location actor) #'can-equip-item actor item inventory-slot equipment-slot)
     (call-next-method))))

(defmethod do-equip-item (actor item inventory-slot equipment-slot)
  (with-slots (equipment) actor
    (stop-behavior actor :activity)
    (remove-from-inventory actor item inventory-slot)
    ;; If there's already an item equipped in the slot, move it to inventory.
    ;; Make sure this never fails by always allowing the item to be held in the
    ;; avatar's hands.
    (when-let ((prev-item (gethash equipment-slot equipment)))
      (let ((container (gethash (or (find-inventory-slot actor prev-item) :in-hands) equipment)))
        (show-text actor "You place ~a in your ~a."
                   (describe-brief prev-item)
                   (describe-brief container :article nil))
        (add-to-contents container prev-item)))
    ;; If the item's slot is :both-hands, also clear the :off-hand slot in the
    ;; same way.
    (when-let ((off-hand (and (eq (slot item) :both-hands) (gethash :off-hand equipment))))
      (let ((container (gethash (or (find-inventory-slot actor off-hand) :in-hands) equipment)))
        (show-text actor "You place ~a in your ~a."
                   (describe-brief off-hand)
                   (describe-brief container :article nil))
        (add-to-contents container off-hand)))
    ;; Equip the item.
    (setf (gethash equipment-slot equipment) item)
    (show-text actor "You equip ~a." (describe-brief item))))

(defun select-equipment-slot (avatar item)
  "Returns the actual slot that will be used to equip `item`."
  (case (slot item)
    (:both-hands :main-hand)
    (:either-hand
     (if (gethash :main-hand (equipment avatar)) :off-hand) :main-hand)
    (:either-finger
     (if (gethash :left-finger (equipment avatar)) :right-finger :left-finger))
    (otherwise (slot item))))

;;; Display a list of the avatar's equipment.

(defparameter *equipment-slot-order*
  '(:head :torso :back :hands :waist :legs :feet
    :ears :neck :wrists :left-finger :right-finger
    :backpack))

(defun show-equipment (actor)
  (with-slots (equipment) actor
    ;; List weapons.
    (let (weapons)
      (when-let ((off-hand (gethash :off-hand equipment)))
        (push (format nil "~a in your off-hand" (describe-brief off-hand)) weapons))
      (when-let ((main-hand (gethash :main-hand equipment)))
        (if (eq (slot main-hand) :both-hands)
            (push (format nil "~a in both hands" (describe-brief main-hand)) weapons)
            (push (format nil "~a in your main hand" (describe-brief main-hand)) weapons)))
      (if weapons
          (show-text actor "You are wielding ~a." (format-list weapons))
          (show-text actor "You are not wielding any weapons.")))
    ;; List tools.
    (if-let ((tool (gethash :tool equipment)))
      (show-text actor "You have ~a at the ready." (describe-brief tool))
      (show-text actor "You have no tool equipped."))
    ;; List worn equipment.
    (let ((items (keep-if #'(lambda (slot)
                              (when-let ((item (gethash slot equipment)))
                                (describe-brief item)))
                          *equipment-slot-order*)))
      (if items
          (show-text actor "You are wearing ~a." (format-list items))
          (show-text actor "You are not wearing anything.")))))

(defcommand (actor ("equip" "eq") item ("in" "on") slot)
  "When no item is specified, list the items that you currently have equipped.

  Otherwise, equip the specified item, moving it from your inventory to the
  appropriate equipment slot. Any item that is already equipped in that slot
  will be returned to your inventory.

  If an item can be equipped in multiple slots, such as a ring which can be worn
  on either hand, you can use \"in\" or \"on\" to specify the slot to use. For
  example, `equip gold ring on left finger`."
  (declare (ignore slot)) ; FIXME: allow  explicit slot
  ;; FIXME: equipping a backpack must transfer contents first
  (if (null item)
      (show-equipment actor)
      (let ((matches (match-in-inventory actor item #'(lambda (x) (typep x 'equipment)))))
        (case (length matches)
          (0 (show-text actor
                        "You don't have anything matching \"~{~a~^ ~}\" that you can equip."
                        item))
          (1 (destructuring-bind (inventory-slot . item) (first matches)
               (equip-item actor item inventory-slot (select-equipment-slot actor item))))
          (t (show-text actor "Do you want to equip ~a?"
                        (format-list (mapcar #'cdr matches) :conjunction "or")))))))

;;

(defevent unequip-item (actor item equipment-slot))

(defmethod do-unequip-item :around (actor item equipment-slot)
  (when (query-observers (location actor) #'can-unequip-item actor item equipment-slot)
    (call-next-method)))

(defmethod do-unequip-item :around (actor (item container) equipment-slot)
  (if (> (length (contents container)) 0)
      (show-text actor "You cannot unequip ~a until it is empty."
                 (describe-brief container :article :definite))
      (call-next-method)))

(defmethod do-unequip-item (actor item equipment-slot)
  (with-slots (equipment) actor
    (stop-behavior actor :activity)
    (let ((container (gethash (or (find-inventory-slot actor item) :in-hands) equipment)))
      (show-text actor "You unequip ~a and place it in your ~a."
                 (describe-brief item)
                 (describe-brief container :article nil))
      (setf (gethash equipment-slot (equipment actor)) nil)
      (add-to-contents container item))))

(defcommand (actor ("unequip" "uneq") item ("into" "to") container)
  "Unequip an item and move it to your inventory. If no container is specified,
  the item is placed into any equipped container with sufficient space, or into
  your hands if there is no other place for the item."
  (declare (ignore container)) ; FIXME: allow choice of container
  (if item
      (let ((matches (match-equipment actor item)))
        (if matches
            (dolist (slot matches)
              (unequip-item actor (gethash slot (equipment actor)) slot))
            (show-text actor "You don't have anything equipped that matches that description.")))
      (show-text actor "What do you want to unequip?")))
