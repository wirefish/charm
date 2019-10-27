(in-package :charm)

#|

Functions that support examining the contents of the player's inventory and
moving items within, into, or out of inventory.

There are a number of verbs/events that interact with inventory in this way:

- get/take: environment -> inventory
- put: inventory -> inventory or environment
- drop: inventory -> location (specialized form of put)
- discard: inventory -> gone
- equip: inventory -> equipment
- unequip: equipment -> inventory
- give: NPC -> inventory

|#

(defparameter *inventory-slot-order* '(:backpack :in-hands))

;;;

(defun find-in-inventory-if (pred avatar)
  "Returns a list of (slot . item) for the items in the inventory of `avatar` for
  which `pred` evaluates to t."
  (loop for slot in *inventory-slot-order*
     nconc
       (when-let ((container (gethash slot (equipment avatar))))
         (mapcar #'(lambda (item) (cons slot item))
                 (find-in-container-if pred container)))))

(defun find-type-in-inventory (type avatar)
  "Returns a list of (slot . item) for the item in the inventory of `avatar`
  which have type `type`."
  (find-in-inventory-if #'(lambda (item) (typep item type)) avatar))

(defun count-type-in-inventory (type avatar)
  (reduce #'+ (mapcar #'(lambda (slot)
                          (if-let ((container (gethash slot (equipment avatar))))
                            (count-type-in-container type container)
                            0))
                      *inventory-slot-order*)))

;;;

(defmethod match-tokens (tokens (target cons))
  (match-tokens tokens (cdr target)))

(defun match-in-inventory (tokens avatar &key test)
  "Returns a list of (slot . item) for all items in inventory that match
  `tokens`. If `test` is not nil, only objects for which the predicate `test`
  evaluates to t are returned."
  (match-objects
   tokens
   (loop for slot in *inventory-slot-order*
         nconc (when-let ((container (gethash slot (equipment avatar))))
                 (mapcar #'(lambda (item)
                             (cons slot item))
                         (if test
                             (find-in-container-if test container)
                             (contents container)))))))

;;;

(defun remove-from-inventory-if (pred avatar)
  "Removes all items from the inventory of `avatar` for which `pred` evaluates
  to t. Returns a list of all removed items."
  (loop for slot in *inventory-slot-order*
     nconc
       (when-let ((container (gethash slot (equipment avatar))))
         (multiple-value-bind (num-removed stacks-removed)
             (remove-from-container-if pred container)
           (declare (ignore num-removed))
           stacks-removed))))

(defun remove-from-inventory (slot item avatar &key quantity)
  (remove-from-container item (gethash slot (equipment avatar)) :quantity quantity))

(defun remove-type-from-inventory (type avatar &key quantity)
  "Removes objects of type `type` from the inventory of `avatar`. If `quantity`
  is not nil, removes at most `quantity` objects. Returns three values: the
  quantity actually removed, a list of (container . item) for stacks that were
  removed, and a list of (container . item) for stacks that were reduced in size
  but not removed."
  (let ((num-removed 0) removed-stacks reduced-stacks)
    (loop for slot in *inventory-slot-order*
       while (or (null quantity) (< num-removed quantity))
       do
         (when-let ((container (gethash slot (equipment avatar))))
           (multiple-value-bind (n removed reduced)
               (remove-type-from-container type container
                                           :quantity (and quantity (- quantity num-removed)))
             (incf num-removed n)
             (nconcf removed-stacks (mapcar #'(lambda (item) (cons container item)) removed)
             (nconcf reduced-stacks (mapcar #'(lambda (item) (cons container item)) reduced))))))
    (values num-removed removed-stacks reduced-stacks)))

;;;

(defun match-equipment (tokens avatar)
  "Returns a list of those equipment slots for `avatar` that contain items that
  match user input `tokens`."
  (remove-if-not #'(lambda (slot)
                     (when-let ((item (gethash slot (equipment avatar))))
                       (match-tokens tokens item)))
                 (remove :in-hands (hash-table-keys *equipment-slots*))))

;;;

(defun addable-to-inventory (item avatar &key quantity)
  "If `item` can be added to the inventory of `avatar`, returns the container
  to which it would be added. Otherwise, returns nil."
  (some #'(lambda (slot)
            (when-let ((container (gethash slot (equipment avatar))))
              (when (addable-to-container item container :quantity quantity)
                container)))
        *inventory-slot-order*))

(defun add-to-inventory (item avatar &key force quantity remove-from)
  "Attempts to add `item` to the inventory of `avatar`. On success, returns two
  values: the container to which the item was added, and a list of the stacks
  that were modified within that container. Returns nil on failure. If `force`
  is t, the item is always successfully added to the avatar's hands as a last
  resort."
  (some #'(lambda (slot)
            (when-let* ((container (gethash slot (equipment avatar)))
                        (stacks (add-to-container item container
                                                  :force (and force (eq slot :in-hands))
                                                  :quantity quantity
                                                  :remove-from remove-from)))
              (values container stacks)))
        *inventory-slot-order*))

;;; The `take-item` event occurs when `actor` removes `item` from `origin`,
;;; which could be the actor's location or a container within that location. If
;;; `quantity` is non-nil then only part of the stack represented by `item` is
;;; taken.

(defevent take-item (actor quantity item origin))

(defmethod do-take-item :around ((actor avatar) quantity item origin)
  (cond
    ((not (typep item 'item))
     (show-text actor "You cannot take ~a." (describe-brief item :article :definite)))
    ((size> (size item) (size actor))
     (show-text actor "~a is too large for you to carry."
                (describe-brief item :article :definite :capitalize t)))
    ((not (addable-to-inventory item actor :quantity quantity))
     (show-text actor "You have no space in your inventory for ~a."
                (describe-brief item :article :definite :count quantity)))
    ;; TODO: check item requirements, uniqueness, ...
    ((query-observers (location actor) #'can-take-item actor quantity item origin)
     (call-next-method)
     (notify-observers (location actor) #'did-take-item actor quantity item origin))))

(defmethod do-take-item ((actor avatar) quantity item origin)
  (when-let ((container (add-to-inventory item actor
                                          :quantity quantity
                                          :remove-from origin)))
    (show-text actor "You place ~a in your ~a."
               (describe-brief item :count quantity)
               (describe-brief container :article nil))))

(defmethod do-take-item :after (actor quantity (item item) origin)
  ;; FIXME: This allows drop/take to cause infinite respawns...
  (respawn item origin))

(defmethod did-take-item ((observer avatar) actor quantity item origin)
  (when (visible-p item observer)
    (when (not (eq observer actor))
      (show-text observer "~a takes ~a."
                 (describe-brief actor :capitalize t)
                 (describe-brief item :count quantity)))
    (remove-neighbor observer item)))

(defun find-containers (actor tokens containers pose &optional default)
  (if tokens
      (let ((matches (match-objects (split-quantity tokens) containers)))
        (case (length matches)
          (0
           (show-text actor
                      "You don't see anything matching \"~{~a~^ ~}\" that can contain items."
                      tokens)
           nil)
          (1
           (first matches))
          (otherwise
           (show-text actor "Do you want to ~a ~a?"
                      pose
                      (format-list (mapcar #'describe-brief matches) :conjunction "or")))))
      default))

(defun match-quantity (tokens objects)
  (multiple-value-bind (tokens quantity) (split-quantity tokens)
    (multiple-value-bind (matches plural) (match-objects tokens objects)
      (do (results
           (remaining (cond
                        ((eq quantity :all) most-positive-fixnum)
                        ((null quantity) (if plural most-positive-fixnum 1))
                        (t quantity))))
          ((or (null matches) (= remaining 0)) results)
        (let* ((match (pop matches))
               (n (min (stack-size match) remaining)))
          (push (cons match n) results)
          (decf remaining n))))))

(defcommand (actor ("get" "take") item "from" source ("into" "to") destination)
  "Take an item from your environment and place it into your inventory. The
  source of the item may be your location or a container within your location,
  such as a table or box. The destination is a container in your inventory, such
  as your backpack."
  ;; TODO: destination? also, deal with quantity
  (declare (ignore destination))
  (when-let ((origin (find-containers actor source
                                      (remove-if-not #'(lambda (x) (and (visible-p x actor)
                                                                        (typep x 'container)))
                                                     (contents (location actor)))
                                      "take something from"
                                      (location actor))))
    (let ((matches (match-quantity item (keep-visible actor (contents origin)))))
      (if (null matches)
          (show-text actor "You don't see anything matching \"~{~a~^ ~}\" that you can take." item)
          (dolist (match matches)
            (destructuring-bind (item . quantity) match
              (take-item actor quantity item origin)))))))
;;;


(defcommand (actor "loot" container)
  (let ((containers (match-objects container
                                   (remove-if-not #'(lambda (x)
                                                      (and (visible-p x actor)
                                                           (typep x 'container)))
                                                  (contents (location actor))))))
    (cond
      (containers
       (dolist (container containers)
         (do ()
             ((null (contents container)))
           (let* ((item (pop (contents container)))
                  (inv-container (add-to-inventory item actor :force t)))
             (show-text actor "You loot ~a from ~a and place ~a in your ~a."
                        (describe-brief item)
                        (describe-brief container)
                        (if (= 1 (stack-size item)) "it" "them")
                        (describe-brief inv-container :article nil))))))
      (container
       (show-text actor "There is nothing here like that you can loot."))
      (t
       (show-text actor "There is nothing here you can loot.")))))

;;; The `give-item` event occurs when `actor` gives a newly-created entity to
;;; `recipient`.

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
    ((not (addable-to-inventory item recipient))
     (show-text recipient "~a tries to give you ~a, but you have no space in your inventory."
                (describe-brief actor :article :definite :capitalize t)
                (describe-brief item)))
    ((query-observers (location recipient) #'can-give-item actor item recipient)
     (call-next-method))))

(defmethod do-give-item (actor item (recipient avatar))
  (multiple-value-bind (container stacks) (add-to-inventory item recipient :force t)
    (when container
      (show-text recipient
                 (concatenate
                  'string
                  (format-give-item actor item)
                  (format nil " You place ~a in your ~a."
                          (if (= 1 (stack-size item)) "it" "them")
                          (describe-brief container :article nil))))
      (update-inventory recipient :changed (mapcar #'(lambda (stack)
                                                       (cons (slot container) stack))
                                                   stacks))
      item)))

;;; The `drop-item` event occurs when `actor` drops `item`, moving it from
;;; `inventory-slot` to the actor's location. If `quantity` is non-nil, only
;;; part of the stack represented by `item` is dropped.

(defevent drop-item (actor item inventory-slot))

(defstruct drop-entry (actor))

(defmethod describe-entry (observer item location (entry drop-entry))
  (with-slots (actor) entry
    (if (eq observer actor)
        (format nil "You drop ~a." (describe-brief item))
        (format nil "~a drops ~a."
                (describe-brief actor :capitalize t)
                (describe-brief item)))))

(defmethod do-drop-item (actor item inventory-slot)
  (let ((container (gethash inventory-slot (equipment actor)))
        (location (location actor)))
    (remove-from-container item container)
    (enter-location item location (make-drop-entry :actor actor))))

(defcommand (actor "drop" item)
  "Drop one or more items, removing them from your inventory and placing them on
  the ground at your current location. If *item* is not specified, you drop
  everything you are holding in your hands."
  (if item
      ;; Drop matching items.
      (multiple-value-bind (tokens quantity) (split-quantity item)
        (multiple-value-bind (matches match-multiple) (match-in-inventory tokens actor)
          (format-log :info "~a ~a ~a ~a" quantity tokens matches match-multiple)
          (cond
            ((null matches)
             (show-text actor "You don't have anything matching \"~{~a~^ ~}\" that you can drop."
                        tokens))
            (t
             (do ((quantity (or quantity (and (not match-multiple) 1))))
                 ((or (null matches) (and quantity (= quantity 0))))
               ;; FIXME: handle quantity properly
               (destructuring-bind (slot . item) (first matches)
                 (drop-item actor item slot)
                 (when quantity (decf quantity))
                 (pop matches)))))))
      ;; Drop all items carried in hands.
      (let ((container (gethash :in-hands (equipment actor))))
        (if (contents container)
            (loop while (contents container)
                  do
                     (drop-item actor (first (contents container)) :in-hands))
            (show-text actor "You aren't carrying anything in your hands.")))))

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
    (remove-from-inventory inventory-slot item actor)
    (let ((changed-slots (list equipment-slot)))
      ;; If there's already an item equipped in the slot, move it to inventory.
      ;; Make sure this never fails by always allowing the item to be held in the
      ;; avatar's hands.
      (when-let ((prev-item (gethash equipment-slot equipment)))
        (let ((container (add-to-inventory prev-item actor :force t)))
          (show-text actor "You place ~a in your ~a."
                     (describe-brief prev-item)
                     (describe-brief container :article nil))))
      ;; If the item's slot is :both-hands, also clear the :off-hand slot in the
      ;; same way.
      (when-let ((off-hand (and (eq (slot item) :both-hands) (gethash :off-hand equipment))))
        (let ((container (add-to-inventory off-hand actor :force t)))
          (show-text actor "You place ~a in your ~a."
                     (describe-brief off-hand)
                     (describe-brief container :article nil))
          (push :off-hand changed-slots)))
      ;; Equip the item.
      (setf (gethash equipment-slot equipment) item)
      (show-text actor "You equip ~a." (describe-brief item))
      (update-equipment actor changed-slots))))

(defun select-equipment-slot (item avatar)
  "Returns the actual slot that will be used to equip `item`."
  (case (slot item)
    (:both-hands :main-hand)
    (:either-hand
     (if (gethash :main-hand (equipment avatar)) :off-hand) :main-hand)
    (:either-finger
     (if (gethash :left-finger (equipment avatar)) :right-finger :left-finger))
    (otherwise (slot item))))

(defparameter *equipment-slot-order*
  '(:head :torso :back :hands :waist :legs :feet
    :ears :neck :wrists :left-finger :right-finger
    :backpack))

(defun show-equipment (actor)
  "Display a list of the items equipped by `actor`."
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
      (let ((matches (match-in-inventory item actor :test #'(lambda (x) (typep x 'equipment)))))
        (case (length matches)
          (0 (show-text actor
                        "You don't have anything matching \"~{~a~^ ~}\" that you can equip."
                        item))
          (1 (destructuring-bind (inventory-slot . item) (first matches)
               (equip-item actor item inventory-slot (select-equipment-slot item actor))))
          (t (show-text actor "Do you want to equip ~a?"
                        (format-list (mapcar #'(lambda (x) (describe-brief (cdr x))) matches)
                                     :conjunction "or")))))))

;;

(defevent unequip-item (actor item equipment-slot))

(defmethod do-unequip-item :around (actor item equipment-slot)
  (when (query-observers (location actor) #'can-unequip-item actor item equipment-slot)
    (call-next-method)))

(defmethod do-unequip-item :around (actor (item container) equipment-slot)
  (if (> (length (contents item)) 0)
      (show-text actor "You cannot unequip ~a until it is empty."
                 (describe-brief item :article :definite))
      (call-next-method)))

(defmethod do-unequip-item (actor item equipment-slot)
  (with-slots (equipment) actor
    (stop-behavior actor :activity)
    (let ((container (add-to-inventory item actor :force t)))
      (show-text actor "You unequip ~a and place it in your ~a."
                 (describe-brief item)
                 (describe-brief container :article nil))
      (setf (gethash equipment-slot (equipment actor)) nil)
      (update-equipment actor (list equipment-slot)))))

(defcommand (actor ("unequip" "uneq") item)
  "Unequip an item and move it to your inventory. The item is placed into any
  equipped container with sufficient space, or into your hands if there is no
  other place for the item."
  (if-let ((item (split-quantity item)))
      (let ((matches (match-equipment item actor)))
        (if matches
            (dolist (slot matches)
              (unequip-item actor (gethash slot (equipment actor)) slot))
            (show-text actor "You don't have anything equipped that matches that description.")))
      (show-text actor "What do you want to unequip?")))
