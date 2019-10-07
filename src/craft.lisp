(in-package :charm)

(defproto crafting-tool (tool))

;;; A recipe describes the requirements for crafting an item.

(defclass recipe (ability)
  ((item
    :initarg :item :reader item
    :documentation "An instance of the item produced by the recipe.")
   (materials
    :initarg :materials :reader materials
    :documentation "Instances of the materials required to craft the recipe.")))

(defmethod command ((ability recipe))
  (find-command "craft"))

(defmacro recipe-list (skill &body specs)
  "A convenience macro to help create a list of recipes."
  `(list ,@(mapcar (lambda (spec)
                     (destructuring-bind (rank item materials) spec
                       `(make-instance 'recipe
                                       :skill ',skill
                                       :rank ,rank
                                       :item (make-instance ',item)
                                       :materials (list ,@(loop for (material count) on materials by #'cddr
                                                                collect `(make-instance ',material :stack-size ,count))))))
                   specs)))

;;; A material is a crafted item that can in turn be used to craft another item.

(defproto material (stackable-item))

;;;

(defun craftable-recipes (avatar skill)
  "Returns a list of the recipes requiring `skill` that are craftable by
  `avatar`. Considers recipes granted by gaining ranks in `skill` and those
  learned from other sources."
  (when-let ((rank (gethash (key skill) (skills avatar))))
    (labels ((craftable-recipe-p (ability)
               (and (typep ability 'recipe)
                    (<= (rank ability) rank))))
      (concatenate 'list
                   (remove-if-not #'craftable-recipe-p (abilities skill))
                   (remove-if-not #'craftable-recipe-p (abilities avatar))))))

(defmethod match-tokens (tokens (target recipe))
  (match-tokens tokens (item target)))

;;; FIXME: move this

(defun find-inventory-items (type avatar)
  "Returns a list of (container . item) for all items in the inventory of
  `avatar` that have type `type`."
  (loop for slot in *inventory-slot-order*
        append (when-let ((container (gethash slot (equipment avatar))))
                 (keep-if #'(lambda (item)
                              (when (typep item type)
                                (cons container item)))
                          (contents container)))))

(defun find-recipe-materials (recipe avatar)
  "Returns two values. The first is a list of (container item count) for
  materials found in the inventory of `avatar`. The second is a list of (type .
  count) for materials that were not found."
  (let (found missing)
    (loop for material in (materials recipe) do
      (let ((items (find-inventory-items (type-of material) avatar))
            (count (stack-size material)))
        (loop while (and items (> count 0)) do
          (destructuring-bind (container . item) (first items)
            (let ((n (min (stack-size item) count)))
              (push (list container item n) found)
              (decf count n)
              (pop items))))
        (when (> count 0)
          (push (cons (type-of material) count) missing))))
    (values found missing)))

;;;

(defevent craft-item (actor item recipe)
  (stop-behavior actor :activity)
  (multiple-value-bind (found missing) (find-recipe-materials recipe actor)
    (cond
      (missing
       (show-text actor "You do not have all the materials required to craft ~a."
                  (describe-brief item)))
      ;;; TODO: other checks
      (t
       (start-behavior actor :activity #'craft item recipe found)))))

(defmethod do-craft-item (actor item recipe)
  (show-text actor "You successfully craft ~a." (describe-brief item)))

(defbehavior craft (actor item recipe materials)
    ((tool (gethash :tool (equipment actor))))
  (:start
   (show-text actor "You begin crafting ~a with your ~a."
              (describe-brief item)
              (describe-brief tool :article nil))
   (change-state :finish 5))
  (:finish
   (do-craft-item actor item recipe)
   (remove-behavior actor :activity))
  (:stop
   (show-text actor "Your crafting attempt has been interrupted.")))

#+nil(defun offer-craft (actor item recipe)
       nil)

(defcommand (actor "craft" item ("with" "using") extra-materials)
  "Attempt to craft *item*, optionally adding *extra-materials* to enhance the
  quality or attributes of the result.

  In order to craft an item, you must:

  - Have achieved the required rank in the appropriate skill;

  - Have an appropriate tool equipped;

  - Have all the required materials (as specified in the `help:recipe`) and
    optional extra materials in your inventory; and

  - Be near an appropriate crafting station.

  If all of the above conditions are met, you will see a message that lists the
  materials that will be consumed and displays your chance of success. You can
  then type `help:accept` to continue with the crafting attempt.

  Crafting takes several seconds. You can save time by crafting multiple
  instances of the same item at once, e.g. `craft 10 iron ingots`. Your chance
  of success applies to each item individually.

  Your crafting attempt will fail if you are interrupted. You can stop crafting
  without any chance of failure by using the `help:stop` command. Note that, if
  your attempt fails, there is a chance you will lose some of the crafting
  materials you used."
  ;; TODO: require a crafting station at (location actor)
  (declare (ignore extra-materials))
  (if-let ((tool (require-type (gethash :tool (equipment actor)) 'crafting-tool)))
    (let* ((skill (find-skill (required-skill tool)))
           (recipes (if item (match-objects item (craftable-recipes actor skill)))))
      (case (length recipes)
        (0 (show-text actor "You cannot craft any such item."))
        (1 (let ((recipe (first recipes)))
             (craft-item actor (item recipe) recipe)))
        (t (show-text actor "Do you want to craft ~a?"
                      (format-list (mapcar #'(lambda (recipe)
                                               (describe-brief (item recipe)))
                                           recipes)
                                   :conjunction "or")))))
    (show-text actor "You do not have a crafting tool equipped.")))
