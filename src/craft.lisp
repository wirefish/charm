(in-package :charm)

(defproto crafting-tool (tool))

;;; A recipe describes the requirements for crafting an item.

(defclass recipe (ability)
  ((item
    :initarg :item :reader item
    :documentation "An instance of the item produced by the recipe.")
   (materials
    :initarg :materials :reader materials
    :documentation "The types and amounts of materials required to craft the
      recipe.")))

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
                                       :materials ',(loop for (material count) on materials by #'cddr
                                                          collect (cons material count)))))
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
        (1 (show-text actor "~{~a~}" recipes))
        (t (show-text actor "Do you want to craft ~a?"
                      (format-list (mapcar #'(lambda (recipe)
                                               (describe-brief (item recipe)))
                                           recipes)
                                   :conjunction "or")))))
    (show-text actor "You do not have a crafting tool equipped.")))
