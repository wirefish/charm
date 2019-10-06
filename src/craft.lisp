(in-package :charm)

;;; A recipe describes the requirements for crafting an item.

(defclass recipe (ability)
  ((item
    :initarg :item :reader item
    :documentation "The item produced by the recipe.")
   (materials
    :initarg :materials :reader materials
    :documentation "The materials required to craft the recipe.")))

(defmethod command ((ability recipe))
  (find-command "craft"))

(defmacro recipe-list (skill &body specs)
  "A convenience macro to help create a list of recipes."
  `(list ,@(mapcar (lambda (spec)
                     (destructuring-bind (rank item materials) spec
                       `(make-instance 'recipe
                                       :skill ',skill
                                       :rank ,rank
                                       :item ',item
                                       :materials ',(loop for (material count) on materials by #'cddr
                                                          collect (cons material count)))))
                   specs)))

;;; A material is an intermediate crafted item.

(defproto material (stackable-item))

;;;

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
  (declare (ignore item extra-materials))
  (show-text actor "TBD"))
