(in-package :lib)

;;; Tools.

(defproto smelting-tool (crafting-tool)
  (icon 'hammer)
  (required-skill 'smelting))

(defproto copper-smelting-hammer (smelting-tool)
  (brief "a copper smelting hammer")
  (level 1))

(defproto bronze-smelting-hammer (smelting-tool)
  (brief "a bronze smelting hammer")
  (level 10)
  (required-rank 20))

;;; A furnace is the crafting station required for smelting.

(defproto furnace (entity)
  (brief "a furnace")
  (icon 'furnace))

;;; Ingots are produced by smelting ore.

(defproto ingot (material)
  (brief "a ~(~a~) ingot")
  (full "The ingot is a small bar of pure ~(~a~), ready to be used as a crafting
    material.")
  (icon 'ore)
  (content nil)
  (required-skill 'smelting))

(defmethod describe-brief ((subject ingot) &rest args)
  (declare (ignore args))
  (format nil (call-next-method) (content subject)))

(defmethod describe-full ((subject ingot))
  (format nil (full subject) (content subject)))

;;; Types of ingots.

(defproto copper-ingot (ingot)
  (content 'copper))

(defproto tin-ingot (ingot)
  (content 'tin))

;;; Skills.

(defskill smelting
  (name "smelting")
  (summary "Allows you to smelt ore, producing pure metal ingots which can be
    used to craft useful items. You must be near a furnace in order to smelt.")
  (level 1)
  (max-rank 100)
  (cost '(:karma 2))
  (abilities
   (recipe-list smelting
     (0 copper-ingot (copper-ore 3))
     (20 tin-ingot (tin-ore 3)))))
