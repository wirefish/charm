(in-package :lib)

;;; Skills.

(defskill mining
  (name "mining")
  (summary "Allows you to gather ore, gemstones, and other materials when you
    have a suitable mining tool equipped.")
  (level 1)
  (max-rank 100)
  (cost '(:karma 5)))

;;; Tools.

(defproto mining-tool (gathering-tool)
  (required-skill 'mining))

(defproto copper-pickaxe (mining-tool)
  (brief "a copper pickaxe")
  (level 1))

;;; Ores are items collected using the mining skill that can be smelted to
;;; produce ingots.

(defproto ore (resource)
  (brief "a chunk[s] of ~(~a~) ore")
  (full "The ore is a small piece of ~(~:*~a~)-bearing rock. It can be smelted to
    obtain ~(~a~) in its pure form.")
  (icon 'ore)
  (stack-limit 100)
  (content nil)
  (required-skill 'mining))

(defmethod describe-brief ((subject ore) &rest args)
  (declare (ignore args))
  (format nil (call-next-method) (content subject)))

(defmethod describe-full ((subject ore))
  (format nil (full subject) (content subject)))

(defproto copper-ore (ore)
  (content 'copper)
  (required-rank 0))

(defproto tin-ore (ore)
  (content 'tin)
  (required-rank 20))

(defproto zinc-ore (ore)
  (content 'zinc)
  (required-rank 40))

(defproto iron-ore (ore)
  (content 'iron)
  (required-rank 60))

(defproto silver-ore (ore)
  (content 'silver)
  (required-rank 80))

;;; Miners can also collect gemstones.

(defproto gemstone (resource)
  (icon 'gemstone)
  (stack-limit 100)
  (required-skill 'mining))

(defproto azurite (gemstone)
  (brief "an azurite"))

;;; Mining nodes.

(defproto mining-deposit (resource-node)
  (required-skill 'mining))

(defproto copper-deposit (mining-deposit)
  (brief "a copper deposit")
  (resources ((copper-ore 1 1 4)
              (azurite 1/20 1 1))))
