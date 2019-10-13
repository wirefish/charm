(in-package :lib)

;;; Skills.

(defskill mining
  (name "mining")
  (summary "Allows you to gather ore, gemstones, and other materials when you
    have a suitable mining tool equipped.")
  (level 1)
  (max-rank 100)
  (cost '(:karma 3)))

;;; Tools.

(defproto mining-tool (gathering-tool)
  (icon 'pickaxe)
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
  (content nil)
  (required-skill 'mining))

(defmethod describe-brief ((subject ore) &rest args)
  (declare (ignore args))
  (format nil (call-next-method) (content subject)))

(defmethod describe-full ((subject ore))
  (format nil (full subject) (content subject)))

;;; Miners can also collect raw gemstones. An ore deposit typically has one or
;;; more associated gemstones that have a low percentage chance of being found.

(defproto raw-gemstone (resource)
  (required-skill 'mining))

;;; A mining deposit contains ore, raw gemstones, and sometimes other things.

(defproto mining-deposit (resource-node)
  (required-skill 'mining))

;;; Copper

(defproto copper-ore (ore)
  (icon 'metal-2-ore)
  (content 'copper)
  (required-rank 0))

(defproto raw-azurite (raw-gemstone)
  (brief "a raw azurite")
  (icon 'sapphire-small)
  (required-rank 10))

(defproto raw-turquoise (raw-gemstone)
  (brief "a raw turquoise[]")
  (icon 'turquoise)
  (required-rank 20))

(defproto copper-deposit (mining-deposit)
  (brief "a copper deposit")
  (icon 'metal-2-ore)
  (resources ((copper-ore 1 1 4)
              (raw-azurite 1/20 1 1)
              (raw-turquoise 1/50 1 1))))

;;; Tin

(defproto tin-ore (ore)
  (icon 'metal-3-ore)
  (content 'tin)
  (required-rank 20))

;;; Zinc

(defproto zinc-ore (ore)
  (icon 'metal-1-ore)
  (content 'zinc)
  (required-rank 40))

;;; Iron

(defproto iron-ore (ore)
  (icon 'metal-3-ore)
  (content 'iron)
  (required-rank 60))

;;; Silver

(defproto silver-ore (ore)
  (icon 'metal-5-ore)
  (content 'silver)
  (required-rank 80))

;;; Gold

(defproto gold-ore (ore)
  (icon 'metal-4-ore)
  (content 'gold)
  (required-rank 100))
