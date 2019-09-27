(in-package :charm)

(defproto creature (entity)
  (attitude :neutral))

(defproto monster (combatant creature))

(defproto npc (creature)
  (attitude :friendly)
  (begins-quests nil)
  (ends-quests nil))

(defproto vendor (npc)
  (sells nil))

(defproto trainer (npc)
  (teaches nil))
