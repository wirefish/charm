(in-package :charm)

(defproto creature (entity)
  (attitude :neutral)
  (begins-quests nil)
  (ends-quests nil))

(defproto monster (combatant creature))

(defproto npc (creature)
  (attitude :friendly))

(defproto vendor (npc)
  (sells nil))

(defproto trainer (npc)
  (teaches nil))
