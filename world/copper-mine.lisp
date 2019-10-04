(in-package :copper-mine)

#|

An old copper mine was abandoned years ago, and subsequently occupied by a band
of kobolds. The kobolds hide in the mine as protection against the spiders in
Silverwood. They also cultivate mushrooms (a delicacy, second only to fresh
meat) and prey upon the occasional wanderer who is foolish enough to enter the
mine.

|#

(defregion copper-mine
  (name "Abandoned Copper Mine")
  (full "This mine once yielded plentiful copper ore, but was abandoned years
    ago due to the increasing dangers in the nearby woods.")
  (climate :underground))

(defproto rough-tunnel (portal)
  (brief "a rough-hewn tunnel")
  (pose "leads ~a to another part of the mine."))

(defproto mine (location)
  (domain :underground)
  (surface :stone))

;;; kobold variants

(defproto kobold-claw (natural-weapon)
  (brief "a dirty claw")
  (damage-type :slashing)
  (attack-verb "rakes"))

(defproto kobold-worker (monster)
  (brief "a kobold worker")
  (icon 'kobold)
  (level 1)
  (attacks '(kobold-claw))
  (respawn-delay '(30 60)))

(defproto kobold-shortsword (one-handed-weapon)
  (brief "a rusty shortsword")
  (level 2)
  (damage-type :piercing)
  (attack-verb "pokes"))

(defproto kobold-guard (monster)
  (brief "a kobold guard")
  (icon 'kobold)
  (level 2)
  (attacks '(kobold-shortsword))
  (respawn-delay '(45 90)))

(defproto kobold-spear (two-handed-weapon)
  (brief "a wicked spear")
  (level 3)
  (damage-type :piercing)
  (attack-verb "impales"))

(defproto kobold-chieftain (monster)
  (brief "the kobold chieftain")
  (icon 'kobold)
  (level 3)
  (attacks '(kobold-spear))
  (respawn-delay '(60 120)))

;;; entry

(deflocation entry (mine)
  (brief "Mine Entry")
  (full "This narrow tunnel slants steeply down to the north. Its low roof is
    supported by heavy timber beams.")
  (exits ((dirt-path :south silverwood::forest-3-1)
          (rough-tunnel :north shaft-top))))

;;; shaft

(defproto shaft-down (portal)
  (brief "the mine shaft")
  (pose "descends deeper into the earth."))

(defproto shaft-up (portal)
  (brief "the mine shaft")
  (pose "ascends toward the surface."))

(deflocation shaft-top (mine)
  (brief "Top of Mine Shaft")
  (full "This is the top of a vertical shaft. Irregular, shallow handholds on
    the wall provide an uncertain means of descent.")
  (exits ((rough-tunnel :south entry)
          (shaft-down :down shaft-middle))))

(deflocation shaft-middle (mine)
  (brief "Middle of Mine Shaft")
  (full "This is the middle of a vertical shaft. The handholds on the wall
    provide only a tenuous grip, making movement up or down very difficult.")
  (exits ((shaft-up :up shaft-top) (shaft-down :down shaft-bottom))))

(deflocation shaft-bottom (mine)
  (brief "Bottom of Mine Shaft")
  (full "This is the bottom of a vertical shaft. You can just make out handholds
    on the wall that provide a means of climbing upwards.")
  (exits ((shaft-up :up shaft-middle) (rough-tunnel :north tunnel-1))))

;; tunnel

(defproto tunnel-room (mine)
  (brief "Mine Tunnel")
  (full "You are in a narrow tunnel deep underground. Heavy timbers
    support the low ceiling."))

(deflocation tunnel-1 (tunnel-room)
  (exits ((rough-tunnel :south shaft-bottom :west tunnel-3
                        :east tunnel-10 :north tunnel-2))))

(deflocation tunnel-2 (tunnel-room)
  (exits ((rough-tunnel :south tunnel-1)))
  (contents (copper-deposit kobold-worker)))

(deflocation tunnel-3 (tunnel-room)
  (exits ((rough-tunnel :northwest tunnel-4 :east tunnel-1 :southwest tunnel-20)))
  (contents (kobold-worker)))

(deflocation tunnel-4 (tunnel-room)
  (exits ((rough-tunnel :southeast tunnel-3 :northeast tunnel-5 :west tunnel-16)))
  (contents (copper-deposit)))

(deflocation tunnel-5 (tunnel-room)
  (exits ((rough-tunnel :southwest tunnel-4 :east tunnel-6))))

(deflocation tunnel-6 (tunnel-room)
  (exits ((rough-tunnel :west tunnel-5 :east tunnel-7 :north antechamber))))

(deflocation tunnel-7 (tunnel-room)
  (exits ((rough-tunnel :west tunnel-6 :southeast tunnel-8)))
  (contents (copper-deposit)))

(deflocation tunnel-8 (tunnel-room)
  (exits ((rough-tunnel :northwest tunnel-7 :northeast tunnel-14 :south tunnel-9)))
  (contents (kobold-worker)))

(deflocation tunnel-9 (tunnel-room)
  (exits ((rough-tunnel :west tunnel-10 :north tunnel-8 :southeast tunnel-11))))

(deflocation tunnel-10 (tunnel-room)
  (exits ((rough-tunnel :west tunnel-1 :east tunnel-9)))
  (contents (kobold-worker)))

(deflocation tunnel-11 (tunnel-room)
  (exits ((rough-tunnel :northwest tunnel-9 :northeast tunnel-12)))
  (contents (copper-deposit)))

(deflocation tunnel-12 (tunnel-room)
  (exits ((rough-tunnel :southwest tunnel-11 :north tunnel-13)))
  (contents (kobold-worker)))

(deflocation tunnel-13 (tunnel-room)
  (exits ((rough-tunnel :south tunnel-12 :northwest tunnel-14 :east tunnel-15))))

(deflocation tunnel-14 (tunnel-room)
  (exits ((rough-tunnel :southwest tunnel-8 :southeast tunnel-13 :north grotto))))

(deflocation tunnel-15 (tunnel-room)
  (exits ((rough-tunnel :west tunnel-13)))
  (contents (copper-deposit)))

(deflocation tunnel-16 (tunnel-room)
  (exits ((rough-tunnel :east tunnel-4 :southwest tunnel-17)))
  (contents (kobold-worker)))

(deflocation tunnel-17 (tunnel-room)
  (exits ((rough-tunnel :northeast tunnel-16 :south tunnel-18)))
  (contents (copper-deposit)))

(deflocation tunnel-18 (tunnel-room)
  (exits ((rough-tunnel :north tunnel-17 :east tunnel-19)))
  (contents (kobold-worker)))

(deflocation tunnel-19 (tunnel-room)
  (exits ((rough-tunnel :west tunnel-18 :east tunnel-20))))

(deflocation tunnel-20 (tunnel-room)
  (exits ((rough-tunnel :west tunnel-19 :northeast tunnel-3)))
  (contents (copper-deposit)))

;; hideout

(deflocation antechamber (mine)
  (brief "Guard Chamber")
  (full "The tunnel broadens here into a good-sized chamber. The floor has been
    worn smooth. Refuse piles fill the shadowed corners.")
  (exits ((rough-tunnel :south tunnel-6 :north hideout)))
  (contents (kobold-guard kobold-guard)))

(deflocation hideout (mine)
  (brief "Kobold Hideout")
  (full "The ceiling of this large chamber is supported by massive, rough-hewn
    timbers. Smoky torches along the walls provide dim, flickering light. Much
    of the floor is covered with a thick bed of slightly-rotten grass and
    leaves.")
  (exits ((rough-tunnel :south antechamber :east storage)))
  (contents (kobold-guard kobold-chieftain)))

(deflocation storage (mine)
  (brief "Storage Chamber")
  (full "This small chamber contains barrels and boxes of primitive supplies.")
  (exits ((rough-tunnel :west hideout))))

;; grotto

;; TODO: add gatherable nodes of a few mushroom types, the respawn picks a
;; random type for the next spawn.

(deflocation grotto (mine)
  (brief "Mushroom Grotto")
  (full "This appears to be a natural cavern. Stalactites hang from the ceiling;
    water drips from their tips into numerous shallow pools. The floor is
    covered with loam and decaying leaves, presumably brought here by the
    kobolds. Various types of mushrooms grow throughout the chamber.")
  (surface :dirt)
  (exits ((rough-tunnel :south tunnel-14)))
  (contents (kobold-worker kobold-guard)))
