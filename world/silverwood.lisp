(in-package :silverwood)

(defregion silverwood
  (name "Silverwood")
  (full "The Silverwood is nestled in the hills west of Arwyck.")
  (climate :temperate))

(defproto giant-spider-bite (weapon)
  (damage-type :piercing)
  (base-damage (3 6))
  (attack-verb "bites")
  (attack-delay 2))

(defproto giant-spider (monster)
  (brief "a giant forest spider")
  (pose "hangs from a nearby branch.")
  (full "This enormous webspinner has luminous eyes and long, hairy legs. Its
    kind prefer to lurk in the forest canopy, awaiting the opportunity to drop
    down upon unsuspecting prey.")
  (icon 'long-legged-spider)
  (level 1)
  (attacks '(giant-spider-bite))
  (entry-pose "drops down from the branches above."))

;; FIXME: drops [(currency/silver 0.5 1 3)]])

;;; prototypes

(defproto forest-portal (portal)
  (brief "the forest")
  (pose "continues to the ~a."))

(defproto stream-portal (portal)
  (brief "the stream")
  (pose "continues to the ~a."))

(defproto forest (location)
  (brief "Forest")
  (full "You are deep in a forest of birch and maple trees.")
  (domain :outdoor)
  (surface :forest))

(defproto forest-path (location)
  (brief "Winding Path")
  (full "You are on a narrow path that winds through a forest of birch and maple
    trees.")
  (domain :outdoor)
  (surface :dirt))

(defproto stream-portal (portal)
  (brief "the stream")
  (pose "continues to the ~a."))

(defproto forest-stream (location)
  (brief "Swift Stream")
  (full "A burbling, turbulent stream of cold water runs north to south through
    the forest.")
  (domain :outdoor)
  (surface :shallow-water))

;; cabin

(defproto old-sword (entity)
  (brief "a single-edged sword")
  (pose "hangs above the hearth.")
  (icon 'katana)
  (full "Although clearly old and in need of sharpening, the sword appears to be
    of excellent craftsmanship. Its long hilt is wrapped with dark cord and its
    small circular guard is ornately carved.

    A slip of parchment is tacked to the wall below the sword. It reads:

    > This blade is given to Knight-Captain Zara Alenys in recognition of her
    heroism at the Battle of Three Rivers. May it help her succeed in her
    quest."))

(defquest find-spiderwebs
  (name "Silk for Sewing")
  (summary "Collect large spiderwebs for the hermit in Silverwood.")
  (level 1))

(defproto large-spiderweb (quest-item)
  (brief "a large spiderweb")
  (pose "is visible in the branches of a nearby tree.")
  (icon 'spider-web)
  (full "The strands of this web are especially strong and thick.")
  (alts ("large web"))
  (unique 5)
  (quest 'find-spiderwebs))

(defmethod do-take-item :after (actor (item large-spiderweb) origin)
  (advance-quest actor find-spiderwebs 1/5)
  (with-delay (30)
    (do-enter-location (make-instance 'large-spiderweb) origin nil)))

(defproto hermit (npc)
  (brief "an old hermit")
  (pose "whistles as she mends a shirt.")
  (full "The hermit is an elderly woman, her skin deeply wrinkled from many
    years of wind and sun. She is stooped and walks only with great effort, but
    her eyes and mind are as sharp as ever. An old scar runs down the left side
    of her face.")
  (begins-quests '(find-spiderwebs)))

(defmethod do-offer-quest (actor (quest (eql find-spiderwebs)) npc)
  (show-say actor npc "Ah, a visitor. Few people come to my little house these
    days, what with the spiders and worse lurking in the woods. Speaking of
    spiders, I could use your help if you are willing."))

(defmethod do-accept-quest (actor (quest (eql find-spiderwebs )) npc)
  (show-say actor npc "As you can see I like to sew; it helps me pass the time
    and keeps my nieces and nephews from running around bare naked! There's just
    one problem: I need spider silk to make thread. Perhaps you can bring me a
    few webs? You can find them all over the forest, but watch out for the
    webspinners! They can be dangerous."))

(defmethod do-advise-quest (actor (quest (eql find-spiderwebs)) npc)
  (show-say actor npc "Having trouble finding webs? Just look around the woods,
    you'll see them."))

(defmethod do-finish-quest (actor (quest (eql find-spiderwebs)) npc)
  (show-say actor npc "Thank you! You can rest assured that I will put these to
    good use."))

(defentity cabin (location)
  (brief "Cabin in the Woods")
  (full "This one-room cabin is tiny but well maintained.")
  (domain :indoor)
  (surface :wood)
  (exits ((exit-doorway :out forest-2-3)))
  (contents (old-sword hermit)))

;;; forest

(defentity forest-1-1 (forest)
  (exits ((forest-portal :east forest-2-1 :south forest-1-2))))

(defentity forest-2-1 (forest)
  (exits ((forest-portal :west forest-1-1 :east forest-3-1 :south forest-2-2)))
  (contents (large-spiderweb)))

(defentity forest-3-1 (forest)
  (exits ((forest-portal :west forest-2-1 :east forest-4-1 :south forest-3-2)
          (dirt-path :north copper-mine::entry)))
  (contents (giant-spider)))

(defentity forest-4-1 (forest)
  (exits ((forest-portal :west forest-3-1 :east forest-5-1 :south forest-4-2))))

(defentity forest-5-1 (location)
  (brief "Shallow Pool")
  (full "A shallow pool has formed at the base of a small waterfall to the
    north. Spray from the waterfall hangs in the air. Thick moss covers the
    ground around the pool.")
  (domain :outdoor)
  (surface :shallow-water)
  (exits ((forest-portal :west forest-4-1 :east forest-6-1 :south forest-5-2)
          ;; FIXME: hidden
          (portal :north behind-the-waterfall))))

(defentity behind-the-waterfall (location)
  (brief "Behind the Waterfall")
  (full "You are in a space behind the waterfall where the earth has eroded
    away, forming a small chamber. Light filters through the curtain of water
    and dances on the walls. The uneven floor is covered with several inches of
    cold water.")
  (domain :underground)
  (surface :shallow-water)
  (exits ((portal :south forest-5-1))))

(defentity forest-6-1 (forest)
  (exits ((forest-portal :west forest-5-1 :south forest-6-2)))
  (contents (giant-spider)))

(defentity forest-0-2 (forest)
  (exits ((forest-portal :east forest-1-2 :south forest-0-3)))
  (contents (giant-spider)))

(defentity forest-1-2 (forest)
  (exits ((forest-portal :east forest-2-2 :north forest-1-1 :south forest-1-3 :west forest-0-2))))

(defentity forest-2-2 (forest)
  (exits ((forest-portal :west forest-1-2 :east forest-3-2 :north forest-2-1 :south forest-2-3))))

(defentity forest-3-2 (forest)
  (exits ((forest-portal :west forest-2-2 :east forest-4-2 :north forest-3-1 :south forest-3-3))))

(defentity forest-4-2 (forest)
  (exits ((forest-portal :west forest-3-2 :east forest-5-2 :north forest-4-1 :south forest-4-3)))
  (contents (large-spiderweb)))

(defentity forest-5-2 (forest-stream)
  (exits ((forest-portal :west forest-4-2 :east forest-6-2)
          (stream-portal :north forest-5-1 :south under-the-bridge))))

(defentity forest-6-2 (forest)
  (exits ((forest-portal :west forest-5-2 :north forest-6-1 :south forest-6-3)))
  (contents (large-spiderweb)))

(defentity forest-0-3 (forest)
  (exits ((forest-portal :east forest-1-3 :south forest-0-4 :north forest-0-2)))
  (contents (large-spiderweb)))

(defentity forest-1-3 (forest)
  (exits ((forest-portal :east forest-2-3 :north forest-1-2 :south forest-1-4 :west forest-0-3))))

(defproto cabin-exterior (entity)
  (brief "a tiny cabin")
  (pose "stands in the middle of the clearing.")
  (icon 'house)
  (full "The cabin's exterior is painted in bright colors. Smoke rises from a
    stone chimney."))

(defentity forest-2-3 (forest)
  (brief "Clearing")
  (full "You are in a small meadow of wildflowers surrounded by trees.")
  (surface :grass)
  (exits ((forest-portal :west forest-1-3 :east forest-3-3 :north forest-2-2 :south forest-2-4)
          (entry-doorway :in cabin)))
  (contents (cabin-exterior)))

;; FIXME (exit cabin :brief "a gaily-painted door" :pose "leads into the cabin.")

(defentity forest-3-3 (forest)
  (exits ((forest-portal :west forest-2-3 :east forest-4-3 :north forest-3-2 :south forest-3-4))))

(defentity forest-4-3 (forest-path)
  (exits ((forest-portal :west forest-3-3 :north forest-4-2 :south forest-4-4)
          (dirt-path :east forest-5-3))))

(defentity forest-5-3 (location)
  (brief "Stone Bridge")
  (full "You stand on an arched bridge of white stone that spans the gurgling
    stream below.")
  (visible (("down"
             "You see the stream about ten feet below you. Its waters
             pass swiftly beneath the bridge.")))
  (domain :outdoor)
  (surface :stone)
  (exits ((dirt-path :west forest-4-3 :east forest-6-3))))

(defentity under-the-bridge (location)
  (brief "Under the Bridge")
  (full "The stream passes beneath an arched bridge of white stone. Someone has
    scratched a few words into the underside of the bridge; it is nearly
    illegible, but you think it says \"Anthony was here.\"")
  (z-offset -1)
  (domain :outdoor)
  (surface :shallow-water)
  (exits ((stream-portal :north forest-5-2 :south forest-5-4))))

(defentity forest-6-3 (forest-path)
  (exits ((dirt-path :west forest-5-3 :east arwyck::forest-gate)
          (forest-portal :north forest-6-2 :south forest-6-4))))

(defentity forest-0-4 (forest)
  (exits ((forest-portal :east forest-1-4 :north forest-0-3)))
  (contents (giant-spider)))

(defentity forest-1-4 (forest)
  (exits ((forest-portal :east forest-2-4 :north forest-1-3
                         :south forest-1-5 :west forest-0-4)))
  (contents (large-spiderweb)))

(defentity forest-2-4 (forest)
  (exits ((forest-portal :west forest-1-4 :east forest-3-4
                         :north forest-2-3 :south forest-2-5))))

(defentity forest-3-4 (forest)
  (exits ((forest-portal :west forest-2-4 :east forest-4-4
                         :north forest-3-3 :south forest-3-5))))

(defentity forest-4-4 (forest-path)
  (exits ((forest-portal :west forest-3-4 :east forest-5-4)
          (dirt-path :north forest-4-3 :south forest-4-5))))

(defentity forest-5-4 (forest-stream)
  (exits ((forest-portal :west forest-4-4 :east forest-6-4)
          (stream-portal :north under-the-bridge :south forest-5-5))))

(defentity forest-6-4 (forest)
  (exits ((forest-portal :west forest-5-4 :north forest-6-3 :south forest-6-5)))
  (contents (giant-spider)))

(defentity forest-1-5 (forest)
  (exits ((forest-portal :east forest-2-5 :north forest-1-4 :south forest-1-6))))

(defentity forest-2-5 (forest-path)
  (exits ((forest-portal :west forest-1-5 :north forest-2-4)
          (dirt-path :east forest-3-5 :south forest-2-6))))

(defentity forest-3-5 (forest-path)
  (exits ((dirt-path :west forest-2-5 :east forest-4-5)
          (forest-portal :north forest-3-4 :south forest-3-6))))

(defentity forest-4-5 (forest-path)
  (exits ((dirt-path :west forest-3-5 :north forest-4-4)
          (forest-portal :east forest-5-5 :south forest-4-6))))

(defentity forest-5-5 (forest-stream)
  (exits ((forest-portal :west forest-4-5 :east forest-6-5)
          (stream-portal :north forest-5-4 :south forest-5-6))))

(defentity forest-6-5 (forest)
  (exits ((forest-portal :west forest-5-5 :east forest-7-5
                         :north forest-6-4 :south forest-6-6)))
  (contents (large-spiderweb)))

(defentity forest-7-5 (location)
  (brief "Granite Outcrop")
  (full "You are at the base of an enormous granite spire that juts high above
    the forest canopy. Climbing it would require the proper gear and skill.")
  (domain :outdoor)
  (surface :forest)
  (exits ((forest-portal :west forest-6-5 :south forest-7-6)
          (portal :up outcrop-top)))) ; FIXME:

(defentity outcrop-top (location)
  (brief "Atop a Granite Outcrop")
  (full "You are precariously perched atop a high granite spire. The view is
    breathtaking: you can see the village of Arwyck to the northeast, and beyond
    it the waters of Emerald Bay. A small island is barely visible on the
    horizon.")
  (domain :outdoor)
  (surface :stone)
  (exits ((portal :down forest-7-5)))) ; FIXME:

(defentity forest-1-6 (forest)
  (exits ((forest-portal :east forest-2-6 :north forest-1-5 :south forest-1-7)))
  (contents (large-spiderweb)))

(defentity forest-2-6 (forest-path)
  (exits ((forest-portal :west forest-1-6 :east forest-3-6)
          (dirt-path :north forest-2-5 :south forest-2-7))))

(defproto ranger (npc)
  (brief "Nina Niyana")
  (pose "sits on a stump as she sharpens a knife.")
  (full "Nina is an elven woman dressed in the practical garb of a hunter. She
    wears her silver hair in a long braid.")
  (icon 'female-elf-silver))

(defmethod did-enter-room ((observer ranger) (actor avatar) location entry)
  (show-text actor "Nina smiles in welcome as you approach."))

(defproto rhody (npc)
  (brief "Rhody Mathers")
  (pose "sits in the grass near the firepit.")
  (full "Rhody is a young boy, perhaps eight years old. He looks a bit tired.
    His face is smeared with berry juice.")
  (icon 'human-boy))

(defentity forest-3-6 (forest)
  (brief "Rangers' Camp")
  (full "Several stained canvas tents have been erected around a tidy firepit
    in this small clearing.")
  (surface :grass)
  (exits ((forest-portal :west forest-2-6 :east forest-4-6
                         :north forest-3-5 :south forest-3-7)))
  (contents (ranger rhody)))

(defentity forest-4-6 (forest)
  (exits ((forest-portal :west forest-3-6 :east forest-5-6 :north forest-4-5)))
  (contents (large-spiderweb giant-spider)))

(defentity forest-5-6 (forest-stream)
  (exits ((forest-portal :west forest-4-6 :east forest-6-6)
          (stream-portal :north forest-5-5 :south canyon-stream-1))))

(defentity forest-6-6 (forest)
  (exits ((forest-portal :west forest-5-6 :east forest-7-6 :north forest-6-5)))
  (contents (giant-spider)))

(defentity forest-7-6 (forest)
  (exits ((forest-portal :west forest-6-6 :north forest-7-5))))

(defentity forest-1-7 (forest)
  (exits ((forest-portal :east forest-2-7 :north forest-1-6)))
  (contents (giant-spider)))

(defentity forest-2-7 (forest-path)
  (exits ((forest-portal :west forest-1-7 :east forest-3-7)
          (dirt-path :north forest-2-6))))

(defentity forest-3-7 (forest)
  (exits ((forest-portal :west forest-2-7 :north forest-3-6))))

;;; canyon-stream

(defproto canyon-stream (location)
  (brief "Canyon Stream")
  (full "A swirling stream of cold water runs north to south through a narrow,
    steep-walled canyon.")
  (domain :outdoor)
  (surface :shallow-water))

(defentity canyon-stream-1 (canyon-stream)
  (exits ((stream-portal :north forest-5-6 :south canyon-stream-2))))

(defentity canyon-stream-2 (canyon-stream)
  (exits ((stream-portal :north canyon-stream-1 :south canyon-end))))

(defproto crevice (portal)
  (brief "a narrow crevice")
  (pose "leads into the cliff face.")
  (message "You carefully follow a narrow ledge alongside the turbulent water."))

(defentity canyon-end (canyon-stream)
  (brief "Canyon's End")
  (full "The narrow canyon comes to an abrupt end, enclosed on three sides by
    steep walls. The waters of the stream froth and churn amongst numerous huge
    boulders before disappearing into a crevice in the south wall.")
  (exits ((stream-portal :north canyon-stream-2)
          (crevice :south dripping-caverns::entrance))))
