(in-package :dripping-caverns)

#|

These caverns are the long-time home of the myconids -- a friendly fungus race
-- and the rockeaters, creatures who devour rocks to survive. The myconids have
been able to live in relative peace with the rockeaters by giving them 'shinies'
as gifts, e.g. jade or copper. The rockeaters especially prize those and eat
them whole instead of grinding them into dust first.

Recently a group of undergnomes passed through and discovered an ancient
altar-like object. They also captured a dwarven explorer who was already there,
studying the artifact.

The undergnomes chased the myconids out of that section of the caverns --
depriving them of access to the caverns where dewcaps, a prized delicacy, are
found -- and began trying to understand the artifact, with the coerced help of
the dwarf.

The artifact can be activated as long as specific stones are placed into four
sockets. These 'shinies' were eaten by rockeaters long ago. An inscription on
the artifact itself provides clues as to which stone must be placed in each
socket.

A rockeater can be induced to vomit its 'shinies' by feeding it a concoction
made from something found in the caverns, as described by the myconid shaman. Or
you can just kill them.

Placing stones incorrectly causes electrical damage, spread across all creatures
near the artifact.

Placing stones correctly causes the entire room containing the artifact room to
rotate, so that the entrance points west instead of east. This blocks access to
the room from the undergnome camp, but provides access to a new passage.

Down that passage is a guardian golem and, beyond that, a treasure chest that is
the reward at the end of this story.

|#

(defregion dripping-caverns
  (name "Dripping Caverns")
  (full "An underground stream winds its way above and through these caves. The
    constantly dripping water has led to the formation of extraordinary
    stalactites throughout the area.")
  (climate :underground))

(defproto narrow-passage (portal)
  (brief "a narrow passage")
  (pose "leads ~a."))

(defproto cavern (location)
  (domain :underground)
  (surface :stone))

;;; entrance

(defproto crevice (portal)
  (brief "a narrow crevice")
  (pose "leads toward distant daylight.")
  (message "You carefully follow a narrow ledge alongside the turbulent water."))

(defentity entrance (cavern)
  (brief "Whirlpool")
  (full "A clamorous stream of turbulent water emerges from a narrow crevice in
    the north wall and forms a swirling pool that fills most of the cavern,
    leaving only a narrow ledge around its perimeter. The water is clearly
    draining out through an opening beneath the pool's surface.")
  (surface :shallow-water)
  (exits ((crevice :north silverwood::canyon-end)
          (narrow-passage :southwest north-barricade
                          :southeast slanted-passage-top))))

;; north barricade

(defproto barricade (narrow-passage)
  (brief "a crude barricade")
  (pose "obstructs the passage to the ~a.")
  (message "You clamber over the barricade, making a fair amount of noise in
    the process."))

(defproto wicker-boat (entity)
  ;; FIXME: The boat can only be carried in your hands and requires them to
  ;; otherwise be empty.
  (brief "a small wicker boat")
  (full "This lightweight boat is suitable for use in calm waters."))

(defentity north-barricade (cavern)
  (brief "Rubble-Filled Chamber")
  (full "A partial cave-in has left much of this chamber choked with debris.")
  (exits ((narrow-passage :northeast entrance)
          (barricade :southwest north-guardroom)))
  (contents (wicker-boat)))

;; north guardroom

(defproto undergnome-guard (monster)
  (brief "an undergnome guard"))

(defentity north-guardroom (cavern)
  (brief "Guardroom")
  (exits ((barricade :northeast north-barricade)
          (narrow-passage :east dewcap-field-nw :southwest undergnome-camp-n)))
  (contents (undergnome-guard)))

;; dewcap fields

(defproto mature-dewcap (item)
  (brief "a mature dewcap")
  (pose "draws your attention."))

(defmethod do-take-item :after (actor (item mature-dewcap) origin)
  (with-delay (30)
    (do-enter-location (make-instance 'mature-dewcap) origin nil)))

(defproto dewcap-field (cavern)
  (brief "Dewcap Fields")
  (full "Water droplets constantly fall from the high ceiling of this chamber,
    almost like rain. Huge stalactites and stalagmites abound and often merge to
    form columns. Hundreds of small green mushrooms blanket the loamy floor.")
  (surface :dirt))

(defentity dewcap-field-nw (dewcap-field)
  (exits ((narrow-passage :west north-guardroom :east dewcap-field-ne
                          :southeast dewcap-field-se)))
  (contents (mature-dewcap)))

(defentity dewcap-field-ne (dewcap-field)
  (exits ((narrow-passage :west dewcap-field-nw :south dewcap-field-se)))
  (contents (mature-dewcap)))

(defentity dewcap-field-se (dewcap-field)
  (exits ((narrow-passage :north dewcap-field-ne :northwest dewcap-field-nw)))
  (contents (mature-dewcap)))

;; undergnome camp

(defproto undergnome-laborer (monster)
  (brief "an undergnome laborer"))

(defentity undergnome-camp-n (cavern)
  (brief "Undergnome Camp")
  (full "The gnomes have set up a number of makeshift shelters in this large
    cavern to protect themselves from the constantly dripping water.")
  (exits ((narrow-passage :northeast north-guardroom :south undergnome-camp-s)))
  (contents (undergnome-laborer)))

(defentity makeshift-table (entity)
  (brief "a makeshift table")
  (pose "stands in the center of the chamber.")
  (full "The table consists of a warped wooden plank balanced on two
    equally-sized boulders."))

(defentity makeshift-chairs (entity)
  (brief "several makeshift chairs")
  (pose "are strewn about.")
  (full "Each chair consists of...a small boulder."))

(defentity undergnome-camp-s (cavern)
  (brief "Undergnome Camp")
  (full "This cavern is relatively dry. Its low ceiling is supported by a
    number of natural stone columns.")
  (exits ((narrow-passage :north undergnome-camp-n :west undergnome-camp-w
                          :south undergnome-hq :east south-guardroom)))
  (contents (makeshift-table makeshift-chairs)))

(defproto undergnome-magus (monster)
  (brief "an undergnome magus")
  (pose "is studing the carvings."))

(defentity undergnome-camp-w (cavern)
  (brief "Undergnome Camp")
  (full "This long, narrow cavern has unusually smooth walls; numerous
    geometric shapes have been expertly carved upon their surface.")
  (exits ((narrow-passage :west artifact-room :east undergnome-camp-s)
          (iron-gate :north holding-cell)))
  (contents (undergnome-magus)))

;; TODO: lock the holding cell, give the commander the key

(defproto dwarven-archaeologist (npc)
  (brief "Brindan Bristlebeard")
  (pose "stands in a relatively dry spot.")
  (full "Brindan is a robust, red-haired dwarven male. Although he is currently
    rather wet and bedraggled, his green eyes sparkle."))

(defentity holding-cell (cavern)
  (brief "Holding Cell")
  (full "This tiny chamber is especially damp and uncomfortable.")
  (exits ((iron-gate :south undergnome-camp-w)))
  (contents (dwarven-archaeologist)))

(defproto undergnome-commander (monster)
  (brief "the undergnome commander"))

(defentity undergnome-hq (cavern)
  (brief "Undergnome Headquarters")
  (full "The walls of this small chamber have been decorated with
    freshly-tanned skins that give the place a pungent smell.")
  (exits ((narrow-passage :north undergnome-camp-s)))
  (contents (undergnome-commander)))

#|

;; south guardroom

(defentity south-guardroom cavern-room
  [:brief "Guardroom"
   :exits [:southeast (barricade south-barricade)
           :west undergnome-camp-s]
   :contents [(undergnome-guard)]])

;; south barricade

(defentity south-barricade cavern-room
  [:brief "Tilted Chamber"
   :full "The floor of this long cavern tilts sharply down to the southeast."
   :exits [:northwest (barricade south-guardroom)
           :southeast rockeater-den-nw]])

;;; artifact room

(defentity ancient-artifact fixture
  [:brief "a circular bronze dais"
   :pose "stands in the middle of the room."
   :full "An intricate map is carved into the top surface of the dais, but the
     lands it depicts are unfamiliar. Four small hemispherical sockets are
     located around the map's edge, each corresponding to a cardinal compass
     direction.

     Around the sides of the dais you find an inscription:

     > Devouring flames leap from the north; raging floods rush from the south.
     The west wind crashes against the bulwark of the eastern mountains. When
     these forces meet, the world shall spin and the guardian shall arise to
     protect what is ours."])

;; TODO: rotate the artifact room to connect to the guardian's hall when the
;; artifact is activated.

(defentity artifact-room cavern-room
  [:brief "Artifact Chamber"
   :full "This circular chamber has smooth stone walls, a domed ceiling, and a
     mosaic tile floor."
   :surface :tile
   :exits [:east undergnome-camp-w]
   :contents [ancient-artifact]])

;;; guardian's hall

;; TODO: the rubble pile turns into a golem if anyone tries to go west, and
;; prevents leaving until it is slain, then it becomes rubble again and won't
;; reform for some amount of time. Maybe it drops a heart that is needed to open
;; the west door.

(defentity rubble-pile fixture
  [:brief "a large pile of rubble"
   :pose "stands in the middle of the chamber."])

(defentity guardian-hall cavern-room
  [:brief "Hall of the Guardian"
   :surface :tile
   :exits [:west treasure-room]
   :contents [rubble-pile]])

;;; treasure room

(defentity treasure-room cavern-room
  [:brief "Treasure Room"
   :full "Who doesn't like treasure?"
   :surface :tile
   :exits [:east guardian-hall]])

;;; rockeater den

(defproto rockeater mob
  [:brief "a rockeater"
   :full "The rockeater is a pony-sized creature with bulbous yellow eyes and a
     wide mouth filled with enormous, flat teeth. Numerous stalagmite-like
     spines grow from its stony hide. It moves about slowly on six stubby
     legs."
   :respawn 30])

(defproto rockeater-den cavern-room
  [:brief "Rockeater Den"
   :full "The stone walls and floor of this cavern bear numerous marks, scrapes,
     and gouges. Little piles of gray dust are littered about."])

(defentity rockeater-den-nw rockeater-den
  [:exits [:northwest south-barricade :south rockeater-den-w :east rockeater-den-n]])

(defentity rockeater-den-n rockeater-den
  [:exits [:west rockeater-den-nw :east rockeater-den-ne]])

(defentity rockeater-den-ne rockeater-den
  [:exits [:west rockeater-den-n :south rockeater-den-e]])

(defentity rockeater-den-w rockeater-den
  [:exits [:north rockeater-den-nw :south rockeater-den-sw]])

(defentity rockeater-sanctum rockeater-den
  [:exits [:east rockeater-den-e]])

(defentity rockeater-den-e rockeater-den
  [:exits [:west rockeater-sanctum :north rockeater-den-ne :south rockeater-den-se]])

(defentity rockeater-den-sw rockeater-den
  [:exits [:north rockeater-den-w :east rockeater-den-s]])

(defentity rockeater-den-s rockeater-den
  [:exits [:west rockeater-den-sw :east rockeater-den-se]])

(defentity rockeater-den-se rockeater-den
  [:exits [:west rockeater-den-s :north rockeater-den-e]])

;;; slanted passage

(defproto slanted-passage cavern-room
  [:brief "Slanted Passage"])

(defentity slanted-passage-top slanted-passage
  [:full "This is the top of a narrow passage that slants steeply down to the
     east."
   :exits [:northwest entrance :east slanted-passage-mid]])

(defentity slanted-passage-mid slanted-passage
  [:full "This is part of a narrow passage that slants steeply down to the
     east."
   :exits [:west slanted-passage-top :east slanted-passage-bot]])

(defentity slanted-passage-bot slanted-passage
  [:full "This is the bottom of a wide tunnel that slants steeply up to the
     west."
   :exits [:west slanted-passage-mid :northeast ceiling-waterfall]])

;; lower stream and pools

(defentity ceiling-waterfall cavern-room
  [:brief "Waterfalls"
   :full "A deluge of cold water rushes in from several openings in the high
     ceiling, creating a number of waterfalls that feed a wide pool. The pool,
     in turn, feeds a sluggish stream that meanders away to the east."
   :surface :shallow-water
   :exits [:southwest slanted-passage-bot :east lower-stream-1]])

(defentity lower-stream-1 cavern-room
  [:brief "Sluggish Stream"
   :full "A slow-moving stream flows eastward through this large chamber."
   :surface :shallow-water
   :exits [:west ceiling-waterfall :east lower-stream-2]])

(defentity lower-stream-2 cavern-room
  [:brief "Sluggish Stream"
   :full "A slow-moving stream flows eastward through this large chamber."
   :surface :shallow-water
   :exits [:west lower-stream-1 :east lower-pool-w :south myconid-camp-ne]])

(defentity lower-pool-w cavern-room
  [:brief "Wide Pool"
   :full "A sluggish stream enters from the west and forms a pool of cold water
     that fills most of this large chamber."
   :surface :shallow-water
   :exits [:west lower-stream-2
           :down (exit underwater-passage-w :hidden true)]])

(defproto underwater-passage room
  [:brief "Underwater Passage"
   :full "This is a wide, water-filled cavern."
   :domain :underground
   :surface :shallow-water])

(defentity underwater-passage-w underwater-passage
  [:exits [:up lower-pool-w :east underwater-passage-e]])

(defentity underwater-passage-e underwater-passage
  [:exits [:up lower-pool-e :west underwater-passage-w]])

(defentity lower-pool-e cavern-room
  [:brief "Wide Pool"
   :full "A still pool of cold water fills the western half of this chamber. The
     sound of dripping water echoes around you. The pool drains to the east
     though a narrow channel."
   :surface :shallow-water
   :exits [:down (exit underwater-passage-e :hidden true)
           :east cockatrice-lair]])

;;; myconid camp

(defproto myconid-camp-exit exit
  [:brief "the cavern"
   :pose "continues to the ~a."])

(defproto myconid-camp cavern-room
  [:brief "Myconid Camp"
   :full "This large cavern has a musty, earthy odor."
   :surface :dirt
   :exit-proto myconid-camp-exit])

(defentity myconid-camp-ne myconid-camp
  [:exits [:north (narrow-passage lower-stream-2)
           :west myconid-camp-nw :south myconid-camp-se]])

(defentity myconid-camp-nw myconid-camp
  [:exits [:east myconid-camp-ne :south myconid-camp-sw]])

(defentity myconid-camp-se myconid-camp
  [:exits [:north myconid-camp-ne :west myconid-camp-sw]])

(defentity myconid-camp-sw myconid-camp
  [:exits [:north myconid-camp-nw :east myconid-camp-se]])

;;; cockatrice lair

(defentity cockatrice-lair cavern-room
  [:brief "Cockatrice's Lair"
   :full "A narrow stream winds its way through the many stalagmites that dot
     the floor of large chamber."
   :exits [:west lower-pool-e :east world.perenvale/cliff-cave]])

|#
