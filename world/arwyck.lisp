(in-package :arwyck)

(defregion arwyck
  (name "Village of Arwyck")
  (full "This once-sleepy hamlet has recently become a hub of adventuring
    activity.")
  (climate :temperate))

;;; docks

(defproto dock-location (location)
  (domain :outdoor)
  (surface :wood))

(defentity west-dock (dock-location)
  (brief "West Dock")
  (full "A short wooden dock juts out into the bay.")
  (exits ((gravel-path :south bayside-plaza-2))))

(defentity east-dock (dock-location)
  (brief "East Dock")
  (full "A rickety wooden dock extends out into the bay.")
  (exits ((gravel-path :south bayside-plaza-4))))

;;; bayside-plaza

(defproto bayside-plaza-portal (portal)
  (brief "the plaza")
  (pose "continues to the ~a."))

(defproto bayside-plaza-location (location)
  (brief "Bayside Plaza")
  (full "A cobbled stone plaza runs for a short distance along the shore of the
    bay.")
  (domain :outdoor)
  (surface :stone))

(defentity bayside-plaza-1 (bayside-plaza-location)
  (exits ((bayside-plaza-portal :east bayside-plaza-2))))

(defentity bayside-plaza-2 (bayside-plaza-location)
  (exits ((bayside-plaza-portal :west bayside-plaza-1 :east bayside-plaza-3)
          (gravel-path :north west-dock))))

(defentity bayside-plaza-3 (bayside-plaza-location)
  (exits ((bayside-plaza-portal :west bayside-plaza-2 :east bayside-plaza-4)
          (dirt-road :south harbor-road-1))))

(defentity bayside-plaza-4 (bayside-plaza-location)
  (exits ((bayside-plaza-portal :west bayside-plaza-3 :east bayside-plaza-5)
          (gravel-path :north east-dock))))

(defentity bayside-plaza-5 (bayside-plaza-location)
  (exits ((bayside-plaza-portal :west bayside-plaza-4))))

#|

;;; harbor-road

(defproto harbor-road-room room
  [:brief "Harbor Road"
   :domain :outdoor
   :surface :dirt])

(defentity harbor-road-1 harbor-road-room
  [:exits [:north bayside-plaza-3 :south harbor-road-2]])

(defentity harbor-road-2 harbor-road-room
  [:exits [:north harbor-road-1 :south village-square-n]])

;;; village-square

(defproto village-square-room room
  [:brief "Village Square"
   :full "This cobbled plaza is the heart of the village. Various
     shops line its perimeter."
   :domain :outdoor
   :surface :stone])

(defentity shaggy-dog npc
  [:brief "a shaggy white dog"
   :pose "is nearby."
   :icon "sniffing-dog.png"
   :full "The dog is very curious and closely watches anyone who passes by. Its
     fur is matted and dirty but it seems happy."]

  (do-talk [this actor npc]
    (show-text actor "The dog's ears perk up and it tilts its head to the side.")))

(defentity village-square-nw village-square-room
  [:exits [:east village-square-n :south village-square-w
           :north (exits/entry-doorway armory-training-hall)
           :west (exits/entry-doorway sword-shop)]
   :contents [shaggy-dog]])

(defbehavior move-shaggy-dog
  [:actor shaggy-dog
   :location village-square-nw
   :pos 0
   :directions [:south :south :east :east :north :north :west :west]]

  (:start [this]
    (change-state this :stretch))

  (:stretch [this]
    (send (:location this) emote (:actor this)
          "The shaggy dog gets up and stretches.")
    (change-state-async this 5 :move))

  (:move [this]
    (let [dir ((:directions this) (:pos this))
          exit (-> @(:location this) :exits dir)
          dest (and exit (resolve (:to exit)))]
      (send (:location this) exit-room (:actor this) exit)
      (-> this
          (assoc :location @dest)
          (update :pos #(mod (inc %) (count (:directions this))))
          (change-state-async 5 :sleep))))

  (:sleep [this]
    (send (:location this) emote (:actor this)
          "The shaggy dog lies down.")
    (change-state-async this 20 :stretch)))

(defentity village-square-n village-square-room
  [:exits [:west village-square-nw :east village-square-ne
           :south village-square-c :north harbor-road-2]])

(defentity village-square-ne village-square-room
  [:exits [:west village-square-n :south village-square-e
           :east (exits/doorway inn-common-room)]])

(defentity village-square-w village-square-room
  [:exits [:north village-square-nw :south village-square-sw :east village-square-c
           :west (exits/doorway library-main)]])

(defentity podium fixture
  [:brief "a low podium"
   :pose "stands in the center of the village square."
   :full "The podium appears solidly-built. A few steps lead up to its wide
     platform. When you speak from atop the podium, your words will be heard
     throughout the village square."])

(defentity village-square-c village-square-room
  [:exits [:west village-square-w :east village-square-e
           :north village-square-n :south village-square-s]
   :contents [podium]])

(defentity village-square-e village-square-room
  [:exits [:north village-square-ne :south village-square-se
           :west village-square-c :east east-road-1]])

(defentity village-square-sw village-square-room
  [:exits [:north village-square-w :east village-square-s :west forest-road-1
           :south (exits/entry-doorway lodge-foyer)]])

(defentity village-square-s village-square-room
  [:exits [:west village-square-sw :east village-square-se
           :north village-square-c :south south-road-1]])

(defentity crone npc
  [:brief "an old crone"
   :pose "stands off to the side, watching people pass by."
   :full "The crone wears a faded gray cloak. Her wavy white hair spills from
     beneath her hood. Her eyes are bright, and she watches any passersby with
     great interest."]

  (did-enter-room [this (actor :avatar) room entry]
    (show-text actor "The crone stares at you for a moment, then looks away."))

  (do-talk [this actor npc]
    (show-say actor this "Greetings, stranger. Your new body may be young, but I
      can sense the age of your spirit. This is not your first time around the
      block, so to speak. In a past life you were a great hero, but perhaps you
      have forgotten your deeds. A pity.

      Know this: you are here for a reason. This world needs you; I feel it in
      my bones, I hear it on the wind...but I know not why. What threat could be
      so grave that, in order to overcome it, we must tear history's heroes away
      from their peaceful slumber in the Dreamlands?

      Of course that raises another question: who or what is doing the tearing?

      In the village of my birth there is a saying: \"May you live in
      interesting times.\" Bah. Perhaps I will be lucky, and my days will end
      before things become too interesting. For you, though, I foresee no such
      luck.")))

(defentity village-square-se village-square-room
  [:exits [:west village-square-s :north village-square-e]
   :contents [crone]])

;;; sword shop

(defentity bryndan npc
  [:brief "Bryndan O'Donnel"
   :pose "cleans a longsword with an oiled rag."
   :full "Bryndan is a lanky man with freckled skin and gray eyes. His auburn
     hair hangs down to his shoulders."
   :sells []])

(defentity sword-shop room
  [:brief "Bryndan's House of Blades"
   :full "A variety of bladed weapons are displayed in racks that fill this
     small shop."
   :domain :indoor
   :surface :wood
   :exits [:east village-square-nw]
   :contents [bryndan]])

;;; inn

(defproto inn-room room
  [:domain :indoor
   :surface :wood
   :exit-proto exits/doorway])

(defentity barkeep npc
  [:brief "Sienna"
   :pose "is working behind the bar."
   :full "Sienna is a tall woman with short yellow hair and light brown eyes.
       She keeps a close eye on her patrons."
   :sells []]

  (do-talk [this actor npc]
    (show-say actor this "Welcome to the Golden Gryphon! I sell food and drink;
      I'd say it's the best in town but my mam told me never to lie, so I won't.
      If you're learning to cook, I can sell you a few recipes as well.")))

(defentity inn-common-room inn-room
  [:brief "Common Room"
   :full "The common room of the Golden Gryphon is a cheerful place. A crackling
     fire burns in the stone hearth. Numerous tables and chairs give the space a
     crowded feel, even when few customers are present. A long oak bar lines one
     wall."
   :exits [:west (exits/exit-doorway village-square-ne)
           :north inn-kitchen :up inn-upstairs-hall]
  :contents [barkeep]])

(defentity cooking-guildmaster npc
  [:brief "Dully the Cook"
   :pose "is stirring a pot of...something."
   :full "Dully is a portly dwarven male with a scraggly beard. He wears a
     grease-stained apron and a strange, poofy hat. He wields a huge wooden
     spoon as if it were a weapon."]

  (did-enter-room [this (actor :avatar) room entry]
    (show-say actor this "Aye, it's a good day for a stew, ain't it?"))

  (do-talk [this actor npc]
    (show-say actor this "Aye, if ye be lookin' ta learn ta cook, I be yer
      dwarf. Type `learn` ta see wha I can teach ya, or `help skills` ta learn
      more about skills an such.")))

(defentity inn-kitchen inn-room
  [:brief "Kitchen"
   :full "This large and somewhat disorganized kitchen smells faintly of ale and
     grease."
   :exits [:south inn-common-room]
   :contents [cooking-guildmaster]])

(defproto slice-of-rye item
  [:brief "a slice[s] of rye bread"
   :icon ["sliced-bread.png" "#ae9c55"]
   :full "This bread seems a little stale, but still adequate for, say, making a
     sandwich."
   :respawn 10])

(defentity inn-upstairs-hall inn-room
  [:brief "Upstairs Hall"
   :full "This dark, windowless hall leads to the Golden Gryphon's guest rooms."
   :exits [:down inn-common-room :west guest-room-1 :east guest-room-2]])

(defentity guest-room-1 inn-room
  [:brief "Small Guest Room"
   :full "This cramped room holds a small bed and an iron-bound chest. The
     window overlooks the village square."
   :exits [:east inn-upstairs-hall]])

(defentity guest-room-2 inn-room
  [:brief "Large Guest Room"
   :full "This room is tastefully furnished. The wide feather bed is lumpy but
     comfortable. An oak wardrobe features carvings of trees and forest
     creatures. The window provides a view of nearby rooftops and the rolling
     hills east of the village."
   :exits [:west inn-upstairs-hall]])

;;; armory

(defproto armory-room room
  [:domain :indoor
   :surface :wood])

(defentity warrior-guildmaster npc
  [:brief "Andalya"
   :pose "is in the center of the room, idly swinging a slender longsword."
   :full "Andalya is a lean, graying woman who wears loose clothing befitting
     one who teaches the martial arts."]

  (do-talk [this actor npc]
    (show-say actor this "Welcome, stranger. I am Andalya. It is my privilege to
      be the local representative of the Company of the Blade. We are a band
      dedicated to the practice of the martial arts. It is my job to recruit
      those who are interested in learning to defend themselves with good steel.

      Type `learn` to see the skills I can teach you. You can also type `help
      skills` to learn more about skills in general.")))

(defentity armory-training-hall armory-room
  [:brief "Training Hall"
   :full "This long hall has stone walls and an arched ceiling. The soft wooden
     floor is clean but marred from the many training matches that have been
     fought here."
   :exits [:south (exits/exit-doorway village-square-nw)]
   :contents [warrior-guildmaster]])

;;; explorers' lodge

(defproto lodge-room room
  [:domain :indoor
   :surface :wood
   :exit-proto exits/doorway])

(defentity quartermaster npc
  [:brief "the quartermaster"
   :pose "stands nearby."
   :full "The quartermaster is a slender man with graying hair and elegant
     mustaches. His clothing, although impeccably clean, seems a little too big
     for him."]

  (do-talk [this actor npc]
    (show-say actor this "Good day to you.")))

(defentity lodge-foyer lodge-room
  [:brief "Explorers' Lodge Foyer"
   :full "This airy room has wood-paneled walls and a thick carpet on the
     floor."
   :exits [:north (exits/exit-doorway village-square-sw)
           :south lodge-trophy-room :west lodge-library]
   :contents [quartermaster]])

(defentity stuffed-gorilla fixture
  [:brief "a stuffed gorilla"
   :pose "stands at the end of the room."
   :icon ["gorilla.png" "#cec4a3"]
   :full "This intimidating creature stands over eight feet tall. Even in death
     its expression is fierce. A plaque mounted on the wall reads:

     > Although it slew several dogs and gravely wounded our scout, our party
     defeated this beast as we traversed the jungles of Phaa, CY 549."])

(defentity lodge-trophy-room lodge-room
  [:brief "Explorers' Lodge Trophy Room"
   :full "The walls of this long room are lined with hunting prizes of all
     kinds: antlers, horns, mounted heads, and more."
   :exits [:north lodge-foyer :west lodge-workshop]
   :contents [stuffed-gorilla]])

(defentity lord-olmquist npc
  [:brief "Lord Olmquist"
   :pose "sits in a chair, smoking a pipe."
   :full "Olmquist is a rather stout, red-cheeked fellow with a bushy white
     beard and bald pate. Horn-rimmed spectacles perch upon his substantial
     nose. He wears a velvet smoking jacket, an open-necked white shirt, and
     dark silk trousers."]

  (do-talk [this actor npc]
    (show-say actor this "Why yes, be a dear and fetch me a bourbon, won't
      you?")))

(defentity lodge-library lodge-room
  [:brief "Explorers' Lodge Library"
   :full "Two walls of this room are lined with shelves full of books. A number
     of comfortable armchairs and small tables fill the remainder of the space."
   :exits [:east lodge-foyer :south lodge-workshop]
   :contents [lord-olmquist]])

(defentity swimming skill
  [:brief "swimming"
   :full "Allows you to swim in deep water, underwater, and in dangerous
     conditions."
   :karma 5
   :max-rank 100])

(defentity climbing skill
  [:brief "climbing"
   :full "Allows you to climb steep, slippery, and unstable surfaces."
   :karma 5
   :max-rank 100])

(defentity explorers-guildmaster npc
  [:brief "Naman Artani"
   :pose "is hunched over a strange device on the table."
   :full "Naman is a wiry young man who wears a scarred leather apron over his
     practical attire. His curly auburn hair floats in a wild cloud around his
     head."
   :teaches-skills [#'swimming #'climbing]]

  (do-talk [this actor npc]
    (show-say actor this "Welcome to the Explorer's Lodge. Members of our guild
      travel across Atalea in search of rarities: strange creatures, lost
      treasures, and ancient knowledge. As you might imagine, survival skills
      come in quite handy in this vocation!

      I am happy to teach you some of those skills. Type `learn` to see what I
      can teach you.")))

(defentity lodge-workshop lodge-room
  [:brief "Explorers' Lodge Workshop"
   :full "A long table fills the center of this room; various implements are
     strewn across its surface. Shelves along the walls hold all manner of tools
     and contraptions."
   :exits [:north lodge-library :east lodge-trophy-room]
   :contents [explorers-guildmaster]])

;;; library

(defproto library-room room
  [:domain :indoor
   :surface :stone
   :exit-proto exits/doorway])

(defentity library-main library-room
  [:brief "Library"
   :full "This large, circular room encompasses the entire ground floor of a
     squat, stone tower. The walls are lined with shelves full of leather-bound
     books."
   :exits [:east village-square-w :down library-basement :up library-study]])

(defentity library-basement library-room
  [:brief "Basement"
   :full "The basement of the library is full of books, scrolls, and other
     scholarly items. Everything is neatly stacked and organized, if a little
     dusty."
   :exits [:up library-main]])


;; FIXME: Not sure yet how I want to deal with these spells. Are they skills on
;; their own, or folded with multiple spells into each skill? Also, have an
;; everyday-magic prerequisite skill that teaches a number of spells like light,
;; etc?

(defentity magic-missile skill
  [:brief "magic missile"
   :full "Gathers pure energy into an arrow-like missile that shoots from your
     outstretched fingertips, striking your target for arcane damage."
   :karma 5
   :damage-type :arcane
   :base-damage 4
   :energy-cost 6])

(defentity heavy-limbs skill
  [:brief "heavy limbs"
   :full "Magically burdens the limbs of your target, slowing their actions for
     the next 15 seconds."
   :karma 5
   :energy-cost 4])

(defentity mage-guildmaster npc
  [:brief "Milena Landeris"
   :pose "sits at a nearby desk, perusing a scroll."
   :full "Milena is pleasant-looking young woman. Her clothing is casual and
     practical. Her long black hair is carefully plaited. She seems somewhat
     bored with the scroll in her hands, and her eyes often wander to the
     nearest window."
   :teaches-skills [#'heavy-limbs #'magic-missile]]

  (do-talk [this actor npc]
    (show-say actor this "Hello! I'm glad for the interruption. I am supposed to
      be studying this scroll, but the mating rituals of the southern
      blue-tailed cockatrice are really not my cup of tea.

      I am the local representative of the College of Arcanists, a guild for
      those who wish to study magic. I am just an apprentice myself, but I can
      teach you some basic skills; type `learn` to see what I can teach. If you
      are new in town, you might want to type `help skills` to learn more about
      skills in general.")))

(defentity library-study library-room
  [:brief "Study"
   :full "This circular room commands an excellent view of Arwyck through a
     number of large, glass-paned windows. A desk and padded leather chair have
     been placed beneath each window."
   :exits [:down library-main]
   :contents [mage-guildmaster]])

;;; east-road

(defproto east-road-room room
  [:brief "East Road"
   :domain :outdoor
   :surface :dirt])

(defentity east-road-1 east-road-room
  [:exits [:west village-square-e :east east-road-2
           :south (exits/entry-doorway mace-shop)]])

(defentity east-road-2 east-road-room
  [:exits [:west east-road-1 :east east-road-3 :south muggers-alley-1]])

(defentity east-road-3 east-road-room
  [:exits [:west east-road-2 :east east-gate
           :north (exits/entry-doorway temple-sanctuary)]])

;; mace shop

(defentity maury npc
  [:brief "Maury na Munigan"
   :pose "stands behind the counter, arms crossed."
   :full "Maury is a burly man with a full beard and little hair left atop his
     head. He looks strong enough to crack a few skulls without the aid of the
     weapons he sells."
   :sells [weapons/pine-club weapons/poplar-club weapons/birch-club weapons/maple-club]]

  (do-talk [this actor target]
    (show-say actor this "Welcome to my shop, ~a. You'll find my wares to be of
      better quality than most." (describe-brief @(:race actor) :art nil))))

(defentity mace-shop room
  [:brief "Maury's Maces"
   :full "This shop sells maces, clubs, and similar weapons meant for cracking
     skulls."
   :domain :indoor
   :surface :wood
   :exits [:north (exits/exit-doorway east-road-1)]
   :contents [maury]])

;;; temple

(defentity old-priest npc
  [:brief "an aged priest"
   :pose "kneels before the altar."
   :full "The priest is unkempt and dirty. His robes are in desperate need of
     repair, and his scraggly beard is in dire need of a trim. You can see
     intelligence and compassion in his striking blue eyes, but also a touch of
     madness."]

  (do-talk [this actor (target this)]
    (show-say actor this "Be welcome in this temple, friend. My flock is few in
      number these days. As the gods abandoned the people, so too did the people
      abandon the gods. I do not blame them. As for myself, I cannot turn my
      back on a lifetime of belief. Not yet.

      It began slowly at first, nearly a century ago. The gods had always lent
      their power to their faithful servants. Members of the priesthood wielded
      magic as powerful as that of any mage.

      But then our powers began to fail. One by one, the gods stopped granting
      us their blessings. Did they simply turn away from us? Did they go
      elsewhere? Perhaps they simply ceased to exist?

      It is heresy to think such things. But now it has been thirty years since
      the last of them disappeared. My order no longer wields any power, and we
      can do little to help the people we once protected. Perhaps we no longer
      deserve the gods' aid and guidance? I do not know, but I will continue my
      search for answers.

      The absence of the gods leaves a great void. My only hope is that evil and
      darkness do not fill it.")))

(defentity temple-sanctuary room
  [:brief "Sanctuary"
   :full "This large room was once a place where people gathered to worship the
     gods, but it has clearly not been used in many years. A large altar stands
     near one end of the room and various statues reside in niches along the
     walls. Dust and grime cover every surface; puddles on the floor mark the
     spots where the wooden roof has begun to fail. The tapestries that once
     lined the walls have fallen into ruin."
   :domain :indoor
   :surface :stone
   :exits [:south (exits/exit-doorway east-road-3)]
   :contents [old-priest]])

;;; east-gate

(defentity east-gate room
  [:brief "East Gate"
   :domain :outdoor
   :surface :stone
   :exits [:west east-road-3]])

;;; muggers-alley

(defproto muggers-alley-room room
  [:brief "Muggers' Alley"
   :domain :outdoor
   :surface :dirt])

(defentity muggers-alley-1 muggers-alley-room
  [:exits [:north east-road-2 :south muggers-alley-2]])

(defentity muggers-alley-2 muggers-alley-room
  [:exits [:north muggers-alley-1 :south muggers-alley-3 :west dagger-shop]])

(defentity muggers-alley-3 muggers-alley-room
  [:exits [:north muggers-alley-2 :south wall-street-6
           :east (exits/doorway warehouse-anteroom)]])

;;; dagger shop

(defentity dagger-vendor npc
  [:brief "Jimmy the Hare"
   :pose "cleans his fingernails with a silver knife."
   :full "Jimmy is a greasy-haired young man whose eyes dart around nervously,
     as if he expects someone to leap from the shadows at any moment."
   :sells [weapons/copper-dagger weapons/bronze-dagger weapons/brass-dagger]]

  (do-talk [this actor target]
    (show-text actor "Jimmy looks around warily.")
    (show-say actor this "Hey stranger, keep yer distance. If ya be needin' a
      dagger, yer in the right place. Pick one ya like an' leave yer silver on
      the table on yer way out.")))

(defentity dagger-shop room
  [:brief "Shivs n' Such"
   :full "This shop sells all manner of knives and daggers."
   :domain :indoor
   :surface :wood
   :exits [:east (exits/exit-doorway muggers-alley-2)]
   :contents [dagger-vendor]])

;;; warehouse

(defproto warehouse-room room
  [:domain :indoor
   :surface :wood
   :exit-proto exits/doorway])

(defentity warehouse-anteroom warehouse-room
  [:brief "Anteroom"
   :full "This cramped room has stained walls and an uneven wooden floor. The
     air is heavy with smoke."
   :exits [:west (exits/exit-doorway muggers-alley-3)
           :east warehouse-storeroom :up warehouse-office]])

(defentity stealth skill
  [:brief "stealth"
   :full "Gives you a chance to move around unnoticed."
   :karma 5])

(defentity vagabond-guildmaster npc
  [:brief "Shady Roger"
   :pose "sits in a chair with his feet on the desk."
   :full "Roger is a scrawny man with three days' stubble. His gaze is
     constantly scanning the room, as if he expects an enemy to appear at any
     moment. He wears well-worn leather garments and a bandolier of knives
     across his chest."
   :teaches-skills [#'stealth]]

  (do-talk [this actor npc]
    (show-say actor this "I see you found our secret lair. Well, not so secret,
      really; if it were we'd have no recruits!

      I am a representative of the Grey Hand. My brothers and sisters work to
      achieve our goals through subtlety, misdirection, and the occasional knife
      in the back. Brute force is for suckers! As for magic, well...not everyone
      likes to spend their days reading musty old scrolls. I'd rather be out in
      the streets, where the action is!

      I can teach you some of the basic skills favored by my guild. Type `learn`
      to see what you can learn from me. You might also want to type `help
      skills` to learn more about skills in general.")))

(defentity warehouse-office warehouse-room
  [:brief "Office"
   :full "A huge mahogany desk dominates this small room. Shelves along the
     walls are packed with numerous scrolls, ledgers, and other documents."
   :exits [:down warehouse-anteroom]
   :contents [vagabond-guildmaster]])

(defentity warehouse-storeroom warehouse-room
  [:brief "Storeroom"
   :full "This large room has a high ceiling supported by heavy beams. All
     shapes and sizes of crates, barrels, and boxes are stacked on the floor."
   :exits [:west warehouse-anteroom :east warehouse-meeting-room]])

(defentity warehouse-meeting-room warehouse-room
  [:brief "Meeting Room"
   :exits [:west warehouse-storeroom]])

;;; south-road

(defproto south-road-room room
  [:brief "South Road"
   :domain :outdoor
   :surface :dirt])

(defentity south-road-1 south-road-room
  [:exits [:north village-square-s :south south-road-2]])

(defentity south-road-2 south-road-room
  [:exits [:north south-road-1 :south wall-street-3]])

;;; wall-street

(defproto wall-street-room room
  [:brief "Wall Street"
   :full "This narrow paved street follows the low wall that marks the southern
     boundary of the village."
   :domain :outdoor
   :surface :stone])

(defentity wall-street-1 wall-street-room
  [:exits [:east wall-street-2]])

(defentity wall-street-2 wall-street-room
  [:exits [:west wall-street-1 :east wall-street-3]])

(defentity wall-street-3 wall-street-room
  [:exits [:west wall-street-2 :east wall-street-4
           :north south-road-2 :south south-gate]])

(defentity wall-street-4 wall-street-room
  [:exits [:west wall-street-3 :east wall-street-5]])

(defentity wall-street-5 wall-street-room
  [:exits [:west wall-street-4 :east wall-street-6]])

(defentity wall-street-6 wall-street-room
  [:exits [:west wall-street-5 :north muggers-alley-3]])

;;; south-gate

(defentity south-gate room
  [:brief "South Gate"
   :surface :stone
   :domain :outdoor
   :exits [:north wall-street-3]])

;;; forest-road

(defproto forest-road-room room
  [:brief "Forest Road"
   :full "This narrow dirt road runs between the village and a small
     forest to the west."
   :domain :outdoor
   :surface :dirt])

(defentity forest-road-1 forest-road-room
  [:exits [:east village-square-sw :west forest-road-2]])

(defentity forest-road-2 forest-road-room
  [:exits [:east forest-road-1 :west forest-road-3]])

(defentity forest-road-3 forest-road-room
  [:exits [:east forest-road-2 :west forest-gate :north grove-sw]])

;;; grove

(defproto grove-room room
  [:brief "Secluded Grove"
   :full "This beautiful grove of aspens and birches seems unnaturally quiet,
     especially given the proximity of the village."
   :domain :outdoor
   :surface :forest])

(defentity grove-sw grove-room
  [:exits [:south forest-road-3 :north grove-nw :east grove-se]])

(defentity druid-guildmaster npc
  [:brief "Raspin Redleaf"
   :pose "tends to a nearby tree."
   :full "Raspin is a wiry, rugged-looking man who wears plain brown clothes
     well suited to travel in the wilderness. His wide leather belt holds
     numerous small pouches as well as a pair of long, curved daggers."
   :teaches-skills []]

  (do-talk [this actor npc]
    (show-say actor this "Welcome, friend. I am Raspin, a representative of the
      Circle of the Grove.

      My guild teaches skills that draw upon the power of nature to heal,
      sustain, and--when needed--destroy. Type `learn` to see what I can teach
      you, or type `help skills` for more general information.")))

(defentity grove-nw grove-room
  [:exits [:south grove-sw :east grove-ne]
   :contents [druid-guildmaster]])

(defentity grove-ne grove-room
  [:exits [:west grove-nw :south grove-se]])

(defentity grove-se grove-room
  [:exits [:north grove-ne :west grove-sw]])

;;; forest-gate

(defentity find-my-son quest
  [:brief "Find My Son"
   :full "Find Miranda's son, who disappeared with his friend while picking
     berries in the Silverwood."
   :level 1])

(defentity miranda npc
  [:brief "Miranda Mathers"
   :full "Miranda is a stout middle-aged woman. She wears practical clothing
     that has seen numerous repairs over the years."
   :begins-quests [find-my-son]]

  (do-begin-quest [this actor (quest find-my-son) npc]
    (show-say actor this "Oh, thank the gods! I have nobody else to turn to.
      Will you help me?

      My son went into the woods west of here with his friend to pick some
      berries. He has been there dozens of times without any problems, but today
      he never returned. I'm so worried!

      Please...find my boy. I don't know what I'd do if I lost him."))

  (do-advise-quest [this actor (quest find-my-son) npc]
    (show-say actor this "Have you found my boy?"))

  (do-finish-quest [this actor (quest find-my-son) npc]
    (show-say actor this "Oh, thank you...thank you!")))

(defentity forest-gate room
  [:brief "Forest Gate"
   :domain :outdoor
   :surface :dirt
   :exits [:east forest-road-3 :west world.silverwood/forest-6-3]
   :contents [miranda]])
|#
