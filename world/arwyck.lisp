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

;;; harbor-road

(defproto harbor-road-location (location)
  (brief "Harbor Road")
  (full "A rutted dirt road runs between the harbor and the village proper.")
  (domain :outdoor)
  (surface :dirt))

(defentity harbor-road-1 (harbor-road-location)
  (exits ((dirt-road :north bayside-plaza-3 :south harbor-road-2))))

(defentity harbor-road-2 (harbor-road-location)
  (exits ((dirt-road :north harbor-road-1 :south village-square-n))))

;;; village-square

(defproto village-square-location (location)
  (brief "Village Square")
  (full "This cobbled plaza is the heart of the village. Various
     shops line its perimeter.")
  (domain :outdoor)
  (surface :stone))

(defproto village-square-portal (portal)
  (brief "the square")
  (pose "continues to the ~a."))

(defproto shaggy-dog (creature state-machine)
  (brief "a shaggy white dog")
  (pose "is nearby.")
  (icon "sniffing-dog.png")
  (full "The dog is very curious and closely watches anyone who passes by. Its
    fur is matted and dirty but it seems happy.")
  (initial-state :stretch))

(defmethod do-change-state ((actor shaggy-dog) (state (eql :stretch)))
  (dolist (observer (contents (location actor)))
    (show-text observer "The shaggy dog gets up and stretches."))
  (with-delay (5)
    (change-state actor :move)))

(defmethod do-change-state ((actor shaggy-dog) (state (eql :move)))
  (let ((exit (random-elt (remove-if-not #'(lambda (x)
                                             (typep x 'village-square-portal))
                                         (exits (location actor))))))
    (traverse-portal actor exit)
    (with-delay (5)
      (change-state actor :sleep))))

(defmethod do-change-state ((actor shaggy-dog) (state (eql :sleep)))
  (dolist (observer (contents (location actor)))
    (show-text observer "The shaggy dog lies down."))
  (with-delay (20)
    (change-state actor :stretch)))

(defmethod do-talk (actor (target shaggy-dog) subject)
  (declare (ignore subject))
  (show-text actor "The dog's ears perk up and it tilts its head to the side."))

(defentity village-square-nw (village-square-location)
  (exits ((village-square-portal :east village-square-n :south village-square-w)
          (entry-doorway :north armory-training-hall :west sword-shop)))
  (contents (shaggy-dog)))

(defentity village-square-n (village-square-location)
  (exits ((village-square-portal :west village-square-nw :east village-square-ne
                                 :south village-square-c)
          (dirt-road :north harbor-road-2))))

(defentity village-square-ne (village-square-location)
  (exits ((village-square-portal :west village-square-n :south village-square-e)
          (entry-doorway :east inn-common-room))))

(defentity village-square-w (village-square-location)
  (exits ((village-square-portal :north village-square-nw :south village-square-sw
                                 :east village-square-c)
          (entry-doorway :west library-main))))

(defproto podium (entity)
  (brief "a low podium")
  (pose "stands in the center of the village square.")
  (full "The podium appears solidly-built. A few steps lead up to its wide
    platform. When you speak from atop the podium, your words will be heard
    throughout the village square."))

(defentity village-square-c (village-square-location)
  (exits ((village-square-portal :west village-square-w :east village-square-e
                                 :north village-square-n :south village-square-s)))
  (contents (podium)))

(defentity village-square-e (village-square-location)
  (exits ((village-square-portal :north village-square-ne :south village-square-se
                                 :west village-square-c)
          (dirt-road :east east-road-1))))

(defentity village-square-sw (village-square-location)
  (exits ((village-square-portal :north village-square-w :east village-square-s)
          (dirt-road :west forest-road-1)
          (entry-doorway :south lodge-foyer))))

(defentity village-square-s (village-square-location)
  (exits ((village-square-portal :west village-square-sw :east village-square-se
                                 :north village-square-c)
          (dirt-road :south south-road-1))))

(defproto crone (npc)
  (brief "an old crone")
  (pose "stands off to the side, watching people pass by.")
  (full "The crone wears a faded gray cloak. Her wavy white hair spills from
    beneath her hood. Her eyes are bright, and she watches any passersby with
    great interest."))

(defmethod did-enter-location ((observer crone) (actor avatar) location entry)
  (show-text actor "The crone stares at you for a moment, then looks away."))

(defmethod do-talk (actor (target crone) subject)
  (declare (ignore subject))
  (show-say actor target "Greetings, stranger. Your new body may be young, but I
    can sense the age of your spirit. This is not your first time around the
    block, so to speak. In a past life you were a great hero, but perhaps you
    have forgotten your deeds. A pity.

    Know this: you are here for a reason. This world needs you; I feel it in my
    bones, I hear it on the wind...but I know not why. What threat could be so
    grave that, in order to overcome it, we must tear history's heroes away from
    their peaceful slumber in the Dreamlands?

    Of course that raises another question: who or what is doing the tearing?

    In the village of my birth there is a saying: \"May you live in interesting
    times.\" Bah. Perhaps I will be lucky, and my days will end before things
    become too interesting. For you, though, I foresee no such luck."))

(defentity village-square-se (village-square-location)
  (exits ((village-square-portal :west village-square-s :north village-square-e)))
  (contents (crone)))

;;; sword shop

(defproto bryndan (vendor)
  (brief "Bryndan O'Donnel")
  (pose "cleans a longsword with an oiled rag.")
  (full "Bryndan is a lanky man with freckled skin and gray eyes. His auburn
    hair hangs down to his shoulders.")
  (sells nil))

(defentity sword-shop (location)
  (brief "Bryndan's House of Blades")
  (full "A variety of bladed weapons are displayed in racks that fill this
    small shop.")
  (domain :indoor)
  (surface :wood)
  (exits ((exit-doorway :east village-square-nw)))
  (contents (bryndan)))

(defmethod do-talk (actor (target bryndan) subject)
  (declare (ignore subject))
  (show-say actor target "Welcome to my shop! I have a variety of bladed weapons
    for sale. Type `buy` to see what I have available."))

;;; inn

(defproto inn-room (location)
  (domain :indoor)
  (surface :wood))

(defproto barkeep (vendor)
  (brief "Sienna")
  (pose "is working behind the bar.")
  (full "Sienna is a tall woman with short yellow hair and light brown eyes. She
    keeps a close eye on her patrons.")
  (sells nil))

(defmethod do-talk (actor (target barkeep) subject)
  (declare (ignore subject))
  (show-say actor target "Welcome to the Golden Gryphon! I sell food and drink;
    I'd say it's the best in town but my mam told me never to lie, so I won't.
    If you're learning to cook, I can sell you a few recipes as well. Type `buy`
    to see what I have for sale."))

(defentity inn-common-room (inn-room)
  (brief "Common Room")
  (full "The common room of the Golden Gryphon is a cheerful place. A crackling
    fire burns in the stone hearth. Numerous tables and chairs give the space a
    crowded feel, even when few customers are present. A long oak bar lines one
    wall.")
  (exits ((exit-doorway :west village-square-ne :north inn-kitchen)
          (stairway :up inn-upstairs-hall)))
  (contents (barkeep)))

(defproto cooking-guildmaster (trainer)
  (brief "Dully the Cook")
  (pose "is stirring a pot of...something.")
  (full "Dully is a portly dwarven male with a scraggly beard. He wears a
    grease-stained apron and a strange, poofy hat. He wields a huge wooden spoon
    as if it were a weapon."))

(defmethod did-enter-location ((observer cooking-guildmaster) (actor avatar) location entry)
  (show-say actor observer "Aye, it's a good day for a stew, ain't it?"))

(defmethod do-talk (actor (target cooking-guildmaster) subject)
  (declare (ignore subject))
  (show-say actor target "Aye, if ye be lookin' ta learn ta cook, I be yer
    dwarf. Type `learn` ta see wha I can teach ya, or `help skills` ta learn
    more about skills an such."))

(defproto slice-of-rye (item)
  (brief "a slice[s] of rye bread")
  (pose "rests on a nearby tabletop.")
  (icon "sliced-bread.png")
  (full "This bread seems a little stale but still adequate for, say, making a
    sandwich."))

(defmethod do-take-item :after (actor (item slice-of-rye) origin)
  (with-delay (30)
    (do-enter-location (make-instance 'slice-of-rye) origin nil)))

(defentity inn-kitchen (inn-room)
  (brief "Kitchen")
  (full "This large and somewhat disorganized kitchen smells faintly of ale and
    grease.")
  (exits ((doorway :south inn-common-room)))
  (contents (cooking-guildmaster slice-of-rye)))

(defentity inn-upstairs-hall (inn-room)
  (brief "Upstairs Hall")
  (full "This dark, windowless hall leads to the Golden Gryphon's guest rooms.")
  (exits ((stairway :down inn-common-room)
          (doorway :west guest-room-1 :east guest-room-2))))

(defentity guest-room-1 (inn-room)
  (brief "Small Guest Room")
  (full "This cramped room holds a small bed and an iron-bound chest. The
    window overlooks the village square.")
  (exits ((doorway :east inn-upstairs-hall))))

(defentity guest-room-2 (inn-room)
  (brief "Large Guest Room")
  (full "This room is tastefully furnished. The wide feather bed is lumpy but
    comfortable. An oak wardrobe features carvings of trees and forest
    creatures. The window provides a view of nearby rooftops and the rolling
    hills east of the village.")
  (exits ((doorway :west inn-upstairs-hall))))

;;; armory

(defproto armory-room (location)
  (domain :indoor)
  (surface :wood))

(defproto warrior-guildmaster (trainer)
  (brief "Andalya")
  (pose "is in the center of the room, idly swinging a slender longsword.")
  (full "Andalya is a lean, graying woman who wears loose clothing befitting
    one who teaches the martial arts."))

(defmethod do-talk (actor (target warrior-guildmaster) subject)
  (declare (ignore subject))
  (show-say actor target "Welcome, stranger. I am Andalya. It is my privilege to
    be the local representative of the Company of the Blade. We are a band
    dedicated to the practice of the martial arts. It is my job to recruit those
    who are interested in learning to defend themselves with good steel.

    Type `learn` to see the skills I can teach you. You can also type `help
    skills` to learn more about skills in general."))

(defentity armory-training-hall (armory-room)
  (brief "Training Hall")
  (full "This long hall has stone walls and an arched ceiling. The soft wooden
    floor is clean but marred from the many training matches that have been
    fought here.")
  (exits ((exit-doorway :south village-square-nw)))
  (contents (warrior-guildmaster)))

;;; explorers' lodge

(defproto lodge-room (location)
  (domain :indoor)
  (surface :wood))

(defproto quartermaster (vendor)
  (brief "the quartermaster")
  (pose "stands nearby.")
  (full "The quartermaster is a slender man with graying hair and elegant
    mustaches. His clothing, although impeccably clean, seems a little too big
    for him."))

(defmethod do-talk (actor (target quartermaster) subject)
  (declare (ignore subject))
  (show-say actor target "Good day to you."))

(defentity lodge-foyer (lodge-room)
  (brief "Explorers' Lodge Foyer")
  (full "This airy room has wood-paneled walls and a thick carpet on the
    floor.")
  (exits ((exit-doorway :north village-square-sw)
          (doorway :south lodge-trophy-room :west lodge-library)))
  (contents (quartermaster)))

(defproto stuffed-gorilla (entity)
  (brief "a stuffed gorilla")
  (pose "stands at the end of the room.")
  (icon "gorilla.png")
  (full "This intimidating creature stands over eight feet tall. Even in death
    its expression is fierce. A plaque mounted on the wall reads:

    > Although it slew several dogs and gravely wounded our scout, our party
    defeated this beast as we traversed the jungles of Phaa, CY 549."))

(defentity lodge-trophy-room (lodge-room)
  (brief "Explorers' Lodge Trophy Room")
  (full "The walls of this long room are lined with hunting prizes of all
    kinds: antlers, horns, mounted heads, and more.")
  (exits ((doorway :north lodge-foyer :west lodge-workshop)))
  (contents (stuffed-gorilla)))

(defproto lord-olmquist (npc)
  (brief "Lord Olmquist")
  (pose "sits in a chair, smoking a pipe.")
  (full "Olmquist is a rather stout, red-cheeked fellow with a bushy white beard
    and bald pate. Horn-rimmed spectacles perch upon his substantial nose. He
    wears a velvet smoking jacket, an open-necked white shirt, and dark silk
    trousers."))

(defmethod do-talk (actor (target lord-olmquist) subject)
  (declare (ignore subject))
  (show-say actor target "Why yes, be a dear and fetch me a bourbon, won't you?"))

(defentity lodge-library (lodge-room)
  (brief "Explorers' Lodge Library")
  (full "Two walls of this room are lined with shelves full of books. A number
    of comfortable armchairs and small tables fill the remainder of the space.")
  (exits ((doorway :east lodge-foyer :south lodge-workshop)))
  (contents (lord-olmquist)))

#| FIXME: skills!

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

|#

(defproto explorers-guildmaster (trainer)
  (brief "Naman Artani")
  (pose "is hunched over a strange device on the table.")
  (full "Naman is a wiry young man who wears a scarred leather apron over his
    practical attire. His curly auburn hair floats in a wild cloud around his
    head.")
  (teaches '(swimming climbing)))

(defmethod do-talk (actor (target explorers-guildmaster) subject)
  (declare (ignore subject))
  (show-say actor target "Welcome to the Explorer's Lodge. Members of our guild
    travel across Atalea in search of rarities: strange creatures, lost
    treasures, and ancient knowledge. As you might imagine, survival skills come
    in quite handy in this vocation!

    I am happy to teach you some of those skills. Type `learn` to see what I can
    teach you."))

(defentity lodge-workshop (lodge-room)
  (brief "Explorers' Lodge Workshop")
  (full "A long table fills the center of this room; various implements are
    strewn across its surface. Shelves along the walls hold all manner of tools
    and contraptions.")
  (exits ((doorway :north lodge-library :east lodge-trophy-room)))
  (contents (explorers-guildmaster)))

;;; library

(defproto library-room (location)
  (domain :indoor)
  (surface :stone))

(defentity library-main (library-room)
  (brief "Library")
  (full "This large, circular room encompasses the entire ground floor of a
    squat, stone tower. The walls are lined with shelves full of leather-bound
    books.")
  (exits ((exit-doorway :east village-square-w)
          (stairway :down library-basement :up library-study))))

(defentity library-basement (library-room)
  (brief "Basement")
  (full "The basement of the library is full of books, scrolls, and other
    scholarly items. Everything is neatly stacked and organized, if a little
    dusty.")
  (exits ((stairway :up library-main))))

;; FIXME: Not sure yet how I want to deal with these spells. Are they skills on
;; their own, or folded with multiple spells into each skill? Also, have an
;; everyday-magic prerequisite skill that teaches a number of spells like light,
;; etc?

#| FIXME: skills!
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
|#

(defproto mage-guildmaster (trainer)
  (brief "Milena Landeris")
  (pose "sits at a nearby desk, perusing a scroll.")
  (full "Milena is pleasant-looking young woman. Her clothing is casual and
    practical. Her long black hair is carefully plaited. She seems somewhat
    bored with the scroll in her hands, and her eyes often wander to the nearest
    window.")
  (teaches '(heavy-limbs magic-missile)))

(defmethod do-talk (actor (target mage-guildmaster) subject)
  (declare (ignore subject))
  (show-say actor target "Hello! I'm glad for the interruption. I am supposed to
    be studying this scroll, but the mating rituals of the southern blue-tailed
    cockatrice are really not my cup of tea.

    I am the local representative of the College of Arcanists, a guild for those
    who wish to study magic. I am just an apprentice myself, but I can teach you
    some basic skills; type `learn` to see what I can teach. If you are new in
    town, you might want to type `help skills` to learn more about skills in
    general."))

(defentity library-study (library-room)
  (brief "Study")
  (full "This circular room commands an excellent view of Arwyck through a
    number of large, glass-paned windows. A desk and padded leather chair have
    been placed beneath each window.")
  (exits ((stairway :down library-main)))
  (contents (mage-guildmaster)))

;;; east-road

(defproto east-road (location)
  (brief "East Road")
  (full "This broad dirt road connects the center of Arwyck to the village's
    eastern gate.")
  (domain :outdoor)
  (surface :dirt))

(defentity east-road-1 (east-road)
  (exits ((dirt-road :west village-square-e :east east-road-2)
          (entry-doorway :south mace-shop))))

(defentity east-road-2 (east-road)
  (exits ((dirt-road :west east-road-1 :east east-road-3)
          (alley :south muggers-alley-1))))

(defentity east-road-3 (east-road)
  (exits ((dirt-road :west east-road-2 :east east-gate)
          (entry-doorway :north temple-sanctuary))))

;; mace shop

(defproto maury (vendor)
  (brief "Maury na Munigan")
  (pose "stands behind the counter, arms crossed.")
  (full "Maury is a burly man with a full beard and little hair left atop his
    head. He looks strong enough to crack a few skulls without the aid of the
    weapons he sells."))
;; FIXME: (sells [weapons/pine-club weapons/poplar-club weapons/birch-club weapons/maple-club]]

(defmethod do-talk (actor (target maury) subject)
  (declare (ignore subject))
  (show-say actor target "Welcome to my shop, ~a. You'll find my wares to be of
    better quality than most." (describe-brief (race actor) :article nil)))

(defentity mace-shop (location)
  (brief "Maury's Maces")
  (full "This shop sells maces, clubs, and similar weapons meant for cracking
    skulls.")
  (domain :indoor)
  (surface :wood)
  (exits ((exit-doorway :north east-road-1)))
  (contents (maury)))

;;; temple

(defproto old-priest (npc)
  (brief "an aged priest")
  (pose "kneels before the altar.")
  (full "The priest is unkempt and dirty. His robes are in desperate need of
    repair, and his scraggly beard is in dire need of a trim. You can see
    intelligence and compassion in his striking blue eyes, but also a touch of
    madness."))

(defmethod do-talk (actor (target old-priest) subject)
  (declare (ignore subject))
  (show-say actor target "Be welcome in this temple, friend. My flock is few in
    number these days. As the gods abandoned the people, so too did the people
    abandon the gods. I do not blame them. As for myself, I cannot turn my back
    on a lifetime of belief. Not yet.

    It began slowly at first, nearly a century ago. The gods had always lent
    their power to their faithful servants. Members of the priesthood wielded
    magic as powerful as that of any mage.

    But then our powers began to fail. One by one, the gods stopped granting us
    their blessings. Did they simply turn away from us? Did they go elsewhere?
    Perhaps they simply ceased to exist?

    It is heresy to think such things. But now it has been thirty years since
    the last of them disappeared. My order no longer wields any power, and we
    can do little to help the people we once protected. Perhaps we no longer
    deserve the gods' aid and guidance? I do not know, but I will continue my
    search for answers.

    The absence of the gods leaves a great void. My only hope is that evil and
    darkness do not fill it."))

(defentity temple-sanctuary (location)
  (brief "Sanctuary")
  (full "This large room was once a place where people gathered to worship the
    gods, but it has clearly not been used in many years. A large altar stands
    near one end of the room and various statues reside in niches along the
    walls. Dust and grime cover every surface; puddles on the floor mark the
    spots where the wooden roof has begun to fail. The tapestries that once
    lined the walls have fallen into ruin.")
  (domain :indoor)
  (surface :stone)
  (exits ((exit-doorway :south east-road-3)))
  (contents (old-priest)))

;;; east-gate

(defentity east-gate (location)
  (brief "East Gate")
  (full "A squat stone gatehouse stands at the village's entrance. To the east,
    a road winds through marshes that stretch along the coast of Emerald Bay.")
  (domain :outdoor)
  (surface :stone)
  (exits ((dirt-road :west east-road-3))))

;;; muggers-alley

(defproto muggers-alley (location)
  (brief "Muggers' Alley")
  (domain :outdoor)
  (surface :dirt))

(defentity muggers-alley-1 (muggers-alley)
  (exits ((alley :north east-road-2 :south muggers-alley-2))))

(defentity muggers-alley-2 (muggers-alley)
  (exits ((alley :north muggers-alley-1 :south muggers-alley-3)
          (entry-doorway :west dagger-shop))))

(defentity muggers-alley-3 (muggers-alley)
  (exits ((alley :north muggers-alley-2 :south wall-street-6)
          (entry-doorway :east warehouse-anteroom))))

;;; dagger shop

(defproto dagger-vendor (vendor)
  (brief "Jimmy the Hare")
  (pose "cleans his fingernails with a silver knife.")
  (full "Jimmy is a greasy-haired young man whose eyes dart around nervously,
    as if he expects someone to leap from the shadows at any moment."))
;; FIXME: sells [weapons/copper-dagger weapons/bronze-dagger weapons/brass-dagger]]

(defmethod do-talk (actor (target dagger-vendor) subject)
  (declare (ignore subject))
  (show-text actor "Jimmy looks around warily.")
  (show-say actor target "Hey stranger, keep yer distance. If ya be needin' a
    dagger, yer in the right place. Pick one ya like an' leave yer silver on the
    table on yer way out."))

(defentity dagger-shop (location)
  (brief "Shivs n' Such")
  (full "This shop sells all manner of knives and daggers.")
  (domain :indoor)
  (surface :wood)
  (exits ((exit-doorway :east muggers-alley-2)))
  (contents (dagger-vendor)))

;;; warehouse

(defproto warehouse (location)
  (domain :indoor)
  (surface :wood))

(defentity warehouse-anteroom (warehouse)
  (brief "Anteroom")
  (full "This cramped room has stained walls and an uneven wooden floor. The
    air is heavy with smoke.")
  (exits ((exit-doorway :west muggers-alley-3)
          (doorway :east warehouse-storeroom)
          (stairway :up warehouse-office))))

#| FIXME: skills
(defentity stealth skill
  [:brief "stealth"
   :full "Gives you a chance to move around unnoticed."
   :karma 5])
|#

(defproto vagabond-guildmaster (trainer)
  (brief "Shady Roger")
  (pose "sits in a chair with his feet on the desk.")
  (full "Roger is a scrawny man with three days' stubble. His gaze is constantly
    scanning the room, as if he expects an enemy to appear at any moment. He
    wears well-worn leather garments and a bandolier of knives across his
    chest.")
  (teaches '(stealth)))

(defmethod do-talk (actor (target vagabond-guildmaster) subject)
  (declare (ignore subject))
  (show-say actor target "I see you found our secret lair. Well, not so secret,
    really; if it were we'd have no recruits!

    I am a representative of the Grey Hand. My brothers and sisters work to
    achieve our goals through subtlety, misdirection, and the occasional knife
    in the back. Brute force is for suckers! As for magic, well...not everyone
    likes to spend their days reading musty old scrolls. I'd rather be out in
    the streets, where the action is!

    I can teach you some of the basic skills favored by my guild. Type `learn`
    to see what you can learn from me. You might also want to type `help skills`
    to learn more about skills in general."))

(defentity warehouse-office (warehouse)
  (brief "Office")
  (full "A huge mahogany desk dominates this small room. Shelves along the
    walls are packed with numerous scrolls, ledgers, and other documents.")
  (exits ((stairway :down warehouse-anteroom)))
  (contents (vagabond-guildmaster)))

(defentity warehouse-storeroom (warehouse)
  (brief "Storeroom")
  (full "This large room has a high ceiling supported by heavy beams. All shapes
    and sizes of crates, barrels, and boxes are stacked on the floor.")
  (exits ((doorway :west warehouse-anteroom :east warehouse-meeting-room))))

(defentity warehouse-meeting-room (warehouse)
  (brief "Meeting Room")
  (full "This cramped room contains a long table and numerous chairs and
    stools.")
  (exits ((doorway :west warehouse-storeroom))))

;;; south-road

(defproto south-road (location)
  (brief "South Road")
  (full "This dirt road connects the village square with the south gate.")
  (domain :outdoor)
  (surface :dirt))

(defentity south-road-1 (south-road)
  (exits ((dirt-road :north village-square-s :south south-road-2))))

(defentity south-road-2 (south-road)
  (exits ((dirt-road :north south-road-1 :south wall-street-3))))

;;; wall-street

(defproto wall-street (location)
  (brief "Wall Street")
  (full "This narrow paved street follows the low wall that marks the southern
    boundary of the village.")
  (domain :outdoor)
  (surface :stone))

(defentity wall-street-1 (wall-street)
  (exits ((cobbled-road :east wall-street-2))))

(defentity wall-street-2 (wall-street)
  (exits ((cobbled-road :west wall-street-1 :east wall-street-3))))

(defentity wall-street-3 (wall-street)
  (exits ((cobbled-road :west wall-street-2 :east wall-street-4 :south south-gate)
          (dirt-road :north south-road-2))))

(defentity wall-street-4 (wall-street)
  (exits ((cobbled-road :west wall-street-3 :east wall-street-5))))

(defentity wall-street-5 (wall-street)
  (exits ((cobbled-road :west wall-street-4 :east wall-street-6))))

(defentity wall-street-6 (wall-street)
  (exits ((cobbled-road :west wall-street-5)
          (alley :north muggers-alley-3))))

;;; south-gate

(defentity south-gate (location)
  (brief "South Gate")
  (surface :stone)
  (domain :outdoor)
  (exits ((cobbled-road :north wall-street-3))))

;;; forest-road

(defproto forest-road (location)
  (brief "Forest Road")
  (full "This narrow dirt road runs between the village and a small
    forest to the west.")
  (domain :outdoor)
  (surface :dirt))

(defentity forest-road-1 (forest-road)
  (exits ((dirt-road :east village-square-sw :west forest-road-2))))

(defentity forest-road-2 (forest-road)
  (exits ((dirt-road :east forest-road-1 :west forest-road-3))))

(defentity forest-road-3 (forest-road)
  (exits ((dirt-road :east forest-road-2 :west forest-gate)
          (dirt-path :north grove-sw))))

;;; grove

(defproto grove (location)
  (brief "Secluded Grove")
  (full "This beautiful grove of aspens and birches seems unnaturally quiet,
    especially given the proximity of the village.")
  (domain :outdoor)
  (surface :forest))

(defentity grove-sw (grove)
  (exits ((dirt-path :south forest-road-3 :north grove-nw :east grove-se))))

(defproto druid-guildmaster (trainer)
  (brief "Raspin Redleaf")
  (pose "tends to a nearby tree.")
  (full "Raspin is a wiry, rugged-looking man who wears plain brown clothes well
    suited to travel in the wilderness. His wide leather belt holds numerous
    small pouches as well as a pair of long, curved daggers.")
  (teaches nil))

(defmethod do-talk (actor (target druid-guildmaster) subject)
  (declare (ignore subject))
  (show-say actor target "Welcome, friend. I am Raspin, a representative of the
    Circle of the Grove.

    My guild teaches skills that draw upon the power of nature to heal, sustain,
    and--when needed--destroy. Type `learn` to see what I can teach you, or type
    `help skills` for more general information."))

(defentity grove-nw (grove)
  (exits ((dirt-path :south grove-sw :east grove-ne)))
  (contents (druid-guildmaster)))

(defentity grove-ne (grove)
  (exits ((dirt-path :west grove-nw :south grove-se))))

(defentity grove-se (grove)
  (exits ((dirt-path :north grove-ne :west grove-sw))))

;;; forest-gate

(defquest find-my-son
  (name "Find My Son")
  (summary "Find Miranda's son, who disappeared with his friend while picking
    berries in the Silverwood.")
  (level 1))

(defproto miranda (npc)
  (brief "Miranda Mathers")
  (full "Miranda is a stout middle-aged woman. She wears practical clothing
    that has seen numerous repairs over the years.")
  (begins-quests '(find-my-son)))

(defmethod do-offer-quest (actor (quest (eql find-my-son)) npc)
  (show-say actor npc "I know we've never met, but I have nobody else to turn
    to. My son is missing. Will you help me?"))

(defmethod do-accept-quest (actor (quest (eql find-my-son)) npc)
  (show-say actor npc "Oh, thank the gods! My son went into the woods west of
    here with his friend to pick some berries. He has been there dozens of times
    without any problems, but today he never returned. I'm so worried!

    Please...find my boy. I don't know what I'd do if I lost him."))

(defmethod do-advise-quest (actor (quest (eql find-my-son)) npc)
  (show-say actor npc "Have you found my boy? He should be in the forest to the
    west."))

(defmethod do-finish-quest (actor (quest (eql find-my-son)) npc)
  (show-say actor npc "Oh, thank you...thank you!"))

(defentity forest-gate (location)
  (brief "Forest Gate")
  (full "A stout wooden stockade separates Arwyck from the forest to the west. A
    narrow gate allows access to the wilderness beyond.")
  (domain :outdoor)
  (surface :dirt)
  (exits ((dirt-road :east forest-road-3 :west silverwood::forest-6-3)))
  (contents (miranda)))
