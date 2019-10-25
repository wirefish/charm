(in-package :isle-of-dawn)

(defregion isle-of-dawn
  (name "Isle of Dawn")
  (full "The souls of ancient heroes are reborn atop a sacred hill in the middle
    of this small island. The magical energies of the place allow a soul to
    regain its physical form so it may join the battle against the great evil
    that threatens Atalea.")
  (climate :temperate))

;;; TODO: define region

(defproto isle-location (location)
  (domain :outdoor)
  (surface :grass))

;;; hilltop

(deflocation hilltop (isle-location)
  (brief "Hilltop")
  (full "You are standing atop a low hill in the center of a small island. A
    ring of ancient standing stones surrounds you.")
  (visible (("standing stones"
             "You count nine stones, each about 12 feet tall and four feet wide.
             The faint remains of ornate tracery are barely visible upon their
             weathered surfaces.")))
  (tutorial "Welcome to Atalea! As a new player, you will sometimes see green
    text like this. These messages provide tips to help you get started. For
    details, type `help tutorial`.

    A few basics: to move around the world, type the name of the direction you
    want to go. For example, type `south` or `s` to move south. To look at your
    surroundings, type `look`. To look at something specific, add its name or
    description. For example, try `look self`.

    Head `south` to begin your adventure!")
  (exits ((gravel-path :south pavilion))))

(setf charm::*new-avatar-location* hilltop)

;;; pavilion

(defproto spirit-warden (npc)
  (brief "the spirit warden")
  (pose "stands nearby, smiling amiably.")
  (full "The spirit warden is an elderly human man, standing well over six feet
    tall. His long white hair and wispy beard frame a wrinkled, grinning face.
    He wears dark blue robes with gold embroidered trim."))

(defmethod did-enter-location ((observer spirit-warden) (actor avatar) location entry)
  (with-delay (3)
    (when (same-location-p actor observer)
      (if (eq (race actor) lib::reborn-hero)
          (show-text actor "The spirit warden beckons to you.")
          (show-text actor "The spirit warden gives you a friendly nod.")))))

(defmethod do-talk (actor (target spirit-warden) subject)
  (declare (ignore subject))
  (show-say actor target "Welcome to Atalea, hero! Or perhaps I should say
    \"welcome back...\"

    I don't know how to explain this, but you died long ago. For reasons
    unknown, you have been recalled from the Dreamlands and once again walk in
    the physical world.

    My colleagues stand ready to help you get oriented after your long absence.
    As you explore this isle, talk to anyone you meet; we are all here to help.

    To the south you will meet an odd creature who will get you started. Head
    that way when you are ready."))

(deflocation pavilion (isle-location)
  (brief "Pavilion")
  (full "An open-air pavilion stands a few feet from the path. Its silk canopy
    is painted in bright colors.")
  (tutorial "You will often encounter creatures with whom you can interact. They
    may provide useful information or offer rewards if you perform actions on
    their behalf. For example, type `talk warden` to talk to the spirit warden.
    He may have something interesting to say.")
  (exits ((gravel-path :north hilltop :south wildflower-field)))
  (contents (spirit-warden)))

;;; wildflower-field

(defquest choose-a-race
  (name "Let's Get Physical")
  (summary "Choose your physical form by meditating at one of the racial shrines
    on the Isle of Dawn."))

(defmethod do-offer-quest (avatar (quest (eql choose-a-race)) npc)
  (show-say avatar npc "Hello, friend! As you may have noticed, your current
    body is just a ghostly manifestation of your spirit. To fix that you'll need
    to select a race and take on a physical form. It just so happens I can help
    you do exactly that!"))

(defmethod do-offer-quest :after (avatar (quest (eql choose-a-race)) npc)
  (maybe-show-tutorial avatar 'offer-quest "The kobold is offering you a quest!
    Quests are tasks set for you by the denizens of the world. Completing them
    can provide you with many kinds of rewards. Type `help quests` for more
    information."))

(defmethod do-accept-quest (avatar (quest (eql choose-a-race)) npc)
  (show-say avatar npc "Excellent! To the west you will find several shrines,
    each dedicated to a different race. Talk to the caretaker at each shrine to
    learn more about his or her people.

    Then, when you find the race that's right for you, return to the selected
    shrine and `meditate`. The caretaker will, ahem, take care of the rest.

    Return to me once you have completed this task.")
  (maybe-show-tutorial avatar 'accept-quest "You can use the `quest` command to
    see the quests you've accepted and track your progress toward their
    completion."))

(defmethod do-advise-quest (avatar (quest (eql choose-a-race)) npc)
  (show-say avatar npc "Have you selected a race yet? The caretakers to the west
    will be more than happy to describe their races and help you make the right
    decision."))

(defmethod do-finish-quest (avatar (quest (eql choose-a-race)) npc)
  (show-say avatar npc "A fine choice! Indeed I had no doubt you would choose to
    become ~a. I am something of an expert in these matters, after all.

    I can't help but notice that your new body, while quite lovely, is also
    quite naked. Aren't you cold? To the south you will find my friend Dhalia,
    the seamstress; talk to her and she will make sure you go forth in
    style." (describe-brief (race avatar))))

(defproto officious-kobold (npc)
  (brief "an officious kobold")
  (pose "sits at a low table in the shade of a large umbrella.")
  (full "The kobold is a tiny humanoid with reptilian features, sparse wiry
    hair, and knobbly gray skin. It is, however, impeccably groomed.")
  (icon 'kobold)
  (begins-quests '(choose-a-race))
  (ends-quests '(choose-a-race)))

(defmethod do-talk (actor (target officious-kobold) subject)
  (declare (ignore subject))
  (if (>= (level actor) 1)
    (show-say actor target "Hello! You might have heard that some of my kind
      have occupied an old copper mine, deep in the forest west of Arwyck. I
      assure you I have nothing whatsoever to do with those filthy creatures,
      despite our shared ancestry.")
    (show-say actor target "Good to see you again! How has that body been
      treating you?")))

(deflocation wildflower-field (isle-location)
  (brief "Field of Wildflowers")
  (full "Flowers of every color and description bloom in the expansive fields
    along the sides of the path. The air is heavy with their fragrance.")

  (tutorial "The &#9829; symbol means that a creature wants to talk to you about
    a quest. The symbol also appears on the map to help you find nearby quests.

    When the symbol is blue, a new quest is available: `talk` to the creature to
    learn more. When the symbol is silver, you have started but not yet
    completed a quest. When it is gold, you have completed a quest and can
    `talk` to the creature to receive your rewards!")
  (visible (("low table"
             "The table looks sturdy but is otherwise unremarkable.")
            ("umbrella"
             "The umbrella is decorated with green and yellow stripes.")))
  (exits ((gravel-path :north pavilion :south clothing-stall :west human-shrine)))
  (contents (officious-kobold)))

;;; human-shrine

(defproto human-caretaker (npc)
  (brief "the human caretaker")
  (pose "stands nearby with a welcoming expression.")
  (full "The caretaker is a tall, athletic woman wearing a practical leather
    outfit and a broad-brimmed hat. Various gardening tools hang from her wide
    leather belt."))

(defmethod do-talk (actor (target human-caretaker) subject)
  (declare (ignore subject))
  (show-say actor target "Hello, traveler. I imagine you are curious about
    humans. Here is what I can tell you.

    First and foremost, humans are known for their optimism and their
    adaptability. We feel like we can do anything, and do it well. Perhaps
    you've heard the expression, \"Jack of all trades, master of none?\" Some
    might consider it a negative, but we humans take pride in our ability to do
    a little bit of everything.

    Second, humans are especially noted for their skill as gatherers and
    farmers. We are adept at using our ingenuity to extract the bounty of the
    earth.

    If you want to join humankind, `meditate` here. I will then use the power of
    the shrine to complete your transformation."))

(deflocation human-shrine (isle-location)
  (brief "Shrine of Humanity")
  (full "This part of the isle is a large garden. Plants bearing flowers,
    fruits, and vegetables are arranged in orderly rows. The soil is dark and
    fertile.")
  (exits ((gravel-path :east wildflower-field :north elven-shrine :west ogre-shrine)))
  (contents (human-caretaker)))

(defmethod did-meditate ((observer human-shrine) actor)
  (if (quest-incomplete-p actor choose-a-race)
      (progn
        (show-text actor "A calming warmth suffuses your being. The caretaker
          smiles broadly as she reaches out to you with her open hand, holding
          it inches from your ghostly form. She smiles broadly as the warmth
          spreads to her hand.

          After a moment she closes her palm. You see a faint glow between her
          fingers which quickly grows brighter. When she opens her hand she
          holds a tiny seedling, its delicate leaves unfolding before your
          eyes.")
        (change-race actor lib::human)
        (advance-quest actor choose-a-race))
      (show-text actor "The caretaker nods in approval.")))

;;; elven-shrine

(defproto wooden-sculpture (entity)
  (brief "a wooden sculpture")
  (pose "stands in the middle of the ring of trees.")
  (full "The sculpture is carved from polished yellow wood. Its form is fluid
    and abstract but somehow evokes images of towering forest oaks and hidden
    woodland dells. Golden-brown moss grows in chaotic yet precise patterns
    along its sides. Atop the sculpture rests a shallow bowl made of green
    glass. The bowl contains clear, cold water."))

(defproto elven-caretaker (npc)
  (brief "the elven caretaker")
  (pose "kneels beside one of the trees, her eyes closed.")
  (full "The elven caretaker is a graceful young woman with striking emerald
    eyes and long, silver hair arranged in complex braids. She wears a pale
    green dress with embroidered trim."))

(defmethod do-talk (actor (target elven-caretaker) subject)
  (declare (ignore subject))
  (show-say actor target "Greetings, stranger. Be welcome in this place. No
    doubt you have come here to learn something about my people. I will tell you
    what I can.

    Mine are a peaceful, thoughtful people who love and respect nature. This was
    not always so; millenia ago we nearly destroyed ourselves with our prideful,
    warlike ways. It took a terrible crisis for my ancestors to change. I pray
    that we never slide back into those habits that nearly left us extinct.

    Elves are quick-footed and quick-witted, although we are not as strong as
    many of the other races of Atalea. We excel as hunters, magicians, bards,
    and--it pains me to say--as thieves. We consider ourselves custodians of the
    forest and take that charge very seriously. We do not stand by idly if our
    beloved home is threatened.

    If you would join my folk, simply `meditate` here to make your purpose
    known; the shrine's power will do the rest."))

(deflocation elven-shrine (isle-location)
  (brief "Shrine of the Forest")
  (full "A low wooden fence surrounds a perfect ring of thirteen graceful birch
    trees. The area radiates an aura of tranquility.")
  (surface :forest)
  (exits ((gravel-path :south human-shrine :north sidhe-shrine :west goblin-shrine)))
  (contents (wooden-sculpture elven-caretaker)))

(defmethod did-meditate ((observer elven-shrine) actor)
  (if (quest-incomplete-p actor choose-a-race)
      (progn
        (show-text actor "The caretaker approaches the nearby sculpture and
          raises her hands to the sky. Motes of light rise from the bowl atop
          the sculpture; they quickly become so bright you are forced to look
          away. A swirling wind rises and the trees begin to sway, their leaves
          glimmering in the light. Soon the motes move toward you, surrounding
          you with their light and warmth.

          After a few moments the wind dies down and the motes dissolve into
          tiny sparkling flecks which quickly disperse in the dying breeze. The
          clearing is calm once again.")
        (change-race actor lib::elf)
        (advance-quest actor choose-a-race))
      (show-text actor "The caretaker bows respectfully.")))

;;; sidhe-shrine

(defproto sidhe-caretaker (npc)
  (brief "the sidhe caretaker")
  (pose "stands nearby, lost in thought.")
  (full "The sidhe caretaker is a tall, gaunt man with a stern look. His long
    white hair is pulled back into a ponytail that nearly reaches the ground. He
    wears dark robes of a rather elaborate and archaic style."))

(defmethod do-talk (actor (target sidhe-caretaker) subject)
  (declare (ignore subject))
  (show-say actor target "Well, well. Another hero, reborn. I suppose I should
    tell you something of my race, although I find it unlikely you were
    fortunate enough to be a member of the Fair Folk in your past life.

    For uncounted millenia, my people lived in an alternate plane of existence
    called the Shadowlands. A few short centuries ago, war with demons drove us
    out of our homeland. We ended up here.

    At our core, we sidhe are creatures of magic. It is our very essence. Few
    sidhe have become famous warriors or minstrels, although of course it is
    possible. But all of our people have some small skill in magic, and many
    have become wizards of reknown.

    I cannot say it seems likely, but if you believe you are truly one of the us
    then `meditate` before the shrine. If you are found worthy, I will welcome
    you into the fold."))

(deflocation sidhe-shrine (isle-location)
  (brief "Shrine of Shadows")
  (full "A lone tree grows atop an enormous granite boulder. Its gnarled, gray
    roots wrap around the stone before sinking into the fertile earth below. The
    tree's twisted branches splay outward and its dense leaves form a solid
    canopy that condemns this part of the isle to perpetual shadow.")
  (visible (("tree"
             "The tree is clearly very old. You get a strange sense that it does
             not belong in this world.")
            ("boulder"
             "Upon closer inspection, you notice that the entire surface of the
             boulder is covered with tiny rune-like markings.")))
  (surface :forest)
  (exits ((gravel-path :south elven-shrine :west dwarven-shrine)))
  (contents (sidhe-caretaker)))

(defmethod did-meditate ((observer sidhe-shrine) actor)
  (if (quest-incomplete-p actor choose-a-race)
      (progn
        (show-text actor "As you begin your meditation, the shadows in the area
          deepen. The leaves above begin to rustle, as if the tree is growing
          restless.

          You vision begins to blur. Shifting figures appear, ghosts of
          creatures from another world. They call to you in an alien language
          you have never heard.

          After a moment, you begin to understand their words. You answer their
          calls, but the meaning of your words is lost as soon as they are
          uttered. Apparently satisfied, the figures disappear; the shadows
          retreat.")
        (change-race actor lib::sidhe)
        (advance-quest actor choose-a-race))
      (show-text actor "The caretaker arches an eyebrow.")))

;;; dwarven-shrine

(defproto dwarven-caretaker (npc)
  (brief "the dwarven caretaker")
  (pose "stands proudly amid the statues.")
  (full "The dwarven caretaker stands about four feet tall, with shoulders
    nearly as wide. He wears full chain mail, even in the heat of the day. His
    armor appears to be of fine workmanship. A heavy maul hangs from his belt.
    His long, red beard falls across a belly that has seen a few pints of ale in
    its day."))

(defmethod do-talk (actor (target dwarven-caretaker) subject)
  (declare (ignore subject))
  (show-say actor target "Well met! If it's a dwarf ye wanna be, then ye be at
    tha right place. I kinna imagine wantin' ta be anythin' else!

    Me people may be wee, but we have tha strength of tha mountains within us!
    Aye, it's true, we have a great love for gold and gems. But we have nae
    great love for tha dark beasties that lurk below tha earth. We dwarves are
    great crafters and even greater at fightin', when tha times require it.

    If ye think that all sounds good, then get ta meditatin'!"))

(deflocation dwarven-shrine (isle-location)
  (brief "Shrine of the Mountain")
  (full "Rows of stone statues stand in the tall grass, each depicting a stout
    warrior with a grim expression and glowing amber eyes. The statues are
    arrayed in formation like soldiers marching to battle.")
  (exits ((gravel-path :east sidhe-shrine :south goblin-shrine)))
  (contents (dwarven-caretaker)))

(defmethod did-meditate ((observer dwarven-shrine) actor)
  (if (quest-incomplete-p actor choose-a-race)
      (progn
        (show-text actor "The caretaker grasps his maul with both hands and
          holds it out before him. After a moment the hammer begins to vibrate
          and erupts with amber light, echoing the statues' eyes. The vibrations
          grow stronger and emanate outward from the caretaker's body. Soon the
          entire area is shaking and the statues begin to rock back and forth.

          A powerful voice intones, \"You have chosen. So be it. Strong as
          stone, bright as steel. May you bring honor to clan and king.\"

          Without warning the shaking stops and the light dissipates.")
        (change-race actor lib::dwarf)
        (advance-quest actor choose-a-race))
      (show-text actor "The caretaker joins you in silent reflection.")))

;;; goblin-shrine

(defproto chessboard (entity)
  (brief "a stone chessboard")
  (pose "rests atop a low table.")
  (full "The chessboard is remarkable for its pieces: each is a finely detailed
    and garishly-painted rendition of a creature who looks much like the
    caretaker. The pieces are so realistic they seem almost alive."))

(defproto goblin-caretaker (npc)
  (brief "the goblin caretaker")
  (pose "stands beside the chessboard, apparently pondering his next move.")
  (icon :goblin)
  (full "The caretaker is a short, big-eared creature with greenish-blue skin.
    In contrast with his somewhat comical proportions and garish attire, his
    large dark eyes evince a keen intellect."))

(defmethod do-talk (actor (target goblin-caretaker) subject)
  (declare (ignore subject))
  (show-say actor target "Hello, friend! I can't tell you how happy I am to see
    your interest in goblinkind. We are a misunderstood folk! Let me set the
    record straight.

    Goblins are small in stature but we have big dreams. Mostly dreams of
    wealth, it's true, but there's nothing wrong with a little spending cash,
    eh?

    Some larger folks will tell you we are thieves, beggars, and worse. That is
    so mostly untrue! The majority of us would rather get rich through shrewd
    business practices and hard bargaining than by using less savory methods.

    When those ignorant louts take offense at our methods, it certainly helps
    that we're quick and stealthy by nature. You won't see many goblins waving
    huge swords around, but as they say, \"Size isn't everything!\"

    If you feel like you're one of us at heart, why not become one of us in
    body, too? Just `meditate` here and the deed will be done."))

(deflocation goblin-shrine (isle-location)
  (brief "Shrine of Fortune")
  (full "The ground here has been cleared, leveled, and surfaced with
    multi-colored bricks.")
  (surface :stone)
  (exits ((gravel-path :north dwarven-shrine :south ogre-shrine :east elven-shrine)))
  (contents (chessboard goblin-caretaker)))

(defmethod did-meditate ((observer goblin-caretaker) actor)
  (if (quest-incomplete-p actor choose-a-race)
      (progn
        (show-text actor "As soon as you close your eyes, you have a vision of
          the world around you getting larger...or are you getting smaller? For
          a moment you are stricken with vertigo as the world shifts and spins.

          Once the dizziness passes, you open your eyes to find that your
          stature and greenish skin now match those of the caretaker.")
        (change-race actor lib::goblin)
        (advance-quest actor choose-a-race)
        (show-say actor observer "Huzzah! Welcome to the family!"))
      (show-text actor "The caretaker leaves you to your meditation.")))

;;; ogre-shrine

(defproto ogre-caretaker (npc)
  (brief "the ogre caretaker")
  (pose "is here, casually pulverizing rocks with his hammer.")
  (icon 'ogre)
  (full "The caretaker is a hulking figure, standing over eight feel tall. He
    wears an iron helm, iron gauntlets, and a breechclout. He carries an
    enormous iron sledgehammer."))

(defmethod do-talk (actor (target ogre-caretaker) subject)
  (declare (ignore subject))
  (show-say actor target "Ugh. I be ogre of few words, but I know what you want.
    I tell you of my kin. We big. We strong. Done!

    You came to `meditate`? Ha. If you wanna be ogre, do it. Done!"))

(deflocation ogre-shrine (isle-location)
  (brief "Shrine of Strength")
  (full "This area is choked with a haphazard collection of rough stone blocks,
    many overgrown with vines and twisting grasses. The blocks' surfaces have
    been crudely painted with scenes of carnage featuring enormous warriors
    laying waste to their tiny assailants.")
  (surface :rocks)
  (exits ((gravel-path :north goblin-shrine :east human-shrine)))
  (contents (ogre-caretaker)))

(defmethod did-meditate ((observer ogre-shrine) actor)
  (if (quest-incomplete-p actor choose-a-race)
      (progn
        (show-text actor "The caretaker selects a boulder and tosses it on the
          ground before you. He then steps over, raises his hammer, and crushes
          the rock as he lets out an enormous belly laugh.

          Rocks fly around. Muscles grow. Brain shrink.")
        (change-race actor lib::ogre)
        (advance-quest actor choose-a-race))
      (show-text actor "The caretaker chuckles and crushes another boulder.")))

;;; clothing-stall

(defproto simple-shirt (clothing-torso)
  (brief "a simple cotton shirt")
  (icon 'common-clothing)
  (full "The shirt is well-made and comfortable."))

(defproto simple-pants (clothing-legs)
  (brief "a pair of cotton pants")
  (full "The pants are made of thick brown cloth."))

(defproto leather-shoes (clothing-feet)
  (brief "a pair of leather shoes")
  (icon 'shoes)
  (full "These low shoes have a sturdy sole, suitable for walking long
    distances."))

(defproto small-backpack (equipment container) ; FIXME: lib::small-backpack etc.
  (brief "a small canvas backpack")
  (icon 'bag-small)
  (full "The backpack has wide leather straps and a drawstring closure.")
  (slot :backpack)
  (capacity 20))

(defquest get-some-clothes
  (name "Cover Up")
  (summary "Pick a white tulip for Dhalia, then exchange it for a set of clothes.")
  (required-quests '(choose-a-race)))

(defmethod do-offer-quest (avatar (quest (eql get-some-clothes)) npc)
  (show-say avatar npc "Greetings! Did our mutual kobold friend send you my way?
    It certainly seems you have need of my wares. I will happily provide you
    with an outfit that should serve your needs, but I must ask a favor in
    return."))

(defmethod do-accept-quest (avatar (quest (eql get-some-clothes)) npc)
  (show-say avatar npc "Wonderful! You see, I am very fond of white tulips, but
    my work here prevents me from taking the time to gather them. Would you head
    to the east and get one for me?"))

(defmethod do-advise-quest (avatar (quest (eql get-some-clothes)) npc)
  (show-say avatar npc "Have you found a white tulip for me? They're just to the
    east."))

(defmethod do-finish-quest (avatar (quest (eql get-some-clothes)) npc)
  (show-say avatar npc "Why thank you, this tulip is lovely! Your timing is
    perfect; I have just finished selecting an outfit for you.")
  (dolist (item '(simple-shirt simple-pants leather-shoes small-backpack))
    (give-item npc (make-instance item) avatar))
  (maybe-show-tutorial avatar 'first-items "The seamstress have given you
    several items; type `inventory` or `inv` to list them.

    Right now you are carrying the items in your hands. To wear an item, use the
    `equip` command. For example, `equip shirt`. Type just `equip` to list the
    items you currently have equipped.

    In addition to clothing, the seamstress also gave you a backpack. Once you
    equip it, items you receive will automatically be placed into the backpack
    instead of staying in your hands. Note that you can't wield weapons unless
    your hands are empty.

    For more information type `help equip` or `help inventory`."))

(defproto white-tulip (quest-item)
  (brief "a white tulip")
  (pose "draws your attention.")
  (icon 'white-tulip)
  (full "The tulip is quite lovely; you can see why Dhalia prizes them.")
  (unique 1)
  (quest 'get-some-clothes)
  (entry-pose "catches your eye.")
  (respawn-delay 5))

(defmethod do-take-item :after (actor quantity (item white-tulip) origin)
  (advance-quest actor get-some-clothes))

(defproto seamstress (npc)
  (brief "Dhalia")
  (pose "stands in the stall, organizing her wares.")
  (full "Dhalia is a human woman of indeterminate age. She wears silver-rimmed
    spectacles and an impeccably-tailored dress decorated with a floral
    pattern.")
  (begins-quests '(get-some-clothes))
  (ends-quests '(get-some-clothes)))

(deflocation clothing-stall (isle-location)
  (brief "Clothing Stall")
  (full "A wooden market stall has been erected beside the path. Its counter is
    piled with basic clothing items in myriad styles and sizes.")
  (exits ((gravel-path :north wildflower-field :south fountain-plaza :east tulip-field-sw)))
  (contents (seamstress)))

;;; tulip-field

(defproto tulip-field-portal (portal)
  (brief "the tulip field")
  (pose "continues to the ~a."))

(defproto tulip-field (isle-location)
  (brief "Field of Tulips")
  (full "Tulips in myriad colors have been planted here.")
  (surface :flowers))

(deflocation tulip-field-sw (tulip-field)
  (exits ((gravel-path :west clothing-stall)
          (tulip-field-portal :east tulip-field-se :north tulip-field-nw)))
  (contents (white-tulip)))

(defmethod did-enter-location :after ((observer tulip-field-sw) (actor avatar) location entry)
  (when (quest-incomplete-p actor get-some-clothes)
    (maybe-show-tutorial actor 'take-items "Some items can be picked up using
      the `take` command. For example, type `take tulip` to take a white tulip
      when you see one. Picking up an item places it into your inventory. Use
      the `inventory` command (or just `inv` for short) to list the items you
      are carrying.")))

(deflocation tulip-field-se (tulip-field)
  (exits ((tulip-field-portal :west tulip-field-sw :north tulip-field-ne)))
  (contents (white-tulip)))

(deflocation tulip-field-nw (tulip-field)
  (exits ((tulip-field-portal :south tulip-field-sw :east tulip-field-ne)))
  (contents (white-tulip)))

(deflocation tulip-field-ne (tulip-field)
  (exits ((tulip-field-portal :west tulip-field-nw :south tulip-field-se)))
  (contents (white-tulip)))

;;; fountain-plaza

(defproto fountain (entity)
  (brief "a stone fountain")
  (pose "stands in the middle of the plaza.")
  (full "This large fountain depicts a circle of men and women, holding hands
    and dancing around a central pillar. Water bursts forth from atop the
    pillar, creating a delightful sound as it lands at the figures' feet."))

(defquest choose-a-gender
  (name "He or She?")
  (summary "Select your gender by meditating at one of the shrines near the
    Fountain Plaza.")
  (required-quests '(get-some-clothes)))

(defmethod do-offer-quest (avatar (quest (eql choose-a-gender)) npc)
  (show-say avatar npc "Another new arrival! Good to see you. We're certain to
    need more heroes of your caliber before long. Right now, though, it's high
    time you chose a gender. I assume you remember the difference between males
    and females from your previous life?"))

(defmethod do-accept-quest (avatar (quest (eql choose-a-gender)) npc)
  (show-say avatar npc "To the east and west you will find statues that
    symbolize the two sexes. Simply `meditate` near the statue that represents
    your preferred gender. Come back to me when you're done."))

(defmethod do-advise-quest (avatar (quest (eql choose-a-gender)) npc)
  (show-say avatar npc "Did you find the statues? They're hard to miss!"))

(defmethod do-finish-quest (avatar (quest (eql choose-a-gender)) npc)
  (show-say avatar npc "You did it! I must admit I'm somewhat surprised by
    your choice. No matter; I'm sure I'll get used to it. Eventually."))

(defproto cherub (npc)
  (brief "a cherub")
  (pose "hovers nearby, its wings flapping madly.")
  (full "The cherub is a small, chubby creature with white feathered wings.
    Although its stature and harmless aspect are those of a child, its dark and
    penetrating eyes make it clear this is a creature both ancient and wise.")
  (begins-quests '(choose-a-gender))
  (ends-quests '(choose-a-gender)))

(defmethod do-talk (actor (target cherub) subject)
  (declare (ignore subject))
  (show-say actor target "Hello there!"))

(deflocation fountain-plaza (isle-location)
  (brief "Fountain Plaza")
  (full "This hexagonal plaza is paved with multi-colored stone tiles.")
  (surface :stone)
  (exits ((gravel-path :north clothing-stall :south circle-of-names
                       :west male-shrine :east female-shrine)))
  (contents (fountain cherub)))

;;; male-shrine

(defproto male-statue (entity)
  (brief "a large marble statue")
  (pose "stands in the center of the plaza.")
  (full "The statue stands over twelve feet tall. Its form subtly shifts as you
    view it, taking on properties of different races and embodying various
    masculine virtues. It somehow reflects your own personal concept of the
    perfect male."))

(deflocation male-shrine (isle-location)
  (brief "Shrine of Masculinity")
  (full "Low shrubs, cut in complex geometric patterns, surround a plaza
    covered with white gravel.")
  (exits ((gravel-path :east fountain-plaza)))
  (contents (male-statue)))

(defmethod did-meditate ((observer male-shrine) actor)
  (if (quest-incomplete-p actor choose-a-gender)
    (progn
      (show-text actor "The statue's mouth begins to move and a deep voice
        intones several archaic words you don't understand.")
      (change-gender actor :male)
      (advance-quest actor choose-a-gender))
    (show-text actor "You feel no more clarity than you did prior to meditation.")))

;;; female-shrine

(defproto female-statue (entity)
  (brief "a large jade statue")
  (pose "stands in the center of the area.")
  (full "The statue stands over twelve feet tall. Its eyes seem to follow you.
    As you consider the statue, your thoughts wander; its features change in
    your mind's eye to reflect your personal ideal of femininity."))

(deflocation female-shrine (isle-location)
  (brief "Shrine of Femininity")
  (full "A multitude of flowering vines have been trained to ornate trellises
    that surround a bowl-shaped area covered with a carpet of blossoming
    clover.")
  (exits ((gravel-path :west fountain-plaza)))
  (contents (female-statue)))

(defmethod did-meditate ((observer female-shrine) actor)
  (if (quest-incomplete-p actor choose-a-gender)
    (progn
      (show-text actor "The statue slowly turns its head to face you and says
        several arcane words in a powerful, mellifluous voice.")
      (change-gender actor :female)
      (advance-quest actor choose-a-gender))
    (show-text actor "You feel no more clarity than you did prior to meditation.")))

;;; circle-of-names

(defproto tree-of-names (entity)
  (brief "a beautiful tree")
  (pose "grows in the center of the lawn.")
  (icon :tree)
  (full "The tree has smooth, silvery bark and broad leaves of the deepest
    green. Thousands of names have been written in luminous ink upon its trunk,
    all using the same flowing script."))

(defquest choose-a-name
  (name "Nameless No More")
  (summary "Choose your name by saying it in the presence of the orb of naming.")
  (required-quests '(choose-a-gender)))

(defmethod do-offer-quest (avatar (quest (eql choose-a-name)) npc)
  (show-say avatar npc "Oh, hello. I didn't see you standing there. I hope you
    haven't been waiting long. How can I help you?

    Ah! You need a name, don't you. You've come to the right place; I'd be happy
    to help you out."))

(defmethod do-accept-quest (avatar (quest (eql choose-a-name)) npc)
  (show-say avatar npc "Just to the east you'll see a magical orb. Find it
    and say the word. Literally! Stand next to the orb and `say` the word you
    want to have as your name. The orb's power is truly remarkable.

    Once your anonymity has been cured, come back to me so I can record your new
    name on the trunk of this tree with the names of all the other heroes who
    have passed this way."))

(defmethod do-advise-quest (avatar (quest (eql choose-a-name)) npc)
  (show-say avatar npc "Still going incognito? You'll find the orb of naming
    just to the east."))

(defmethod do-finish-quest (avatar (quest (eql choose-a-name)) npc)
  (show-say avatar npc "Yes, what is it? Of course, you've chosen your name!
    Let me see, where is my pen? Ah, there it is. And now to write your
    name...how did you spell it, again? Just a few strokes of the pen,
    and...done!

    ~a is a fine name. Wear it proudly." (describe-brief avatar)))

(defproto mistress-of-names (npc)
  (brief "the mistress of names")
  (pose "stands beneath the tree.")
  (full "The mistress of names is a short, slender woman of indeterminate age.
    Her long auburn hair is bound in a loose ponytail. She wears a pair of
    horn-rimmed spectacles and her clothing is rumpled and ink-stained.")
  (begins-quests '(choose-a-name))
  (ends-quests '(choose-a-name)))

(defmethod do-talk (actor (target mistress-of-names) subject)
  (declare (ignore subject))
  (if (name actor)
      (show-say actor target "Ah, ~a. I just love the sound of your name!
        ~{~c~^-~}... It simply rolls off the tongue."
                (name actor)
                (map 'list #'(lambda (c) (char-upcase c)) (name actor)))
      (show-text actor "The mistress of names frowns at you over her glasses.")))

(deflocation circle-of-names (isle-location)
  (brief "Circle of Names")
  (full "You stand within a wide circle of well-tended lawn surrounded by a low
    stone wall.")
  (exits ((gravel-path :north fountain-plaza :east clifftop :south guard-station)))
  (contents (tree-of-names mistress-of-names)))

;;; clifftop

(defproto orb-of-naming (entity)
  (brief "the orb of naming")
  (pose "hovers a few feet above the ground.")
  (icon :sphere)
  (full "The orb is a spherical stone about two feet in diameter. Its surface is
    smooth and cloudy. If a creature speaks a word within its presence, the orb
    has the power to make that word the creature's name."))

(deflocation clifftop (isle-location)
  (brief "Windy Clifftop")
  (full "You stand atop a rocky cliff that falls perhaps a hundred feet to a
    narrow beach. The wind is brisk and smells of the sea.")
  (tutorial "To speak, use the `say` command. For example, to say \"hello\", you
    could type `say hello`. Everyone (and everything!) in your location will
    hear what you say.

    In this room, the `look:orb of naming` is always listening, so be careful
    what you say! If you speak a single word that the orb deems suitable, that
    word will become your name. Choose wisely.")
  (surface :stone)
  (exits ((gravel-path :west circle-of-names)))
  (contents (orb-of-naming)))

(defmethod did-say ((observer orb-of-naming) actor message)
  (when (quest-incomplete-p actor choose-a-name)
    (if (and (= (length message) 1)
             (valid-name-p (first message)))
        (progn
          (show-text actor "The orb begins to glow, dimly at first, but then
            more brightly. Sparks skitter across its smooth surface and you feel
            an uncomfortable tingle beneath your skin.")
          (change-name actor (string-capitalize (first message)))
          (advance-quest actor choose-a-name))
        (show-text actor "The orb glows dimly for a moment, but nothing happens."))))

;;; guard-station

(defproto worn-dagger (dagger)
  (brief "a worn copper dagger")
  (full "The dagger has a slender blade and a smooth wooden handle."))

(defquest kill-some-plants
  (name "Weed Control")
  (summary "Prove your worth to the guard by killing a vineling. Lashleaf?
    Whatever.")
  (required-quests '(choose-a-name)))

(defmethod do-offer-quest (avatar (quest (eql kill-some-plants)) npc)
  (show-say avatar npc "Greetings, ~a. It seems you're nearly ready to leave
    this place, but I have my doubts. Before I allow you to venture further, I'm
    going to teach you how to fight! You'll need to be able to handle a weapon
    if you want to survive in the real world." (name avatar)))

(defmethod do-accept-quest (avatar (quest (eql kill-some-plants)) npc)
  (show-say avatar npc "West of here you'll find some...plants. Not normal
    plants, but vicious killers! Vinelings, I think they're called. Or maybe
    lashleaves? Whatever. The name's not important.

    I have a dagger for you. Here, let me show you how to use it.")
  (give-item npc (make-instance 'worn-dagger) avatar)
  (show-text avatar "The guard begins to demonstrate some basic dagger
      techniques.")
  (with-delay (2)
    (learn-skill avatar dagger-proficiency npc)
    (show-say avatar npc "You're a natural! Now equip that dagger and go kill
      one of those plants. Strike fast and true! If you can overcome such a
      fearsome foe, I'll happily let you pass.")))

(defmethod do-advise-quest (avatar (quest (eql kill-some-plants)) npc)
  (show-say avatar npc "Any progress so far? Kill one of those plant things and
    we'll talk."))

(defmethod do-finish-quest (avatar (quest (eql kill-some-plants)) npc)
  (show-say avatar npc "Great job! I'll confess, those things give me the heebie
    jeebies. Plants shouldn't writhe around like that. Please, feel free to head
    south whenever you like."))

(defproto guard (npc)
  (brief "a burly guard")
  (pose "stands nearby.")
  (icon 'human-male)
  (begins-quests '(kill-some-plants))
  (ends-quests '(kill-some-plants)))

(deflocation guard-station (isle-location)
  (brief "Guard Station")
  (full "A small guard post stands alongside the path.")
  (tutorial "In your adventures you will have the chance to learn many different
    skills. Each skill improves your ability to fight, use magic, craft items,
    and more.

    The guard here has a quest that will teach you your first skill: the basics
    of hand-to-hand combat! Talk to him to get started. You can also type `help
    skills` to learn more about skills in general.")
  (exits ((gravel-path :north circle-of-names :west overgrown-field-se)
          (iron-gate :south cobbled-square)))
  (contents (guard)))

(defmethod can-exit-location ((observer guard) (actor avatar) (location guard-station) exit)
  (if (and (eq (direction exit) :south)
           (not (quest-finished-p actor kill-some-plants)))
      (progn
        (show-text actor "The guard refuses to let you go that way.")
        (show-say actor observer "Stop right there, friend. You need to perform
          the tasks set for you by my comrades to the north before I will let
          you pass.")
        nil)
      (progn
        (show-text actor "The guard nods respectfully as you leave.")
        t)))

;;; overgrown-field

(defproto overgrown-field-portal (portal)
  (brief "the field")
  (pose "continues to the ~a."))

(defproto overgrown-field (isle-location)
  (brief "Overgrown Field")
  (full "Tangled vines and weeds make it difficult to move through this area.")
  (surface :weeds))

(defproto lashling-tendril (natural-weapon)
  (brief "a thorny tendril")
  (damage-type :slashing)
  (attack-verb "whips"))

(defproto lashling (monster)
  (brief "a lashling")
  (pose "flails its tendrils in a menacing display.")
  (full "The lashling is a small mass of writhing vines and weeds that has
    somehow gained the ability to move, albeit very slowly. Sharp thorns
    protrude from the ends of its leafy, tentacle-like appendages.")
  (entry-pose "emerges from beneath the weeds.")
  (level 0)
  (attacks (lashling-tendril))
  (respawn-delay 30))

(defmethod do-kill :after ((actor avatar) (target lashling))
  (advance-quest actor kill-some-plants))

(deflocation overgrown-field-sw (overgrown-field)
  (exits ((overgrown-field-portal :east overgrown-field-se :north overgrown-field-nw)))
  (contents (lashling)))

(deflocation overgrown-field-se (overgrown-field)
  (tutorial "To begin attacking a lashling, type `attack lashling`. You will
    automatically perform basic attacks with the weapon in your main hand.
    Combat ends when you or your opponent is dead!

    Depending on the skills you choose to learn as you explore the world, you
    will gain access to a variety of special attacks and defenses that you can
    use during combat.")
  (exits ((overgrown-field-portal :west overgrown-field-sw :north overgrown-field-ne)
          (gravel-path :east guard-station)))
  (contents (lashling)))

(deflocation overgrown-field-nw (overgrown-field)
  (exits ((overgrown-field-portal :south overgrown-field-sw :east overgrown-field-ne)))
  (contents (lashling)))

(deflocation overgrown-field-ne (overgrown-field)
  (exits ((overgrown-field-portal :west overgrown-field-nw :south overgrown-field-se)))
  (contents (lashling)))

;;; cobbled-square

(deflocation cobbled-square (isle-location)
  (brief "Cobbled Square")
  (full "The fresh smell of the sea pleasantly fills this small seaside plaza.")
  (tutorial "Completing quests has granted you `help:experience`, shown by the
    purple bar near the top of your screen. You have already gained enough
    experience to advance from level 0 to level 1. Your level measures your
    overall power.

    Each time you gain a level, you also gain `help:karma`. You can spend karma
    to learn `help:skills`, which grant you benefits and access to new
    abilities. As you travel the world you will find people who can teach you
    all sorts of skills.")
  (surface :stone)
  (exits ((iron-gate :north guard-station)
          (sandy-path :west beach-east)
          (cobbled-lane :east pier)
          (entry-doorway :south dockmaster-shack))))

;;; pier

(defproto dock-sign (entity)
  (brief "an informative sign")
  (pose "is posted here.")
  (icon :sign)
  (full "The sign indicates that a ship frequently arrives here to carry
    passengers across the sea to the village of Arwyck."))

(defproto gangplank (portal)
  (brief "a gangplank"))

(deflocation pier (isle-location)
  (brief "Sturdy Pier")
  (full "This stone pier juts out into the sea, giving ships that visit the
    isle a safe place to dock.")
  (tutorial "To board a ship for Arwyck, wait until it arrives and then move
    `east`. Once the ship reaches its destination, move `south` to disembark.")
  (surface :stone)
  (exits ((cobbled-lane :west cobbled-square)))
  (contents (dock-sign)))

;;; the-siren

(defproto sailor (npc)
  (brief "a grizzled sailor")
  (full "The sailor has a short, salt-and-pepper beard and wears a faded
    uniform."))

(defmethod do-talk (actor (target sailor) subject)
  (declare (ignore subject))
  (show-say actor target "Welcome aboard the *Siren*! Every few moments we make
    the short run between Arwyck and the Isle of Dawn. Relax and enjoy the
    trip!"))

(deflocation the-siren (location)
  (brief "The *Siren*")
  (pose "is docked at the pier.")
  (full "This sturdy single-masted vessel makes frequent trips between the Isle
    of Dawn and the village of Arwyck on the mainland.")
  (icon :ship)
  (domain :outdoor)
  (surface :wood)
  (surrounding :shallow-water)
  (contents (sailor)))

(defmethod do-enter-world :after ((actor the-siren) location entry)
  (start-behavior actor :move-the-siren #'lib::move-vehicle
                  '((gangplank :west pier)
                    (gangplank :south arwyck::west-dock))))

;;; dockmaster-shack

(defproto dockmaster (npc)
  (brief "the dockmaster")
  (pose "sits behind the desk.")
  (full "The dockmaster is a grizzled man with a short salt-and-pepper beard.
    His left eye is covered with a leather patch, but his right eye harbors a
    dangerous gleam."))

(deflocation dockmaster-shack (isle-location)
  (brief "Dockmaster's Shack")
  (full "This one-room structure is dominated by a large desk that is piled with
    documents and empty wine bottles.")
  (visible (("documents"
             "You discreetly scan the papers on top of the pile. They appear to
             be schedules, manifests, and other such shipping-related
             documents.")
            ("wine bottles"
             "You may have been dead for hundreds of years, but wine hasn't
             changed all that much; you still recognize the cheap stuff when you
             see it.")))
  (domain :indoor)
  (surface :wood)
  (exits ((exit-doorway :north cobbled-square)
          (stairway :down dockmaster-basement)))
  (contents (dockmaster)))

;;; dockmaster-basement

(deflocation dockmaster-basement (isle-location)
  (brief "Basement of the Dockmaster's Shack")
  (full "This low, damp space is more of a crawlspace than a basement. Several
    barrels and crates have been pushed into one corner of the dirt floor.")
  (domain :indoor)
  (surface :dirt)
  (exits ((stairway :up dockmaster-shack))))

;;; beach

(defproto shiny-seashell (stackable-item)
  (brief "a shiny seashell")
  (icon :seashell)
  (full "The seashell's polished surface is covered with an intricate pattern
    of white and orange whorls.")
  (alts ("a shiny shell"))
  (respawn-delay '(300 600)))

(defproto beach-portal (portal)
  (brief "the beach")
  (pose "continues ~a."))

(defproto beach-location (isle-location)
  (brief "Rocky Beach")
  (full "The sand on this narrow beach is full of pebbles and shell
    fragments.")
  (domain :outdoor)
  (surface :sand))

(deflocation beach-east (beach-location)
  (exits ((beach-portal :west beach-center)
          (sandy-path :east cobbled-square))))

(deflocation beach-center (beach-location)
  (exits ((beach-portal :west beach-west :east beach-east))))

(deflocation beach-west (beach-location)
  (exits ((beach-portal :east beach-center)))
  (contents (shiny-seashell)))
