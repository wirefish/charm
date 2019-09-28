(defsystem "charm"
  :description "charm: a multiuser text-based role-playing game server"
  :version "0.0.1"
  :author "Craig Becker <craig@wirefish.com>"
  :license "MIT"
  :depends-on ("cl-ppcre" "cl-async" "cl-base64" "babel" "ironclad" "sqlite")
  :serial t
  :components
  ((:module "src" :components
            ((:file "packages")

             (:file "utility")
             (:file "buffer")
             (:file "queue")
             (:file "random")
             (:file "logging")
             (:file "json")
             (:file "encode")
             (:file "text")
             (:file "http")
             (:file "websocket")

             (:file "describe")
             (:file "match")

             (:file "session")
             (:file "show")
             (:file "command")

             (:file "entity")
             (:file "container")
             (:file "location")
             (:file "item")
             (:file "combatant")
             (:file "creature")
             (:file "corpse")
             (:file "equipment")
             (:file "skill")
             (:file "avatar")
             (:file "region")
             (:file "event")
             (:file "state-machine")

             (:file "config")
             (:file "help")
             (:file "tutorial")
             (:file "map")
             (:file "accept")
             (:file "quest")
             (:file "inspect")
             (:file "skills")
             (:file "inventory")
             (:file "meditate")
             (:file "move")
             (:file "talk")
             (:file "combat")
             (:file "status")
             (:file "admin")

             (:file "database")
             (:file "server")))

   (:module "world" :components
            ((:module "lib" :components
                      ((:file "races")
                       (:file "locations")
                       (:file "portals")
                       (:file "weapons")
                       (:file "vehicle")))
             (:file "arwyck")
             (:file "copper-mine")
             (:file "dripping-caverns")
             (:file "isle-of-dawn")
             (:file "silverwood")))))
