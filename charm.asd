(defsystem "charm"
  :description "charm: a multiuser text-based role-playing game server"
  :version "0.0.1"
  :author "Craig Becker <craig@wirefish.com>"
  :license "MIT"
  :depends-on ("cl-ppcre" "cl-async" "cl-base64" "ironclad" "sqlite")
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
             (:file "behavior")
             (:file "container")
             (:file "region")
             (:file "location")
             (:file "item")
             (:file "combatant")
             (:file "creature")
             (:file "equipment")
             (:file "weapon")
             (:file "armor")
             (:file "skill")
             (:file "avatar")
             (:file "tool")
             (:file "event")
             (:file "aura")

             (:file "config")
             (:file "help")
             (:file "tutorial")
             (:file "calendar")
             (:file "map")
             (:file "accept")
             (:file "quest")
             (:file "inspect")
             (:file "skills")
             (:file "inventory")
             (:file "meditate")
             (:file "move")
             (:file "talk")
             (:file "gather")
             (:file "craft")
             (:file "vendor")
             (:file "combat")
             (:file "status")
             (:file "chat")
             (:file "admin")

             (:file "database")
             (:file "server")))

   (:module "world" :components
            ((:module "lib" :components
                      ((:file "races")
                       (:file "locations")
                       (:file "portals")
                       (:file "coins")
                       (:file "weapons")
                       (:file "armor")
                       (:file "vehicle")
                       (:file "mining")
                       (:file "smelting")))
             (:file "arwyck")
             (:file "copper-mine")
             (:file "dripping-caverns")
             (:file "isle-of-dawn")
             (:file "mistmarsh")
             (:file "silverwood")))))
