(in-package :charm)

(defun maybe-show-tutorial (avatar key message)
  (when (and (tutorials-on avatar)
             (not (gethash key (tutorials-seen avatar))))
    (show-tutorial avatar message)
    (setf (gethash key (tutorials-seen avatar)) t)))

(defcommand (actor "tutorial" subcommand)
  "Use this command to control which tutorial text you see. Some rooms have
  associated tutorial text that you see the first time you enter. This text is
  used to introduce new players to game concepts and commands.

  The command has the following subcommands:

  - `tutorial on` enables tutorial text. This is the default.

  - `tutorial off` disables tutorial text.

  - `tutorial reset` clears the set of tutorials you've already seen, so you
    will see them again.

  - `tutorial` with no subcommand displays the tutorial for the current room, if
    it has one."
  (cond
    ((null subcommand)
     (if (tutorial (location actor))
         (show-tutorial actor (tutorial (location actor)))
         (show-text actor "There is no tutorial for this location.")))
    ((string-equal (first subcommand) "on")
     (progn
       (setf (tutorials-on actor) t)
       (show-text actor "Tutorials are now enabled.")))
    ((string-equal (first subcommand) "off")
     (progn
       (setf (tutorials-on actor) nil)
       (show-text actor "Tutorials are now disabled.")))
    ((string-equal (first subcommand) "reset")
     (progn
       (clrhash (tutorials-seen actor))
       (show-text actor "Tutorials have been reset.")))
    (t
     (show-text actor "Unknown subcommand ~s." (first subcommand)))))
