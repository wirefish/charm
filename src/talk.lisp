(in-package :charm)

;;; Talk to an NPC.

(defevent talk (actor npc subject)
  "Check for special cases, such as available or completed quests, before
  triggering a generic `do-talk` event."
  (let (quest)
    (cond
      ((setf quest (find-complete-quest actor npc))
       (finish-quest actor quest npc))
      ((setf quest (find-incomplete-quest actor npc))
       (advise-quest actor quest npc))
      ((setf quest (find-available-quest actor npc))
       (offer-quest actor quest npc))
      (t
       (do-talk actor npc subject)))))

(defmethod do-talk (actor (target entity) subject)
  (show-text actor "You cannot talk to ~a." (describe-brief target :article :definite)))

(defmethod do-talk (actor (target creature) subject)
  (show-text actor "~a has nothing to say to you." (describe-brief target :article :definite :capitalize t)))

(defcommand (actor "talk" "to" target "about" subject)
  "Talk to a nearby creature. You may optionally specify a particular subject of
  interest."
  (let ((matches (match-objects actor target (remove actor (contents (location actor))))))
    (case (length matches)
      (0 (show-text actor "You don't see anyone like that to talk to."))
      (1 (talk actor (first matches) subject))
      (otherwise (show-text actor
                            "Do you want to talk to ~a?"
                            (format-list (mapcar #'describe-brief matches) :conjunction "or"))))))

;;; Say something to everyone at the same location.

(defevent say (actor message))

(defmethod do-say (actor message))

(defun punct-char-p (c)
  (not (or (alpha-char-p c) (digit-char-p c))))

(defun format-message (words)
  "Formats a raw sequence of words for presentation to an avatar."
  (let ((message (format nil "~{~a~^ ~}" words)))
    (setf (char message 0) (char-upcase (char message 0)))
    (if (not (punct-char-p (char message (1- (length message)))))
        (concatenate 'string message ".")
        message)))

(defmethod do-say :around (actor message)
  (show-text actor "You say, ~s" (format-message message))
  (call-next-method))

(defmethod do-say :after (actor message)
  (notify-observers (location actor) #'did-say actor message))

(defmethod did-say ((observer avatar) actor message)
  (when (not (eq observer actor))
    (show-text actor "~a says, ~s" (describe-brief actor :capitalize t) (format-message message))))

(defcommand (actor ("say" "\"") :rest words)
  (say actor words))

;;; Yell something that can be heard at your location and nearby locations.

;;; Tell something to a specific player.
