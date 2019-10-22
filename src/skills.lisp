(in-package :charm)

;;;

(defevent learn-skill (avatar skill trainer))

(defmethod do-learn-skill :around (avatar skill trainer)
  (cond
    ;; Check required level.
    ((< (level avatar) (level skill))
     (show-text avatar "You must be at least level ~d to learn the skill ~s."
                (level skill) (name skill)))
    ;; Check required race.
    ((and (required-race skill) (not (eq (required-race skill) (key (race avatar)))))
     (show-text avatar "You must be ~a in order to learn the skill ~s."
                (describe-brief (symbol-value (required-race skill)))
                (name skill)))
    ;; Check that the skill is not already known.
    ((skill-rank avatar (key skill))
     (show-text avatar "You have already learned the skill ~s." (name skill)))
    ;; FIXME: Check that all required skills are known.
    ;; FIXME: Check that no exclusive skills are known.
    ;; FIXME: Check that the cost can be paid.
    (t
     (call-next-method))))

(defmethod do-learn-skill (avatar skill trainer)
  ;; FIXME: Deduct the cost.
  (setf (gethash (key skill) (skills avatar)) 0)
  (if trainer
      (show-notice avatar "~a teaches you the skill ~s."
                   (describe-brief trainer :capitalize t)
                   (name skill))
      (show-notice avatar "You learn the skill ~s." (name skill)))
  (update-skills avatar (list (key skill))))

(defcommand (actor "learn" skill-name)
  "When no *skill-name* is given, list the skills taught by a nearby trainer.
  Otherwise, try to learn the specified skill from the trainer."
  (if-let ((trainer (find-if #'(lambda (x) (typep x 'trainer))
                             (contents (location actor)))))
    ;; There is a trainer here.
    (if skill-name
        ;; Try to learn the skill.
        (let ((matches (match-objects skill-name
                                      (mapcar #'find-skill (teaches trainer)))))
          (case (length matches)
            (0
             (show-text actor "~a doesn't teach any skill like that."
                        (describe-brief trainer :capitalize t)))
            (1
             (learn-skill actor (first matches) trainer))
            (t
             (show-text actor "Do you want to learn ~a?"
                        (format-list (mapcar #'describe-brief matches) :conjunction "or")))))
        ;; List teachable skills.
        (progn
          (show-text actor "~a can teach you the following skills:"
                     (describe-brief trainer :capitalize t))
          (dolist (key (teaches trainer))
            (let ((skill (find-skill key)))
              (show-text actor "- `learn:~a`: ~a Level ~d. Cost: ~{~(~a~) ~d~^, ~}."
                         (name skill) (summary skill) (level skill)
                         (cost skill))))))
    ;; No trainer.
    (show-text actor "There is no trainer here.")))

;;;

(defevent unlearn-skill (avatar skill trainer))

(defmethod do-unlearn-skill :around (avatar skill trainer)
  ;; FIXME: check that unlearning won't violate any constrains on
  ;; required-skills for other known skills.
  (if (skill-rank avatar (key skill))
      (call-next-method)
      (show-text avatar "Do haven't learned the skill ~s." (name skill))))

(defmethod do-unlearn-skill (avatar skill trainer)
  ;; FIXME: add back karma
  (remhash (key skill) (skills avatar))
  (show-notice avatar "You unlearn the skill ~s." (name skill))
  (update-skills avatar (list (key skill))))

(defcommand (actor "unlearn" skill-name)
  "Unlearn a skill you previously learned. This can only be done at a trainer
  who teaches the skill. Unlearning a skill refunds any karma spent on the
  skill, but other costs will not be refunded."
  (if-let ((trainer (find-if #'(lambda (x) (typep x 'trainer))
                             (contents (location actor)))))
    ;; There is a trainer here.
    (if skill-name
        ;; Unlearn the skill.
        (let ((matches (match-objects skill-name
                                      (mapcar #'find-skill (teaches trainer)))))
          (case (length matches)
            (0
             (show-text actor "~a doesn't teach any skill like that."
                        (describe-brief trainer :capitalize t)))
            (1
             (unlearn-skill actor (first matches) trainer))
            (t
             (show-text actor "Do you want to unlearn ~a?"
                        (format-list (mapcar #'describe-brief matches) :conjunction "or")))))
        ;; List skills that the actor knows and the trainer teaches.
        (let ((keys (remove-if-not #'(lambda (key)
                                       (member key (teaches trainer)))
                                   (hash-table-keys (skills actor)))))
          (if (null keys)
              (show-text actor "~a can't remove your knowledge of any skill you
                currently know." (describe-brief trainer :capitalize t))
              (show-links actor
                          (format nil "~a can remove your knowledge of the following skills:"
                                  (describe-brief trainer :capitalize t))
                          "unlearn"
                          (sort (mapcar #'(lambda (key) (name (find-skill key))) keys)
                                #'string<)))))
    ;; No trainer.
    (show-text actor "There is no trainer here.")))

;;;

(defcommand (actor ("skills" "sk") :rest skill-name)
  "View information about the skills you've learned. If *skill-name* is
  specified, show details for matching skills. Otherwise, show a list of all the
  skills you have learned.

  For more information see `help:learn` and `help:unlearn`."
  (if (null skill-name)
      ;; List all skills.
      (with-slots (skills) actor
        (if (> (hash-table-count skills) 0)
            (show-links actor "You have learned the following skills:" "skills"
                        (sort (mapcar #'(lambda (x) (name (find-skill x)))
                                      (hash-table-keys skills))
                              #'string<))
            (show-text actor "You haven't learned any skills yet.")))
      ;; Show more information for matching skills.
      (let ((matches (match-skills skill-name actor)))
        (if (null matches)
            (show-text actor "You have not learned any skills matching that description.")
            (dolist (skill matches)
              (let ((rank (gethash (key skill) (skills actor))))
                (show-text actor "- ~a~a: ~a"
                           (name skill)
                           (if (> (max-rank skill) 0)
                               (format nil " (rank ~d/~d)" (floor rank) (max-rank skill))
                               "")
                           (summary skill))))))))
