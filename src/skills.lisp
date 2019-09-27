(in-package :charm)

;;;

(defevent learn-skill (avatar skill trainer))

(defmethod do-learn-skill :around (avatar skill trainer)
  (cond
    ;; Check required level.
    ((< (level avatar) (level skill))
     (show-text avatar "You must be at least level ~d to learn the skill ~s."
                (level skill) (name skill)))
    ;; Check required race. FIXME: this comparison is wrong
    ((and (required-race skill) (not (eq (required-race skill) (race avatar))))
     (show-text avatar "You must be ~a in order to learn the skill ~s."
                (describe-brief (required-race skill))
                (name skill)))
    ;; FIXME: check that the skill is not already known.
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
      (show-notice avatar "You learn the skill ~s." (name skill))))

;;; TODO: add unlearn-skill event.

;;; TODO: add learn and unlearn commands.

(defcommand (actor ("skills" "sk"))
  "View a list of the skills you have learned."
  ;; FIXME: sort by level/name/guild?
  (if (> (hash-table-count (skills actor)) 0)
      (progn
        (show-text actor "You have learned the following skills:")
        (maphash #'(lambda (key rank)
                     (let ((skill (find-skill key)))
                       (show-text actor "- ~a~a: ~a"
                                  (name skill)
                                  (if (> (max-rank skill) 0)
                                      (format nil " (rank ~d/~d)" rank (max-rank skill))
                                      "")
                                  (summary skill))))
                 (skills actor)))
      (show-text actor "You haven't learned any skills yet.")))
