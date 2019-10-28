;;;; Events and commands related to inspecting entities.

(in-package :charm)

(defevent look (actor target tool))

(defmethod do-look (actor target tool)
  (show-description actor target))

(defcommand (actor "look" "at" target ("in" "on") where)
  "Look at something in your environment. By just typing `look`, you will see a
  description of your current location and a summary of the entities present
  there. To look at something specific, add *target*. For example, `look orc` or
  `look at self`. (Note that 'at' is always optional.) Normally the command will
  look for entities in your location and among the items you have equipped. To
  look at something in a specific place, add *where*. For example, `look at
  dagger in backpack`."
  (cond
    ;; Look at the actor's location.
    ((and (null target) (null where))
     (show-location actor))
    ;; Look at the actor.
    ((and (= (length target) 1) (string-equal (first target) "self") (null where))
     (look actor actor nil))
    ;; Look at contents of specific containers.
    (where
     (let ((containers (match-objects-if #'(lambda (x) (typep x 'container))
                                         where
                                         (list*
                                          (gethash :backpack (equipment actor))
                                          (gethash :in-hands (equipment actor))
                                          (contents (location actor))))))
       (if containers
           (if target
               ;; Look at matching items.
               (if-let ((matches (apply #'match-objects-if
                                        #'(lambda (x) (visible-p x actor))
                                        target
                                        (mapcar #'contents containers))))
                 (dolist (match matches)
                   (look actor match nil))
                 (show-text "You don't see anything matching ~s in ~a."
                            (merge-tokens target)
                            (format-list (mapcar #'describe-brief containers) :conjunction "or")))
               ;; List the contents of the containers.
               (dolist (container containers)
                 (show-text actor "The ~a contains ~a."
                            (describe-brief container :article nil)
                            (format-list (mapcar #'describe-brief (contents container))))))
           (show-text actor "You don't see anything matching ~s that might contain anything."
                      (merge-tokens where)))))
    ;; Look for matches among the contents and exits of the location, and
    ;; equipped items.
    (t
     (if-let ((matches (match-objects target
                                      (keep-visible actor (contents (location actor)))
                                      (exits (location actor))
                                      (hash-table-values (equipment actor)))))
       (dolist (match matches)
         (look actor match nil))
       (show-text actor "You don't see anything matching ~s here, or among your equipped items."
                  (merge-tokens target))))))
