(in-package :charm)

;;;

(defproto gathering-tool (tool))

(defproto resource (stackable-item)
  (required-rank 0))

;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod transform-slot-init-form (def-name (slot-name (eql 'resources)) init-form)
    (when init-form
      `(list ,@(loop for (class probability min max) in init-form
                     collect `(list (make-instance ',class) ,probability ,min ,max))))))

(defproto resource-node (entity)
  (required-skill nil)
  (resources nil) ;; list of (item probability min max)
  (respawn-delay '(150 300))
  (exit-pose "is exhausted.")
  (attempts-remaining (uniform-random 1 3) :instance))

(defmethod format-give-item ((actor resource-node) item)
  (format nil "You obtain ~a." (describe-brief item)))

;;;

(defun gatherable-resources (actor node)
  "Returns the subset of a node's resources that are actually obtainable by an
  actor based on equipped tool and current rank in the required skill."
  (let ((rank (skill-rank actor (required-skill node))))
    (remove-if-not #'(lambda (resource)
                       (>= rank (required-rank (first resource))))
                   (resources node))))

(defun gathered-resources (gatherable-resources)
  "Randomly determines the number of type of resources actually gathered, given
  the resources that could be gathered."
  (keep-if #'(lambda (resource)
               (destructuring-bind (item probability min max) resource
                 (when (< (random 1.0) probability)
                   (make-instance (type-of item) :stack-size (uniform-random min max)))))
           gatherable-resources))

(defun gathering-skill-increase (actor-rank item-rank)
  "Returns the amount by which to increase an actor's rank in a gathering skill
  based on the actor's current rank and the required-rank of an item that has
  been gathered."
  (expt 1.25 (- item-rank actor-rank)))

;;;

(defevent gather-resources (actor resources node))

(defmethod do-gather-resources (actor resources node)
  (if resources
      (let* ((skill-key (required-skill node))
             (actor-rank (skill-rank actor skill-key)))
        (dolist (item resources)
          (give-item node item actor))
        (increase-skill-rank
         actor skill-key
         (apply #'+ (mapcar #'(lambda (item)
                                (gathering-skill-increase actor-rank (required-rank item)))
                            resources))))
      (show-text actor "You obtain nothing of value.")))

(defmethod do-gather-resources :after (actor resources node)
  (notify-observers (location actor) #'did-gather-resources actor resources node)
  (when (= 0 (decf (attempts-remaining node)))
    (exit-world node (location actor) nil)
    (respawn node (location actor))))

;;;

(defbehavior gather (actor resources node)
    ((tool (gethash :tool (equipment actor))))
  (:start
   (show-text actor "You begin gathering from ~a with your ~a."
              (describe-brief node :article :definite)
              (describe-brief tool :article nil))
   (change-state :finish 5))
  (:finish
   (do-gather-resources actor (gathered-resources resources) node)
   (remove-behavior actor :activity))
  (:stop
   (show-text actor "Your gathering attempt has been interrupted.")))

(defcommand (actor "gather")
  "Attempt to gather resources from a resource node at your location. You must
  have a tool equipped that is appropriate for the type of resource you wish to
  gather. Gathering takes several seconds and will be interrupted if you move,
  enter combat, or use another ability."
  (if-let ((tool (gethash :tool (equipment actor))))
    (if-let ((node (find-if #'(lambda (x)
                                (and (typep x 'resource-node)
                                     (eq (required-skill x) (required-skill tool))))
                            (contents (location actor)))))
      (if-let ((resources (gatherable-resources actor node)))
        (progn
          (stop-behavior actor :activity)
          (start-behavior actor :activity #'gather resources node))
        (show-text actor
                   "You do not have the required skill to gather anything from ~a."
                   (describe-brief node :article :definite)))
      (show-text actor "You cannot gather anything here using the tool you have equipped."))
    (show-text actor "You do not have a tool equipped.")))
