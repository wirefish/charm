(in-package :charm)

;;;

(defproto gathering-tool (equipment)
  (slot :tool)
  (required-skill nil)
  (required-rank 0))

(defmethod do-equip-item :around (actor (item gathering-tool) inventory-slot equipment-slot)
  (let ((rank (skill-rank actor (required-skill item))))
    (cond
      ((null rank)
       (show-text actor "You must learn the ~a skill before equipping that item."
                  (name (find-skill (required-skill item)))))
      ((< rank (required-rank item))
       (show-text actor "You must achieve rank ~d in the ~a skill before equipping that item."
                  (required-rank item)
                  (name (find-skill (required-skill item)))))
      (t
       (call-next-method)))))

(defproto resource (item)
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

;;;

(defevent gather-resources (actor resources node))

(defmethod do-gather-resources (actor resources node)
  (if resources
      (dolist (item resources)
        (give-item node item actor))
      (show-text actor "You obtain nothing of value.")))

(defmethod do-gather-resources :after (actor resources node)
  (notify-observers (location actor) #'did-gather-resources actor resources node)
  (when (= 0 (decf (attempts-remaining node)))
    (exit-world node (location actor))
    (respawn node (location actor))))

;;;

(defcommand (actor "gather")
  (if-let ((tool (gethash :tool (equipment actor))))
    (if-let ((node (find-if #'(lambda (x)
                                (and (typep x 'resource-node)
                                     (eq (required-skill x) (required-skill tool))))
                            (contents (location actor)))))
      (if-let ((resources (gatherable-resources actor node)))
        (progn
          (show-text actor "You begin gathering from ~a with your ~a."
                     (describe-brief node :article :definite)
                     (describe-brief tool :article nil))
          (with-delay (5)
            (do-gather-resources actor (gathered-resources resources) node)))
        (show-text actor
                   "You do not have the required skill to gather anything from ~a."
                   (describe-brief node :article :definite)))
      (show-text actor "You cannot gather anything here using the tool you have equipped."))
    (show-text actor "You do not have a tool equipped.")))
