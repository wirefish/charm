(in-package :charm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric transform-slot-init-form (def-name slot-name init-form))

  (defmethod transform-slot-init-form (def-name slot-name init-form)
    init-form)

  (defmethod transform-slot-init-form (def-name (slot-name (eql 'brief)) init-form)
    `(parse-noun ,init-form))

  (defmethod transform-slot-init-form (def-name (slot-name (eql 'pose)) init-form)
    `(parse-verb ,init-form))

  (defmethod transform-slot-init-form (def-name (slot-name (eql 'alts)) init-form)
    `(list ,@(mapcar #'(lambda (x)
                         `(parse-noun ,x))
                     init-form)))

  (defmethod transform-slot-init-form (def-name (slot-name (eql 'visible)) init-form)
    `(pairlis ',(mapcar #'(lambda (x) `(parse-noun ,x))
                        (mapcar #'first init-form))
              ',(mapcar #'second init-form)))

  (defmethod transform-slot-init-form (def-name (slot-name (eql 'exits)) init-form)
    `(list ,@(loop for (class . dirs) in init-form
                   append (loop for (dir dest) on dirs by #'cddr
                                collect `(make-instance ',class :direction ,dir :destination ',dest)))))

  (defmethod transform-slot-init-form (def-name (slot-name (eql 'contents)) init-form)
    `(list ,@(mapcar #'(lambda (spec)
                         (if (listp spec)
                             `(make-instance ',(car spec) ,@(cdr spec))
                             `(make-instance ',spec)))
                     init-form)))

  (defmethod transform-slot-init-form (def-name (slot-name (eql 'attack-verb)) init-form)
    `(parse-verb ,init-form))

  (defmethod transform-slot-init-form (def-name (slot-name (eql 'base-damage)) init-form)
    `(list ,@init-form))

  (defun transform-slot (def-name slot)
    (destructuring-bind (name init-form &optional instance) slot
      `(,name
        :initform ,(transform-slot-init-form def-name name init-form)
        :allocation ,(if instance :instance :class)
        ,(if instance :accessor :reader) ,name
        ,@(when instance (list :initarg (intern (symbol-name name) :keyword)))))))

;;; A macro to help define an entity class.

(defmacro defproto (name (&rest bases) &body slots)
  `(progn
     (defclass ,name ,bases
       ,(mapcar #'(lambda (slot) (transform-slot name slot)) slots))
     (export '(,name ,@(mapcar #'car slots)))))

;;; A macro to define an entity class and a singleton instance.

(defmacro defentity (name (&rest bases) &body slots)
  (let ((initargs (loop for (slot-name init-form) in slots
                        append (list (intern (string slot-name) :keyword)
                                     (transform-slot-init-form name slot-name init-form)))))
    `(progn
       (defclass ,name ,bases
         ,(mapcar #'(lambda (slot)
                      (list (car slot) :initarg (intern (string (car slot)) :keyword)))
           slots))
       (defparameter ,name (make-instance ',name ,@initargs)))))

;;; Entities have a notion of size. Depending on context this can indicate the
;;; size of the entity itself, or the size of the largest entity than can
;;; enter or pass through the entity.

(defparameter *sizes*
  (plist-hash-table '(:miniscule 1
                      :tiny 2
                      :small 3
                      :medium 4
                      :large 5
                      :huge 6
                      :gigantic 7)))

(defun size< (a b)
  (< (gethash a *sizes*) (gethash b *sizes*)))

(defun size<= (a b)
  (<= (gethash a *sizes*) (gethash b *sizes*)))

(defun size> (a b)
  (> (gethash a *sizes*) (gethash b *sizes*)))

;;; Base entity class.

(defvar *next-entity-id* 0)

(defproto entity ()
  (brief "an entity")
  (pose "is here.")
  (full nil)
  (icon 'cube)
  (alts ())
  (size :medium)
  (entry-pose nil)
  (hidden nil)
  (id (incf *next-entity-id*) :instance)
  (location nil :instance))

(defmethod print-object ((object entity) stream)
  "Inserts an entity's unique ID into its printed representation."
  (print-unreadable-object (object stream :type t :identity t)
    (write (id object) :stream stream)))

(defmethod encode-slot ((name (eql 'id)) value)
  nil)

(defmethod decode-slot ((name (eql 'id)) value)
  (incf *next-entity-id*))

(defgeneric get-modifier (modifier entity))

(defmethod get-modifier (modifier (entity entity))
  0)

;;; Entity subclasses.

(defproto container ()
  (capacity nil)
  (empty-pose "is empty.")
  (nonempty-pose "contains ~a.")
  (contents () :instance))

(defproto portal (entity)
  (brief "a portal")
  (pose "leads ~a.")
  (size :gigantic)
  (exit-message "~a heads ~a.")
  (enter-message "~a enters from ~a.")
  (direction nil :instance)
  (destination nil :instance))

(defproto location (entity container)
  (brief "Unnamed Location")
  (visible nil) ; things to look at that aren't proper entities
  (size :gigantic)
  (domain :outdoor) ; or :indoor, :underground, :astral, etc.
  (surface :dirt) ; any anything else, really
  (tutorial nil)
  (exits () :instance))

(defproto item (entity)
  (brief "an item")
  (unique nil) ;; or max number carried
  (bound nil) ;; if t, cannot be dropped or given
  (weight 1)
  (stack-limit 1)
  (stack-size 1 :instance))

(defproto quest-item (item)
  (quest nil) ; key of associated quest
  (bound t))

(defproto equipment (item)
  (level 0)
  (slot nil)
  (proficiency nil)
  (allow-nonproficient-use nil)
  (mastery nil)
  (modifiers nil)
  (quality nil :instance)
  (affixes nil :instance)
  (inscription nil :instance))

(defmethod get-modifier (modifier (entity equipment))
  (getf (modifiers entity) modifier 0))

(defproto weapon (equipment)
  (damage-type :crushing)
  (base-damage (1 4))
  (attack-verb "hits")
  (attack-delay 3))

(defproto creature (entity)
  (attitude :neutral)
  (begins-quests nil)
  (ends-quests nil))

;;; FIXME: move some of the more specific things to their own files.

(defproto vendor (creature)
  (sells nil))

(defproto trainer (creature)
  (teaches nil))
