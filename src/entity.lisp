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
  (icon nil)
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

(defgeneric get-modifier (modifier entity)
  (:documentation "Entities can have or impart modifiers, depending on context.
    A modifier is an integer value that adjusts some attribute, and defaults to
    zero.")
  (:method (modifier entity)
    0))

;;; Don't encode an entity's unique ID, and instead assign it a new one when it
;;; is decoded.

(defmethod encode-slot ((name (eql 'id)) value)
  nil)

(defmethod decode-slot ((name (eql 'id)) value)
  (incf *next-entity-id*))

;;; Define how entities are described.

(defmethod visible-p ((subject entity) observer)
  (not (hidden subject)))

(defmethod describe-brief ((subject entity) &rest args)
  (apply #'format-noun (brief subject) args))

(defmethod describe-pose ((subject entity))
  (format-verb (pose subject)))

(defmethod describe-full ((subject entity))
  (or (full subject)
      (concatenate 'string
                   (describe-brief subject :capitalize t :article :definite)
                   " is unremarkable.")))

(defmethod describe-icon ((subject entity))
  (or (icon subject) 'cube))

;;; Define how entities are matched against user input.

(defmethod match-tokens (tokens (target entity))
  (labels ((find-id-token (tokens)
             (when-let ((id-token (find-if #'(lambda (x) (starts-with #\# x)) tokens)))
               (parse-integer id-token :start 1 :junk-allowed t))))
    (if-let ((id (find-id-token tokens)))
      (when (= (id target) id) :exact)
      (apply #'best-match
             (match-tokens tokens (brief target))
             (mapcar #'(lambda (x) (match-tokens tokens x)) (alts target))))))

;;;

(defun same-location-p (a b)
  (and (location a) (eq (location a) (location b))))
