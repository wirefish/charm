(in-package :charm)

;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod transform-slot-init-form (def-name (slot-name (eql 'contents)) init-form)
    `(list ,@(mapcar #'(lambda (spec)
                         (if (listp spec)
                             `(make-instance ',(car spec) ,@(cdr spec))
                             `(make-instance ',spec)))
                     init-form))))

(defproto container ()
  (capacity nil)
  (empty-pose "is empty.")
  (nonempty-pose "contains ~a.")
  (contents () :instance))

;;;

(defun remove-from-contents (container object)
  (with-slots (contents) container
    (setf contents (delete object contents))
    (setf (location object) nil)
    object))

(defun remove-from-contents-if (container pred)
  "Removes all entities from `container` for which `pred` evaluates to t, and
  returns a list of the removed entities."
  (with-slots (contents) container
    (let ((removed (remove-if-not pred contents)))
      (setf contents (delete-if pred contents))
      removed)))

;;;

(defun replace-in-contents (container old new)
  (with-slots (contents) container
    (setf contents (nsubstitute new old contents))))

;;; Some entities can be combined together into stacks. For example, two
;;; identical coins might combine into a stack of two coints that take one slot
;;; in a container, rather than two separate slots. Entities that can stack must
;;; implement `stackable-p` and `add-to-stack`.

(defgeneric stackable-p (stack entity))

(defmethod stackable-p (stack entity))

(defgeneric add-to-stack (stack entity))

;;;

(defun find-stack-in-container (container item)
  (position-if #'(lambda (stack) (stackable-p stack item)) (contents container)))

(defun can-contain-item (container item)
  (and (size<= (size item) (size container))
       (or (< (length (contents container)) (capacity container))
           (find-stack-in-container container item))))

(defun add-to-contents (container entity)
  "Adds `entity` to the contents of `container`. Returns `entity` or the stack
  into which `entity` was merged."
  (with-slots (capacity contents) container
    (let ((stack-pos (find-stack-in-container container entity)))
      (cond
        (stack-pos
         (add-to-stack (nth stack-pos contents)))
        ((or (null capacity)
             (< (length contents) capacity))
         (push entity contents)
         (setf (location entity) container)
         entity)))))

;;;

(defun find-item-of-same-type-in-container (container item)
  (find-if #'(lambda (x) (eq (type-of x) (type-of item)))
           (contents container)))
