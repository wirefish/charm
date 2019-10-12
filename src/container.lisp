(in-package :charm)

;;; A container is a "mix-in" base class that allows an entity to contain other
;;; entities.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod transform-slot-init-form (def-name (slot-name (eql 'contents)) init-form)
    `(list ,@(mapcar #'(lambda (spec)
                         (if (listp spec)
                             `(make-instance ',(first spec) ,@(rest spec))
                             `(make-instance ',spec)))
                     init-form))))

(defproto container ()
  (capacity nil)
  (empty-pose "is empty.")
  (nonempty-pose "contains ~a.")
  (contents () :instance))

(defmethod decode-object ((object container) args)
  (call-next-method)
  (dolist (entity (contents object))
    (setf (location entity) object)))

;;;

(defun count-quantity (objects)
  (reduce #'+ (mapcar #'stack-size objects)))

(defun find-in-container-if (pred container)
  "Returns a list of objects in `container` for which `pred` evaluates to t."
  (remove-if-not pred (contents container)))

(defun find-type-in-container (type container)
  "Returns a list of objects in `container` with the specified type."
  (find-in-container-if #'(lambda (x) (typep x type)) container))

(defun count-type-in-container (type container)
  "Returns the total quantity of objects with type `type` in container."
  (count-quantity (find-type-in-container type container)))

;;;

(defun remove-from-container (object container &key quantity)
  "Removes `object` from `container`. If `quantity` is not nil, removes at most
  `quantity` of `object`. Returns two values: the quantity actually removed, and
  t if object was completely removed or nil if its stack size was merely
  reduced."
  (with-slots (contents) container
    (if (or (null quantity) (>= quantity (stack-size object)))
        (progn
          (setf contents (delete object contents))
          (setf (location object) nil)
          (values (stack-size object) t))
        (progn
          (decf (stack-size object) quantity)
          (values quantity nil)))))

(defun remove-from-container-if (pred container &key quantity)
  "Removes objects from `container` for which `pred` evaluates to t. If
  `quantity` is not nil, removes at most `quantity` such objects. Returns two
  values: the number of objects actually removed and a list of the stacks that
  were removed, if any."
  (do ((objects (find-in-container-if pred container))
       (num-removed 0)
       removed-stacks)
      ((or (null objects) (eql num-removed quantity))
       (values num-removed removed-stacks))
    (let ((object (pop objects)))
      (multiple-value-bind (n stack-removed)
          (remove-from-container object container
                                 :quantity (and quantity (- quantity num-removed)))
        (incf num-removed n)
        (print (list n num-removed quantity))
        (when stack-removed
          (push object removed-stacks))))))

(defun remove-type-from-container (type container &key quantity)
  "Removes objects of type `type` from container. If `quantity` is not nil,
  removes at most `quantity` objects. Returns two values: the quantity actually
  removed and a list of stacks that were removed."
  (remove-from-container-if #'(lambda (x) (typep x type)) container :quantity quantity))

;;;

(defun stack-remaining (stack)
  "Returns the quantity that can be added to `stack` before it reaches its stack
  limit."
  (- (stack-limit stack) (stack-size stack)))

(defun add-to-stack (object stack &key quantity)
  "Attempts to add `object` to `stack`. If `quantity` is not null, adds at most
  that number to the stack. Returns the quantity that was actually added to the
  stack, which can be zero."
  (let ((n (min (stack-remaining stack)
                (if quantity
                    (min (stack-size object) quantity)
                    (stack-size object)))))
    (if (> n 0)
        (progn
          (incf (stack-size stack) n)
          n)
        0)))

;;;

(defgeneric containable-p (object container)
  (:documentation "Returns t if `object` can generally be stored in `container`,
    based on properties of `object` and without regard to available capacity of
    `container`."))

(defmethod containable-p (object (container container))
  (size<= (size object) (size container)))

;;;

(defun addable-to-container (object container &key quantity)
  "Returns t if it is possible to add `object` to `container`. As a secondary
  value, returns the list of stacks into which object can be merged."
  (let ((stacks (find-in-container-if #'(lambda (x) (stackable-p x object)) container)))
    (with-slots (contents capacity) container
      (values
       (and (containable-p object container)
            (or (>= (reduce #'+ (mapcar #'stack-remaining stacks) :initial-value 0)
                    (or quantity (stack-size object)))
                (null capacity)
                (< (length contents) capacity)))
       stacks))))

(defun add-to-container (object container
                         &key quantity force remove-from)
  "Attempts to add `object` to `container`. When possible, `object` will be
  merged into existing stacks; any excess quantity will create a new stack. When
  `quantity` is specified, only part of the stack represented by `object` is
  added to `container`. When `force` is t, a new stack can always be created
  even if it exceeds the capacity of the container. When `remove-from` is not
  nil, anything added to `container` is also removed from the specified
  container. Returns the list of modified stacks in `container` (which may
  include `object` if it was not merged into any existing stack and used to
  create a new stack with its original stack-size), or nil if the object could
  not be added."
  (multiple-value-bind (addable stacks)
      (addable-to-container object container :quantity quantity)
    (when (or addable force)
      (when remove-from
        (remove-from-container object remove-from :quantity quantity))
      (with-slots (contents) container
        ;; Place as much into existing stacks as possible, and overflow the rest
        ;; into an empty slot.
        (let ((quantity (or quantity (stack-size object)))
              changed-stacks)
          (do ()
              ((or (= 0 quantity) (null stacks)))
            (let* ((stack (first stacks))
                   (n (add-to-stack object stack :quantity quantity)))
              (when (> n 0)
                (decf quantity n)
                (push stack changed-stacks))
              (pop stacks)))
          (when (> quantity 0)
            ;; Use `object` as a new stack unless its stack size is reduced, in
            ;; which case create a new instance of the stackable type.
            (when (< quantity (stack-size object))
              (setf object (make-instance (type-of object) :stack-size quantity)))
            (setf (location object) container)
            (push object changed-stacks)
            (push object contents))
          changed-stacks)))))
