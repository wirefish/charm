(in-package :charm)

;;;

(defun remove-from-contents (container object)
  (with-slots (contents) container
    (setf contents (delete object contents))
    (setf (location object) nil)
    object))

(defun remove-from-contents-if (container pred)
  "Removes all objects from `container` for which `pred` evaluates to t, and
  returns a list of the removed items."
  (with-slots (contents) container
    (let ((removed-items (remove-if-not pred contents)))
      (setf contents (delete-if pred contents))
      removed-items)))

;;;

(defun replace-in-contents (container old new)
  (with-slots (contents) container
    (setf contents (nsubstitute new old contents))))

;;;

(defgeneric add-to-contents (container object))

(defmethod add-to-contents (container object)
  (with-slots (contents) container
    (push object contents)
    (setf (location object) container)
    object))

(defun can-stack-items (a b)
  (and (eq (type-of a) (type-of b))
       (<= (+ (stack-size a) (stack-size b)) (stack-limit a))))

(defun find-stack-in-container (container item)
  (position-if #'(lambda (x) (can-stack-items item x)) (contents container)))

(defun can-contain-item (container item)
  (and (size<= (size item) (size container))
       (or (< (length (contents container)) (capacity container))
           (find-stack-in-container container item))))

(defmethod add-to-contents (container (item item))
  (with-slots (contents) container
    (let ((stack-pos (find-stack-in-container container item)))
      (cond
        (stack-pos
         (let ((stack (nth stack-pos contents)))
           (incf (stack-size stack) (stack-size item))
           stack))
        ((or (null (capacity container))
             (< (length contents) (capacity container)))
         (call-next-method))))))

;;;

(defun find-item-of-same-type-in-container (container item)
  (find-if #'(lambda (x) (eq (type-of x) (type-of item)))
           (contents container)))
