(in-package :charm)

(defproto item (entity)
  (brief "an item")
  (unique nil) ;; or max number carried
  (bound nil) ;; if t, cannot be dropped or given
  (weight 1)
  (stack-limit 1)
  (stack-size 1 :instance))

(defmethod stackable-p ((stack item) (item item))
  (and (eq (type-of stack) (type-of item))
       (<= (+ (stack-size stack) (stack-size item)) (stack-limit stack))))

(defmethod add-to-stack ((stack item) (item item))
  (incf (stack-size stack) (stack-size item))
  stack)
