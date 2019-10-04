(in-package :charm)

(defproto item (entity)
  (brief "an item")
  (unique nil) ;; or max number carried
  (bound nil) ;; if t, cannot be dropped or given
  (weight 1)
  (stack-limit 1)
  (stack-size 1 :instance))

(defproto stackable-item (item)
  (stack-limit 100))

(defmethod describe-brief ((subject item) &rest args)
  (apply #'format-noun (brief subject) :count (stack-size subject) args))

(defmethod describe-pose ((subject item))
  (format-verb (pose subject) :count (stack-size subject)))

(defmethod stackable-p ((stack item) (item item))
  (and (eq (type-of stack) (type-of item))
       (<= (+ (stack-size stack) (stack-size item)) (stack-limit stack))))

(defmethod add-to-stack ((stack item) (item item))
  (incf (stack-size stack) (stack-size item))
  stack)
