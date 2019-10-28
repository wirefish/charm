(in-package :charm)

(defproto item (entity)
  (brief "an item")
  (unique nil) ; or max number carried
  (bound nil) ; if t, cannot be dropped or given
  (weight 1)
  (buy-price nil) ; price to purchase from vendor as (currency amount)
  (sell-price t) ; if t, buy-price / 10; nil, cannot sell; otherwise as above
  (stack-limit 1)
  (stack-size 1 :instance))

(defproto stackable-item (item)
  (stack-limit 100))

(defproto currency (stackable-item)
  (weight 0)
  (stack-limit 100000))

(defmethod describe-brief ((subject item) &rest args)
  (let ((count (or (getf args :count) (stack-size subject))))
    (apply #'format-noun (brief subject) :count count (remove-from-plist args :count))))

(defmethod describe-pose ((subject item))
  (format-verb (pose subject) :count (stack-size subject)))

(defmethod stackable-p ((stack item) (item item))
  (and (eq (type-of stack) (type-of item))
       (<= (+ (stack-size stack) (stack-size item)) (stack-limit stack))))
