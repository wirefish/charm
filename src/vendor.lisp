(in-package :charm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun create-vendor-item (type)
    (let ((item (make-instance type)))
      (destructuring-bind (currency quantity) (vendor-price item)
        (cons item
              (make-instance currency :stack-size quantity)))))

  (defmethod transform-slot-init-form (def-name (slot-name (eql 'sells)) init-form)
    `(list ,@(mapcar #'(lambda (type)
                         `(create-vendor-item ',type))
                     init-form))))

(defproto vendor (npc)
  (sells nil))

(defun match-vendor-items (tokens vendor)
  (remove-if-not #'(lambda (item)
                     (match-tokens tokens (car item)))
                 (sells vendor)))

(defevent buy-item (actor item vendor price))

(defmethod do-buy-item :around (actor item vendor price)
  (let ((avail (count-type-in-inventory (type-of price) actor)))
    (if (< avail (stack-size price))
        (show-text actor "You don't have enough ~a to buy that."
                   (describe-brief price :count t))
        (call-next-method))))

(defmethod do-buy-item (actor item vendor price)
  (let ((paid (remove-type-from-inventory (type-of price) actor
                                          :quantity (stack-size price))))
    (print paid)
    (give-item vendor (make-instance (type-of item)) actor)
    (show-text actor "You buy ~a for ~a."
               (describe-brief item)
               (describe-brief price))))

(defcommand (actor "buy" item)
  "Purchase an item from a nearby vendor. If *item* is not specified, display a
  list of items available for purchase."
  (if-let ((vendor (find-if #'(lambda (x) (typep x 'vendor))
                            (contents (location actor)))))
    (if item
        ;; Purchase an item.
        (let ((matches (match-vendor-items item vendor)))
          (case (length matches)
            (0 (show-text actor "~a doesn't sell anything like that."
                          (describe-brief vendor :capitalize t)))
            (1 (destructuring-bind (item . price) (first matches)
                 (buy-item actor item vendor price)))
            (t (show-text actor "Do you want to buy ~a?"
                          (format-list (mapcar #'(lambda (x) (describe-brief (car x)))
                                               matches)
                                       :conjunction "or")))))
        ;; List available items.
        (show-text actor "~a sells the following items: ~a."
                   (describe-brief vendor :capitalize t)
                   (format-list
                    (mapcar #'(lambda (item)
                                (format nil "~a for ~a"
                                        (describe-brief (car item))
                                        (describe-brief (cdr item))))
                            (sells vendor)))))))
