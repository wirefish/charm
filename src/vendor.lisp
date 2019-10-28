(in-package :charm)

(defun create-vendor-item (type)
  (let ((item (make-instance type)))
    (destructuring-bind (currency quantity) (vendor-price item)
      (cons item
            (make-instance currency :stack-size quantity)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
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

;;;

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
    (give-item vendor (make-instance (type-of item)) actor)
    (show-text actor "You buy ~a for ~a."
               (describe-brief item)
               (describe-brief price))))

(defun find-vendor (location)
  (find-if #'(lambda (x) (typep x 'vendor))
           (contents location)))

(defcommand (actor "buy" item)
  "Purchase an item from a nearby vendor. If *item* is not specified, display a
  list of items available for purchase."
  (if-let ((vendor (find-vendor (location actor))))
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
                            (sells vendor)))))
    (show-text actor "There is no vendor here.")))

;;;

(defevent sell-item (actor item vendor price))

(defmethod do-sell-item (actor item vendor price)
  ;; TODO:
  (print (list actor item (location item) vendor price)))

(defcommand (actor "sell" item)
  "Sell an item to a nearby vendor."
  (if-let ((vendor (find-vendor (location actor))))
    (if item
        (multiple-value-bind (tokens quantity) (split-quantity item)
          (let ((matches (match-in-inventory tokens actor :test #'sell-price)))
            (cond
              ((null matches)
               (show-text actor "You have nothing like that that the vendor wants."))
              ((or (= (length matches) 1)
                   (eq quantity :all))
               (dolist (item matches)
                 (sell-item actor (cdr item) vendor (sell-price (cdr item)))))
              (t (show-text actor "Do you want to sell ~a?"
                          (format-list (mapcar #'(lambda (x) (describe-brief (cdr x)))
                                               matches)
                                       :conjunction "or"))))))
        (show-text actor "What do you want to sell?"))
    (show-text actor "There is no vendor here.")))
