;;;; A simple FIFO queue.

(in-package :charm)

(defstruct queue
  (head nil)
  (tail nil))

(defun queue-empty (queue)
  (null (queue-head queue)))

(defun queue-push (item queue)
  "Adds `item` to the back of `queue`."
  (let ((cell (cons item nil)))
    (if (queue-tail queue)
        (setf (cdr (queue-tail queue)) cell
              (queue-tail queue) cell)
        (setf (queue-head queue) cell (queue-tail queue) cell))))

(defun queue-pop (queue)
  "Removes and returns the item at the front of `queue`."
  (let ((item (first (queue-head queue))))
    (setf (queue-head queue) (rest (queue-head queue)))
    (when (null (queue-head queue))
      (setf (queue-tail queue) nil))
    item))
