;;;; Generic functions that define how general descriptive information is
;;;; obtained for game objects.

(in-package :charm)

;;; The generic function `is-visible-p` is called to determine whether `subject`
;;; is visible to `observer`.

(defgeneric is-visible-p (subject observer))

(defmethod is-visible-p (subject observer)
  (not (hidden subject)))

(defmethod is-visible-p ((subject quest-item) (observer avatar))
  (quest-incomplete-p observer (find-quest (quest subject))))

;;; The generic function `describe-brief` returns a string containing a short
;;; phrase describing `target`. The string contains an article if appropriate.
;;; Examples: "an apple", "the master of ceremonies", "Bob the Builder",
;;; "Chamber of Shadow", "12 eggs".

(defgeneric describe-brief (target &rest args))

(defmethod describe-brief ((target entity) &rest args)
  (apply #'format-noun (slot-value target 'brief) args))

(defmethod describe-brief ((target avatar) &rest args)
  (or (name target) (apply #'describe-brief (race target) args)))

;;; The generic function `describe-pose` returns a sentence fragment that
;;; describes the target's current pose as seen by an observer at the same
;;; location. Examples: "is here.", "sits in the corner, sulking."

(defgeneric describe-pose (target))

(defmethod describe-pose ((target entity))
  (format-verb (pose target)))

(defmethod describe-pose ((target item))
  (format-verb (pose target) :count (stack-size target)))

;;; The generic function `describe-full` returns a string containing one or more
;;; sentences providing a full description of `target`.

(defgeneric describe-full (target))

(defmethod describe-full ((target entity))
  (or (full target)
      (concatenate 'string
                   (describe-brief target :capitalize t :article :definite)
                   " is unremarkable.")))

(defmethod describe-full ((target portal))
  (or (full target)
      (format nil "~a ~@?"
              (describe-brief target :capitalize t)
              (describe-pose target)
              (direction-name (direction target)))))

(defmethod describe-full ((target avatar))
  (or (full target) (describe-full (race target))))
