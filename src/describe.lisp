;;;; Generic functions that determine how game objects are described to the
;;;; player.

(in-package :charm)

(defgeneric visible-p (subject observer)
  (:documentation "Returns t if `subject` is visible to `observer`.")
  (:method (subject observer)
    t))

(defun keep-visible (observer object-list)
  "Returns a list containing those elements of `object-list` that are visible to
  `observer`."
  (remove-if-not #'(lambda (x) (visible-p x observer)) object-list))

(defgeneric describe-brief (subject &rest args)
  (:documentation "Returns a string containing a short phrase describing
    `subject`. The string contains an article if appropriate. Examples: 'an
    apple', 'the master of ceremonies', 'Bob the Builder', '12 eggs', or
    'Chamber of Shadows'. The `args` can be used to control the specifics of the
    result, based on the underlying implementation."))

(defgeneric describe-pose (subject)
  (:documentation "Returns a sentence fragment that describes the subject's
    current pose as seen by an observer at the same location. Examples: 'is
    here.', 'sits in the corner, sulking.'"))

(defgeneric describe-full (subject)
  (:documentation "Returns a string containing one or more sentences that
    provide a full description of `subject`, such as could be obtained by an
    observer who carefully looks at `subject` for a few moments."))

(defgeneric describe-icon (subject)
  (:documentation "Returns a symbol denoting the icon to display for
    `subject`."))
