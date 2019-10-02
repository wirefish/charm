(in-package :charm)

(defun make-offer (actor fn &rest args)
  "Sets the pending offer for `actor` so that, if accepted, `fn` will be applied
  to `args`."
  (setf (pending-offer actor) (cons fn args)))

(defcommand (actor "accept")
  "The accept command allows you to accept whatever was most recently offered to
  you. The offer could be for any number of things, such as a quest, a trade, or
  a party invitation."
  (if-let ((offer (pending-offer actor)))
    (progn
      (setf (pending-offer actor) nil)
      (apply (first offer) (rest offer)))
    (show-text actor "You have not been offered anything.")))
