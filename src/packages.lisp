(defpackage :charm
  (:documentation "Core server functionality.")
  (:use :common-lisp :sb-mop :alexandria)
  (:export
   :defentity :defproto :defregion :transform-slot-init-form
   :describe-brief :describe-pose :describe-full
   ;;
   :opposite-direction :add-exit :remove-exit :traverse-portal
   ;;
   :quest :defquest :offer-quest :advance-quest :quest-available-p
   :quest-active-p :quest-incomplete-p :quest-complete-p :quest-finished-p
   :name :summary :details :level :required-quests
   ;;
   :show-text :show-say :announce :maybe-show-tutorial
   :update-map-nearby
   :change-race :change-gender :change-name :valid-name-p
   :with-delay :format-log
   :run-server))

(in-package :charm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (find-package :lib))
    (make-package :lib :use '(:common-lisp :alexandria :charm)))

  (defparameter *region-packages*
    '(:isle-of-dawn
      :arwyck
      :silverwood))

  (dolist (name *region-packages*)
    (when (not (find-package name))
      (make-package name :use '(:common-lisp :charm :lib :alexandria)))))
