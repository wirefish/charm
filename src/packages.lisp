(defpackage :charm
  (:documentation "Core server functionality.")
  (:use :common-lisp :sb-mop :alexandria)
  (:import-from :cl-async :with-delay)
  (:export
   :defproto :deflocation :defregion :transform-slot-init-form
   :describe-brief :describe-pose :describe-full :describe-exit :describe-entry
   :format-noun :format-verb
   :same-location-p
   ;;
   :opposite-direction :add-exit :remove-exit :traverse-portal
   ;;
   :defrace
   ;;
   :defbehavior :start-behavior :stop-behavior :change-state
   ;;
   :quest :defquest :offer-quest :advance-quest :quest-available-p
   :quest-active-p :quest-incomplete-p :quest-complete-p :quest-finished-p
   :key :name :summary :details :level :required-quests
   ;;
   :skill :defskill
   :required-race :required-skills :exclusive-skills :cost :max-rank
   :rank :command :recipe-list
   ;;
   :show-text :show-say :show-notice :announce :maybe-show-tutorial
   :update-map-nearby
   :change-race :change-gender :change-name :valid-name-p
   :with-delay :format-log
   :run-server))

(in-package :charm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (find-package :lib))
    (make-package :lib :use '(:common-lisp :alexandria :charm)))

  (defparameter *region-packages*
    '(:arwyck
      :dripping-caverns
      :copper-mine
      :isle-of-dawn
      :silverwood))

  (dolist (name *region-packages*)
    (when (not (find-package name))
      (make-package name :use '(:common-lisp :charm :lib :alexandria)))))
