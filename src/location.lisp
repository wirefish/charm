(in-package :charm)

;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod transform-slot-init-form (def-name (slot-name (eql 'exits)) init-form)
    `(list ,@(loop for (class . dirs) in init-form
                   append (loop for (dir dest) on dirs by #'cddr
                                collect `(make-instance ',class :direction ,dir :destination ',dest))))))

(defproto portal (entity)
  (brief "a portal")
  (pose "leads ~a.")
  (size :gigantic)
  (message nil)
  (exit-message "~a heads ~a.")
  (enter-message "~a enters from ~a.")
  (direction nil :instance)
  (destination nil :instance))

(defproto location (container entity)
  (brief "Unnamed Location")
  (visible nil) ; things to look at that aren't proper entities
  (size :gigantic)
  (domain :outdoor) ; or :indoor, :underground, :astral, etc.
  (surface :dirt) ; any anything else, really
  (tutorial nil)
  (z-offset 0)
  (exits () :instance))

(defmethod add-to-contents ((location location) entity)
  (prog1
      (call-next-method)
      (setf (location entity) location)))

;;;

(defparameter *directions*
  (plist-hash-table
   '(:north #("north" "n" :south)
     :northeast #("northeast" "ne" :southwest)
     :east #("east" "e" :west)
     :southeast #("southeast" "se" :northwest)
     :south #("south" "s" :north)
     :southwest #("southwest" "sw" :northeast)
     :west #("west" "w" :east)
     :northwest #("northwest" "nw" :southeast)
     :up #("up" "u" :down)
     :down #("down" "d" :up)
     :in #("in" nil :out)
     :out #("out" nil :in))))

(defun direction-name (dir)
  (elt (gethash dir *directions*) 0))

(defun direction-abbrev (dir)
  (elt (gethash dir *directions*) 1))

(defun opposite-direction (dir)
  (elt (gethash dir *directions*) 2))

(defun find-exit (location direction)
  (find-if #'(lambda (x) (eq (direction x) direction)) (exits location)))

(defun remove-exit (location direction)
  (with-slots (exits) location
    (setf exits (delete-if #'(lambda (x) (eq (direction x) direction)) exits))))

(defun add-exit (location portal)
  (with-slots (exits) location
    (push portal exits)))
