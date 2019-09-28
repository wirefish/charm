(in-package :charm)

;;; Standard directions of movement.

(defparameter *directions*
  (plist-hash-table
   '(:north #("north" "n" :south (0 -1 0))
     :northeast #("northeast" "ne" :southwest (1 -1 0))
     :east #("east" "e" :west (1 0 0))
     :southeast #("southeast" "se" :northwest (1 1 0))
     :south #("south" "s" :north (0 1 0))
     :southwest #("southwest" "sw" :northeast (-1 1 0))
     :west #("west" "w" :east (-1 0 0))
     :northwest #("northwest" "nw" :southeast (-1 -1 0))
     :up #("up" "u" :down (0 0 1))
     :down #("down" "d" :up (0 0 -1))
     :in #("in" nil :out (0 0 0))
     :out #("out" nil :in (0 0 0)))))

(defun direction-name (dir)
  (elt (gethash dir *directions*) 0))

(defun direction-abbrev (dir)
  (elt (gethash dir *directions*) 1))

(defun opposite-direction (dir)
  (elt (gethash dir *directions*) 2))

(defun direction-offset (dir)
  (elt (gethash dir *directions*) 3))

;;; Portals connect locations to one another.

(defproto portal (entity)
  (brief "a portal")
  (pose "leads ~a.")
  (size :gigantic)
  (message nil)
  (exit-message "~a heads ~a.")
  (enter-message "~a enters from ~a.")
  (direction nil :instance)
  (destination nil :instance))

(defmethod describe-full ((subject portal))
  (or (full subject)
      (format nil "~a ~@?"
              (describe-brief subject :capitalize t)
              (describe-pose subject)
              (direction-name (direction subject)))))

(defmethod match-tokens (tokens (target portal))
  (let ((dir (direction target)))
    (best-match (match-tokens tokens (direction-name dir))
                (match-tokens tokens (direction-abbrev dir))
                (call-next-method))))

;;; A location is a place in the world.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod transform-slot-init-form (def-name (slot-name (eql 'exits)) init-form)
    `(list ,@(loop for (class . dirs) in init-form
                   append (loop for (dir dest) on dirs by #'cddr
                                collect `(make-instance ',class :direction ,dir :destination ',dest))))))

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

(defun find-exit (location direction)
  (find-if #'(lambda (x) (eq (direction x) direction)) (exits location)))

(defun remove-exit (location direction)
  (with-slots (exits) location
    (setf exits (delete-if #'(lambda (x) (eq (direction x) direction)) exits))))

(defun add-exit (location portal)
  (with-slots (exits) location
    (push portal exits)))

(defun find-destination (location direction)
  (when-let ((exit (find-exit location direction)))
    (and (boundp (destination exit))
         (symbol-value (destination exit)))))

(defmethod describe-icon ((subject location))
  (or (and (not (find-exit subject :out)) (icon subject))
      (when-let ((in-dest (find-destination subject :in)))
        (icon in-dest))))
