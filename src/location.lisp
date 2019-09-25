(in-package :charm)

(defun opposite-direction (dir)
  (case dir
    (:north :south)
    (:northeast :southwest)
    (:east :west)
    (:southeast :northwest)
    (:south :north)
    (:southwest :northeast)
    (:west :east)
    (:northwest :southeast)
    (:up :down)
    (:down :up)
    (:in :out)
    (:out :in)
    (otherwise dir)))

(defun direction-name (dir)
  (string-downcase (symbol-name dir)))

(defun find-exit (location direction)
  (find-if #'(lambda (x) (eq (direction x) direction)) (exits location)))

(defun remove-exit (location direction)
  (with-slots (exits) location
    (setf exits (delete-if #'(lambda (x) (eq (direction x) direction)) exits))))

(defun add-exit (location portal)
  (with-slots (exits) location
    (push portal exits)))
