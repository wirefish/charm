(in-package :charm)

;;; The directory containing data available to the running server.

(defparameter *root-directory* (pathname "/var/charm/"))

;;; Other directories, derived from above.

(defparameter *data-directory* (merge-pathnames "data/" *root-directory*))

(defparameter *help-directory* (merge-pathnames "help/" *data-directory*))

;;; The following must be assigned values from within the world definition, as
;;; appropriate.

(defparameter *new-avatar-location* nil)

(defparameter *new-avatar-race* nil)
