(in-package :lib)

;;; Paths are outdoor portals with unrestricted size.

(defproto path (portal)
  (brief "a path"))

(defproto gravel-path (path)
  (brief "a gravel path"))

(defproto sandy-path (path)
  (brief "a sandy path"))

(defproto cobbled-lane (path)
  (brief "a cobbled lane"))

;;; Doorways have no door to close, but restrict size.

(defproto doorway (portal)
  (brief "a doorway")
  (size :large))

(defproto entry-doorway (doorway)
  (pose "enters a building to the ~a."))

(defproto exit-doorway (doorway)
  (pose "exits the building to the ~a."))

;;;

(defproto stairway (portal)
  (brief "a stairway")
  (size :large))

;;;

(defproto iron-gate (portal)
  (brief "an iron gate")
  (size :large))
