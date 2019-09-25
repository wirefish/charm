(in-package :charm)

;;;; Functions used to send information to the client.

(defun format-text (control-string args)
  (if args (apply #'format nil control-string args) control-string))

(defun show-raw (target control-string &rest args)
  (when (has-session-p target)
    (send-client-command (session target) "showRaw" (format-text control-string args))))

(defun show-text (target control-string &rest args)
  (when (has-session-p target)
    (send-client-command (session target) "showText" (format-text control-string args))))

(defun show-notice (target control-string &rest args)
  (when (has-session-p target)
    (send-client-command (session target) "showNotice" (format-text control-string args))))

(defun show-error (target control-string &rest args)
  (when (has-session-p target)
    (send-client-command (session target) "showError" (format-text control-string args))))

(defun show-tutorial (target message)
  (when (has-session-p target)
    (send-client-command (session target) "showTutorial" message)))

(defun show-help (target message)
  (send-client-command (session target) "showHelp" message))

(defun show-links (target heading prefix links)
  (send-client-command (session target) "showLinks" heading prefix links))

(defun show-say (target speaker control-string &rest args)
  (when (has-session-p target)
    (send-client-command
     (session target)
     "showSay"
     (format nil "~a says" (describe-brief speaker :capitalize t :article :definite))
     (format-text control-string args))))

(defun show-description (viewer subject)
  (show-text viewer (describe-full subject)))

(defun show-location (viewer)
  "Shows a description of the viewer's current location."
  (with-slots (location) viewer
    (send-client-command
     (session viewer)
     "describeRoom"
     (describe-brief location)
     (describe-full location)
     (sort (mapcar #'direction (remove-if-not #'(lambda (exit) (is-visible-p exit viewer))
                                              (exits location)))
           #'string<)
     (mapcar #'(lambda (object)
                 (list (id object) (describe-brief object) (describe-pose object)))
             (remove-if #'(lambda (object)
                            (or (eq object viewer)
                                (not (is-visible-p object viewer))))
                        (contents location))))))

(defun announce (location control-string &rest args)
  (let ((message (format-text control-string args)))
    (dolist (x (contents location))
      (show-notice x message))))

(defun update-avatar (avatar &rest properties)
  (when-let ((session (session avatar)))
    (send-client-command session "updateAvatar" (plist-hash-table properties))))

;;; Functions that manage the pane which displays entities in the same location
;;; as the player, aka neighbors.

(defun neighbor-properties (entity)
  "Returns a hash table of properties describing a neighbor, to be sent to the
  client."
  (plist-hash-table (list :key (id entity)
                          :brief (describe-brief entity :article nil)
                          :icon (icon entity))))

(defun set-neighbors (avatar)
  (when-let ((session (session avatar)))
    (send-client-command session "setNeighbors"
                         (keep-if #'(lambda (x)
                                      (when (and (is-visible-p x avatar) (not (eq x avatar)))
                                        (neighbor-properties x)))
                                  (contents (location avatar))))))

(defun update-neighbor (avatar obj &optional message)
  (when-let ((session (session avatar)))
    (send-client-command session "updateNeighbor"
                         (neighbor-properties obj)
                         message)))

(defun remove-neighbor (avatar obj &optional message)
  (when-let ((session (session avatar)))
    (send-client-command session "removeNeighbor"
                         (id obj) message)))
