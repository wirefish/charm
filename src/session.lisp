(in-package :charm)

(defun make-session-key ()
  (ironclad:byte-array-to-hex-string (ironclad:make-random-salt)))

;;; NOTE: The socket, session, and avatar have a number of references between
;;; themselves, as follows:
;;;
;;; socket -> session
;;; session -> socket and avatar
;;; avatar -> sesion

(defstruct session
  (key (make-session-key))
  (username nil)
  (avatar nil)
  (socket nil)
  (input-buffer nil)
  (output-queue (make-queue)))

(defvar *sessions* (make-hash-table :test 'equal)) ; username -> session

(defun find-session (username)
  (gethash username *sessions*))

(defun connect-session (session socket)
  "Associates a socket with a session and sends any queued output messages."
  (setf (session-socket session) socket)
  (loop while (not (queue-empty (session-output-queue session))) do
       (let ((data (queue-pop (session-output-queue session))))
         (as:write-socket-data socket data))))

(defun disconnect-session (session)
  "Removes the references between `session` and the associated socket, if any."
  (let ((socket (session-socket session)))
    (when socket
      (setf (as:socket-data socket) nil)))
  (setf (session-socket session) nil))

(defun send-message (session message)
  "Sends a message to the socket associated with the session. If the session is
  not connected (i.e. its socket is nil) then the message is queued so it can be
  sent later when `connect-session` is called."
  (let ((data (websocket-encode-message message))
        (socket (session-socket session)))
    (if socket
        (as:write-socket-data socket data)
        (queue-push data (session-output-queue session)))))

(defun send-client-command (session command &rest args)
  "Sends a message that contains a JSON array whose first element is a command
  name and whose subsequent elements are arguments to that command."
  (let ((message (with-output-to-string (s) (encode-json (cons command args) s))))
    (if session
        (send-message session message)
        (format t "~a~%" message))
    nil))

(defun has-session-p (target)
  (slot-exists-p target 'session))
