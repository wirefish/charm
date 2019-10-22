(in-package :charm)

(defmacro defbehavior (name (actor &rest args) (&rest vars) &body states)
  "Defines a function `name` with arguments `actor . args`. When called, the
  function begins running a behavior on `actor` and returns a closure that can
  be called to change the behavior's state. Each instance of the behavior has
  local variables `vars` which can be initialized based on `args`. Each state is
  similar to a case clause; it comprises a state name (typically a keyword) and
  a sequence of forms to execute when entering that state. Within a state, the
  function `change-state` can be called to transition to a new state; it takes a
  required `state` argument and an optional `delay` argument that specifies a
  delay in seconds before entering the new state. The special :stop state is
  entered to stop the behavior; it must not initiate a transition to any other
  state, and subsequent state change requests will be ignored."
  (let ((event (gensym))
        (stopped (gensym))
        (state (gensym))
        (key (gensym))
        (delay (gensym)))
    `(defun ,name (,actor ,key ,@args)
       (let* (,@vars ,event ,stopped)
         (labels ((finish ()
                    (remove-behavior ,actor ,key))
                  (change-state (,state &optional ,delay)
                    (when (not ,stopped)
                      (if ,delay
                          (setf ,event (as:delay
                                        #'(lambda () (change-state ,state))
                                        :time ,delay))
                          (progn
                            (format-log :debug "~a changing state to ~a" ,actor ,state)
                            (case ,state ,@states))))))
           (change-state ,(first (first states)))
           (lambda (,state &optional ,delay)
             (when (and ,event (not (as:event-freed-p ,event)))
               (as:free-event ,event))
             (change-state ,state ,delay)
             (when (eq ,state :stop) (setf ,stopped t))))))))

(defun start-behavior (actor key behavior &rest args)
  "Starts running `behavior` for `actor`, associating it with an arbitrary
  keyword `key` that can be passed to `stop-behavior` to stop running the
  behavior."
  (format-log :debug "starting behavior ~a (~a) for ~a" key behavior actor)
  (push (cons key (apply behavior actor key args)) (behaviors actor)))

(defun change-behavior-state (actor key state &optional delay)
  "Changes the state of a behavior from outside the behavior's normal state
  machine."
  (when-let ((entry (assoc key (behaviors actor))))
    (funcall (cdr entry) state delay)))

(defun remove-behavior (actor key)
  "Removes a behavior associated with `key` for `actor` without sending it any
  state change. This is typically used from within the behavior itself (via the
  local `finish` function) to indicate it has naturally ended."
  (with-slots (behaviors) actor
    (when-let ((entry (assoc key behaviors)))
      (setf behaviors (delete (car entry) behaviors :key #'car)))))

(defun stop-behavior (actor key)
  "Stops running a behavior associated with `key` for `actor` by changing its
  state to :stop and removing it from the actor's list of behaviors."
  (with-slots (behaviors) actor
    (when-let ((entry (assoc key behaviors)))
      (format-log :info "stopping behavior ~a for ~a" key actor)
      (setf behaviors (delete (car entry) behaviors :key #'car))
      (funcall (cdr entry) :stop)
      key)))

(defun stop-all-behaviors (actor)
  (loop for (key . closure) in (behaviors actor)
        do
           (format-log :info "stopping behavior ~a for ~a" key actor)
           (funcall closure :stop))
  (setf (behaviors actor) nil))

;;; An activity is just a behavior with key :activity and a few other special
;;; properties: (1) an entity can perform only one activity at a time; (2) many
;;; actions are disallowed while an activity is in progress; (3) certain allowed
;;; actions will interrupt the activity.

(defun stop-activity (actor)
  (stop-behavior actor :activity))

(defun start-activity (actor behavior &rest args)
  (stop-activity actor)
  (apply #'start-behavior actor :activity behavior args))

(defun has-activity (actor)
  (assoc :activity (behaviors actor)))
