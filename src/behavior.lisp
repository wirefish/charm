(in-package :charm)

(defmacro defbehavior (name (actor &rest args) (&rest vars) &body states)
  "Defines a function `name` with arguments `actor` and `args`. When called, the
  function begins running a behavior on `actor` and returns a closure that can
  be called to stop the behavior. Each instance of the behavior has local
  variables `vars` which can be initialized based on `args`. Each state is
  similar to a case clause; it comprises a state name (typically a keyword) and
  a sequence of forms to execute when entering that state. Within a state, the
  function `change-state` can be called to transition to a new state; it takes a
  required `state` argument and an keyword `delay` argument that specifies a
  delay in seconds before entering the new state. The special :stop state is
  executed when the cancel closure is run, and must not initiate a transition to
  any other state."
  (let ((event (gensym))
        (state (gensym))
        (delay (gensym)))
    `(defun ,name (,actor ,@args)
       (let (,@vars ,event)
         (labels ((change-state (,state &optional ,delay)
                    (if ,delay
                        (setf ,event (as:delay
                                      #'(lambda () (change-state ,state))
                                      :time ,delay))
                        (progn
                          (format-log :debug "~a changing state to ~a" ,actor ,state)
                          (case ,state ,@states)))))
           (change-state ,(first (first states)))
           (lambda (&optional (,state :stop) ,delay)
             (when (and ,event (not (as:event-freed-p ,event)))
               (as:free-event ,event))
             (change-state ,state ,delay)))))))

(defun start-behavior (actor key behavior &rest args)
  "Starts running `behavior` for `actor`, associating it with an arbitrary
  keyword `key` that can be passed to `stop-behavior` to stop running the
  behavior."
  (format-log :debug "starting behavior ~a (~a) for ~a" key behavior actor)
  (push (cons key (apply behavior actor args)) (behaviors actor)))

(defun start-parallel-behaviors (actor key behavior &rest arglists)
  "Starts running multiple parallel instances of `behavior`, where each arglist
  defines the arguments for one instance. The closure stored under `key` will
  change the state of all instances when called."
  (format-log :debug "starting parallel behaviors ~a (~a) for ~a" key behavior actor)
  (let ((closures (mapcar #'(lambda (args)
                              (apply behavior actor args))
                          arglists)))
    (push (cons key #'(lambda (&optional (state :stop) delay)
                        (dolist (closure closures)
                          (funcall closure state delay))))
          (behaviors actor))))

(defun change-behavior-state (actor key state &optional delay)
  "Changes the state of a behavior from outside the behavior's normal state
  machine."
  (when-let ((entry (assoc key (behaviors actor))))
    (funcall (cdr entry) state delay)))

(defun remove-behavior (actor key)
  "Removes a behavior associated with `key` for `actor` without sending it any
  state change. This is typically used from within the behavior itself to
  indicate it has naturally ended."
  (with-slots (behaviors) actor
    (when-let ((entry (assoc key behaviors)))
      (setf behaviors (delete (car entry) behaviors :key #'car)))))

(defun stop-behavior (actor key)
  "Stops running a behavior associated with `key` for `actor`."
  (with-slots (behaviors) actor
    (when-let ((entry (assoc key behaviors)))
      (format-log :debug "stopping behavior ~a for ~a" key actor)
      (setf behaviors (delete (car entry) behaviors :key #'car))
      (funcall (cdr entry) :stop))))

(defun stop-all-behaviors (actor)
  (loop for (key . closure) in (behaviors actor)
        do
           (format-log :debug "stopping behavior ~a for ~a" key actor)
           (funcall closure :stop))
  (setf (behaviors actor) nil))
