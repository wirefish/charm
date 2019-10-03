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
           (lambda ()
             (when (and ,event (not (as:event-freed-p ,event)))
               (as:free-event ,event))
             (change-state :stop)))))))

(defun start-behavior (actor key behavior &rest args)
  "Starts running `behavior` for `actor`, associating it with an arbitrary
  keyword `key` that can be passed to `stop-behavior` to stop running the
  behavior."
  (format-log :debug "starting behavior ~a (~a) for ~a" key behavior actor)
  (push (cons key (apply behavior actor args)) (behaviors actor)))

(defun stop-behavior (actor key)
  "Stops running a behavior associated with `key` for `actor`."
  (with-slots (behaviors) actor
    (when-let ((entry (assoc key behaviors)))
      (format-log :debug "stopping behavior ~a for ~a" key actor)
      (setf behaviors (delete (car entry) behaviors :key #'car))
      (funcall (cdr entry)))))

(defun stop-all-behaviors (actor)
  (loop for (key . cancel-closure) in (behaviors actor)
        do
           (format-log :debug "stopping behavior ~a for ~a" key actor)
           (funcall cancel-closure))
  (setf (behaviors actor) nil))
