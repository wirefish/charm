(in-package :charm)

(defstruct command
  verbs clauses handler)

(defun format-command-alts (alts)
  (if (second alts)
      (format nil "~a (or ~{~a~^, ~})" (first alts) (rest alts))
      (first alts)))

(defun command-syntax (command)
  (format nil "> Syntax: ~a~{ [~a]~}~%~%"
          (format-command-alts (command-verbs command))
          (mapcar #'(lambda (clause)
                      (destructuring-bind (verbs . name) clause
                        (case verbs
                          ((nil :word :rest) (format nil "*~(~a~)*" name))
                          (t (format nil "~a *~(~a~)*" (format-command-alts verbs) name)))))
                  (command-clauses command))))

(defun command-help (command)
  (concatenate 'string
               (command-syntax command)
               (documentation (command-handler command) t)))

(defvar *commands* (make-hash-table :test #'equal))

(defun register-command (verbs clauses handler)
  (let ((command (make-command :verbs verbs :clauses clauses :handler handler)))
    (dolist (verb verbs)
      (setf (gethash verb *commands*) command))))

(defun get-all-command-verbs ()
  (let ((verbs nil))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (push (first (command-verbs v)) verbs))
             *commands*)
    (sort (remove-duplicates verbs) #'string<)))

(defun parse-grammar (grammar)
  "The grammar is a list of symbols, keywords, strings, and lists of strings.
  The symbols correspond with arguments that are passed to the command
  implementation. The strings correspond with input tokens and determine how
  other input tokens are assigned to each argument. Keywords allow for special
  handling of input tokens.

  The first two elements of the list are the symbol for the actor argument and
  the string or list of strings that define the verbs for the command.

  After that, each subsequence of tokens defines an additional argument. It can

  symbol -- a matching clause with no preceeding prepositions
  string-or-list symbol -- a matching clause with preceeding prepositions
  :word symbol -- a clause that consumes one token
  :rest symbol -- a clause that consumes the rest of the input

  Matching clauses are those that match against entities in the world."
  (let* ((actor (pop grammar))
         (verbs (let ((v (pop grammar)))
                  (if (listp v) v (list v)))))
    (do (clauses)
        ((null grammar) (list actor verbs (nreverse clauses)))
      (let ((x (pop grammar)))
        (cond
          ((keywordp x)
           (push (cons x (pop grammar)) clauses))
          ((symbolp x)
           (push (cons nil x) clauses))
          ((stringp x)
           (push (cons (list x) (pop grammar)) clauses))
          ((listp x)
           (push (cons x (pop grammar)) clauses)))))))

(defmacro defcommand (grammar &body body)
  (destructuring-bind (actor verbs clauses) (parse-grammar grammar)
    (let ((arg-names (list* actor (mapcar #'cdr clauses))))
      `(register-command
        ',verbs
        ',clauses
        (lambda ,arg-names ,@body)))))

(defvar *aliases* (make-hash-table :test #'equal))

(defun make-alias (command &rest aliases)
  (let ((tokens (tokenize-input command)))
    (dolist (alias aliases)
      (setf (gethash alias *aliases*) tokens))))

(defun strip-preposition (preps tokens)
  (if (find (first tokens) preps :test #'string-equal)
      (rest tokens)
      tokens))

(defun find-next-preposition (clauses tokens)
  (position-if #'(lambda (token)
                   (some #'(lambda (clause) (find token (car clause) :test #'string-equal)) clauses))
               tokens))

(defun parse-command-args (clauses tokens)
  (do (results)
      ((null clauses) (nreverse results))
    (let ((clause (car (pop clauses))))
      ;; clause is (verbs . arg-name)
      (cond
        ;; No more input to assign to this clause.
        ((null tokens)
         (push nil results))
        ;; Consume a single word.
        ((eq clause :word)
         (push (first tokens) results)
         (pop tokens))
        ;; Consume the rest of the input.
        ((eq clause :rest)
         (push tokens results)
         (setf tokens nil))
        ;; Consume until the next preposition, or end of input.
        (t
         (let ((end (find-next-preposition clauses tokens)))
           (if (null end)
               (progn
                 (push (strip-preposition clause tokens) results)
                 (setf tokens nil))
               (progn
                 (push (strip-preposition clause (subseq tokens 0 end)) results)
                 (setf tokens (subseq tokens end))))))))))

(defun find-command (verb)
  (gethash verb *commands*))

(defun run-command (actor command tokens)
  (with-slots (clauses handler) command
    (apply handler actor (parse-command-args clauses tokens))))

(defun process-input (avatar input)
  "Processes input from a player."
  (let ((tokens (tokenize-input input)))
    (when tokens
      (let* ((verb (string-downcase (first tokens)))
             (alias (gethash verb *aliases*)))
        (when alias
          (setf tokens alias)
          (setf verb (first tokens)))
        (let ((command (find-command verb)))
          (if command
              (run-command avatar command (rest tokens))
              (show-error avatar "Unknown command ~s. Type \"help\" for help.~%" verb)))))))
