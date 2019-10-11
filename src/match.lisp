(in-package :charm)

;;;; Functions that match input tokens against properties of objects.

(defgeneric match-tokens (tokens target)
  (:documentation "Evaluates a match between `tokens`, which is a sequence of
    strings, and `target`, which is an arbitrary object. Returns two values. The
    primary value is nil for no match, :partial for a partial match, and :exact
    for an exact match. The secondary value can be t if the match preferentially
    matches multiple instances of `target` in contexts where that makes sense,
    or nil otherwise.")
  (:method (tokens target)
    nil))

(defun position-next-word (s pos)
  "Returns the position of the first character of the next word in `s` that
  follows the word at `pos`, or nil if there are no more words."
  (let ((sep (position #\Space s :start pos)))
    (and sep
         (position-if-not #'(lambda (c) (char= c #\Space)) s :start sep))))

(defun match-token (token subject pos)
  "Matches `token` against the substring of `subject` starting at position
  `pos`, and returns nil, :partial, or :exact based on the quality of the
  match."
  (when (eql (string-not-greaterp token subject :start2 pos) (length token))
    (if (or (= (- (length subject) pos) (length token))
            (char= #\Space (char subject (+ pos (length token)))))
        :exact
        :partial)))

(defmethod match-tokens (tokens (target string))
  "Matches a sequence of strings `tokens` against `target`. Returns a primary
  value of :exact if the tokens are exact matches for all the words in `target`,
  :partial if the tokens are prefixes of a subset of the words in `target`, or
  nil otherwise."
  (do ((pos 0)
       (match nil))
      ((or (null tokens) (null pos))
       (and (null tokens) (if (and pos (eq match :exact)) :partial match)))
    (case (match-token (first tokens) target pos)
      ((nil)
       (setf match :partial))
      (:partial
       (setf match :partial)
       (pop tokens))
      (:exact
       (when (null match) (setf match :exact))
       (pop tokens)))
    (when pos
      (setf pos (position-next-word target pos)))))

(defun best-match (&rest matches)
  "Returns the best match quality from `matches`, where :exact is better than
  :partial, which in turn is better than nil."
  (reduce #'(lambda (a b)
              (cond
                ((or (eq a :exact) (eq b :exact)) :exact)
                ((or (eq a :partial) (eq b :partial)) :partial)))
          matches))

(defmethod match-tokens (tokens (target noun))
  (let ((match (match-tokens tokens (noun-singular target))))
    (if (eq match :exact)
        :exact
        (if-let ((plural (noun-plural target)))
          (case (match-tokens tokens plural)
            (:exact (values :exact t))
            (:partial (values :partial (not match)))
            (otherwise match))
          match))))

(defun match-targets (tokens &rest targets)
  (let ((matches (mapcar #'(lambda (target)
                             (multiple-value-list (match-tokens tokens target)))
                         targets)))
    (values (apply #'best-match (mapcar #'first matches))
            (some #'(lambda (x) x) (mapcar #'second matches)))))

(defun match-objects (tokens &rest object-lists)
  "Returns a list of objects that represent matches between `tokens` and objects
  in `object-lists`. If any match is exact, only exact matches are returned.
  Otherwise, all partial matches are returned. If there are matches, the
  secondary value is t if any match indicated a preferential match to
  multiple instances of an object, or nil otherwise."
  (let (matches best match-multiple)
    (dolist (list object-lists)
      (dolist (object list)
        (multiple-value-bind (match multiple) (match-tokens tokens object)
          (when match
            (when multiple (setf match-multiple t))
            (push (cons object match) matches)
            (setf best (best-match best match))))))
    (values
     (keep-if #'(lambda (x) (when (eq (cdr x) best) (car x))) matches)
     match-multiple)))
