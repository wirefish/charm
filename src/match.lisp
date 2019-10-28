(in-package :charm)

;;;; Functions that match input tokens against properties of objects.

(defgeneric match-tokens (tokens target)
  (:documentation "Evaluates a match between `tokens`, which is a sequence of
    strings, and `target`, which is an arbitrary object. Returns one of :exact,
    :partial, or nil to indicate the quality of the match.")
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
  "Matches a sequence of strings `tokens` against `target`. Returns :exact if
  the tokens are exact matches for all the words in `target`, :partial if the
  tokens are prefixes of a subset of the words in `target`, or nil otherwise."
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

(defmethod match-tokens (tokens (target noun))
  "Matches `tokens` against the singular and plural forms of `target`. Returns
  one of :exact, :partial, or nil depending on the quality of the match."
  (with-slots (singular plural) target
    (let ((match (match-tokens tokens singular)))
      (if (or (null plural) (eq match :exact))
          match
          (or (match-tokens tokens plural) match)))))

(defun best-match (&rest matches)
  "Returns the best match quality from `matches`, where :exact is better than
  :partial, which in turn is better than nil."
  (do (best)
      ((or (eq best :exact) (null matches)) best)
    (case (pop matches)
      (:exact (setf best :exact))
      (:partial (when (not best) (setf best :partial))))))

(defun match-tokens-any (tokens &rest objects)
  "Returns the best match quality resulting from matching `tokens` against each
  element of `objects`."
  (do (match)
      ((or (eq match :exact) (null objects)) match)
    (case (match-tokens tokens (pop objects))
      (:exact (setf match :exact))
      (:partial (when (not match) (setf match :partial))))))

(defun match-objects (tokens &rest object-lists)
  "Returns a list of objects that represent the best matches between `tokens`
  and objects in `object-lists`. If any match is exact, only exact matches are
  returned. Otherwise, all partial matches are returned. The secondary value
  indicates the best match quality and is one of :exact, :partial, or nil."
  (let (exact-matches partial-matches)
    (dolist (objects object-lists)
      (dolist (object objects)
        (case (match-tokens tokens object)
          (:exact
           (push object exact-matches))
          (:partial
           (when (not exact-matches)
             (push object partial-matches))))))
    (values (or exact-matches partial-matches)
            (cond
              (exact-matches :exact)
              (partial-matches :partial)))))

(defun match-objects-if (test tokens &rest object-lists)
  (apply #'match-objects tokens
         (mapcar (curry #'remove-if-not test) object-lists)))
