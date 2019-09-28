(in-package :charm)

;;;; Functions that match input tokens against properties of objects.

(defgeneric match-tokens (tokens target)
  (:documentation "Evaluates a match between `tokens`, whhich is a sequence of
    strings, and `target`, which is an arbitrary object. Returns nil for no
    match, :partial for a partial match, and :exact for an exact match.")
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
  the tokens are exact matches for all the words in `target`. Returns :partial
  if the tokens are prefixes of a subset of the words in `target`. Otherwise,
  returns nil."
  (do ((pos 0)
       (match nil))
      ((or (null tokens) (null pos))
       (and (null tokens)
            (if (and pos (eq match :exact)) :partial match)))
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
        (with-slots (plural) target
          (best-match match (and plural (match-tokens tokens plural)))))))

(defmethod match-tokens (tokens (target entity))
  (labels ((find-id-token (tokens)
             (when-let ((id-token (find-if #'(lambda (x) (starts-with #\# x)) tokens)))
               (parse-integer id-token :start 1 :junk-allowed t))))
    (if-let ((id (find-id-token tokens)))
      (if (= (id target) id) :exact nil)
      (apply #'best-match
             (match-tokens tokens (brief target))
             (mapcar #'(lambda (x) (match-tokens tokens x)) (alts target))))))

(defmethod match-tokens (tokens (target portal))
  (let ((dir (direction target)))
    (best-match (match-tokens tokens (direction-name dir))
                (match-tokens tokens (direction-abbrev dir))
                (call-next-method))))

(defmethod match-tokens (tokens (target avatar))
  (best-match (call-next-method)
              (match-tokens tokens (race target))))

(defmethod match-tokens (tokens (target quest))
  (match-tokens tokens (name target)))

(defun match-objects (actor tokens &rest object-lists)
  "Returns a list of objects that represent matches between `tokens` and objects
  in `object-lists`. If any match is exact, only exact matches are returned.
  Otherwise, all partial matches are returned."
  (let (matches best)
    (dolist (list object-lists)
      (dolist (object list)
        (when (is-visible-p object actor)
          (when-let ((match (match-tokens tokens object)))
            (push (cons object match) matches)
            (setf best (best-match best match))))))
    (keep-if #'(lambda (x) (when (eq (cdr x) best) (car x))) matches)))
