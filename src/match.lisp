(in-package :charm)

(defgeneric match-tokens (tokens target))

(defun position-next-word (s pos)
  "Returns the position of the first character of the next word in `s` that
  follows the word at `pos`, or nil if there are no more words."
  (let ((sep (position #\Space s :start pos)))
    (and sep
         (position-if-not #'(lambda (c) (char= c #\Space)) s :start sep))))

(defun match-token (token subject pos)
  (eql (string-not-greaterp token subject :start2 pos) (length token)))

(defmethod match-tokens (tokens (target string))
  (do ((pos 0))
      ((or (null tokens) (null pos)) (null tokens))
    (when (match-token (first tokens) target pos)
      (pop tokens))
    (setf pos (position-next-word target pos))))

(defmethod match-tokens (tokens (target noun))
  (or (match-tokens tokens (noun-singular target))
      (with-slots (plural) target
        (and plural (match-tokens tokens plural)))))

(defmethod match-tokens (tokens (target entity))
  (labels ((find-id-token (tokens)
             (when-let ((id-token (find-if #'(lambda (x) (starts-with #\# x)) tokens)))
               (parse-integer id-token :start 1 :junk-allowed t))))
    (if-let ((id (find-id-token tokens)))
      (= (id target) id)
      (or (match-tokens tokens (brief target))
          (some #'(lambda (x) (match-tokens tokens x)) (alts target))))))

(defmethod match-tokens (tokens (target portal))
  (or (match-tokens tokens (symbol-name (direction target)))
      (call-next-method)))

(defmethod match-tokens (tokens (target avatar))
  (or (call-next-method)
      (match-tokens tokens (race target))))

(defmethod match-tokens (tokens (target quest))
  (match-tokens tokens (name target)))

(defun match-objects (actor tokens &rest object-lists)
  "Returns a list of objects that represent all matches between `tokens` and
  objects in `object-lists`."
  (let (matches)
    (dolist (list object-lists)
      (dolist (object list)
        (when (and (is-visible-p object actor)
                   (match-tokens tokens object))
          (push object matches))))
    matches))
