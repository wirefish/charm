(in-package :charm)

(defun article-p (token)
  "Returns t if `token` is an article."
  (some #'(lambda (s) (string-equal token s)) '("a" "an" "the")))

(defun split-article (text)
  "Given a string `text` that represents a noun phrase, returns two values that
  represent the article (or nil if no article is present) and the remainder of
  the string."
  (let* ((sep (position #\space text))
         (prefix (subseq text 0 sep)))
    (if (article-p prefix)
        (values prefix (subseq text (+ sep 1)))
        (values nil text))))

(defun guess-article (text)
  "Given a string representing a noun phrase with no article, attempt to guess
  the appropriate indefinite article."
  (if (find (char text 0) "aeiou") "an" "a"))

(defun guess-plural (text)
  "Given a string `text` that represents a noun phrase, returns a guess at its
  plural form."
  (cond
    ((string-ends-with text "y")
     (if (find (char text (- (length text) 2)) "aeiou")
         (strcat text "s")
         (strcat (subseq text 0 (- (length text) 1)) "ies")))
    ((some #'(lambda (x) (string-ends-with text x)) '("s" "x" "z" "o" "ch" "sh"))
     (strcat text "es"))
    (t
     (strcat text "s"))))

(defun guess-singular (text)
  (cond
    ((string-ends-with text "ies")
     (strcat (subseq text 0 (- (length text) 3)) "y"))
    ((string-ends-with text "es")
     (let ((stem (subseq text 0 (- (length text) 2))))
       (if (string= (guess-plural stem) text)
           stem
           (subseq text 0 (- (length text) 1)))))
    ((string-ends-with text "s")
     (subseq text 0 (- (length text) 1)))
    (t
     text)))

(defun make-singular-and-plural (text)
  "Given a string representing a noun phrase, returns two values representing
  the singular and plural forms of the string. The string may contain a
  specifier in square brackets; otherwise the plural form is guessed. For
  example:

  'red car' -> 'red car' 'red cars'
  'box[es] of rocks' -> 'box of rocks' 'boxes of rocks'
  'sarcophag[us|i] -> 'sarcophagus' 'sarcophagi'"
  (let ((start (position #\[ text)))
    (if (null start)
        (values text (strcat (guess-plural text)))
        (let* ((sep (position #\| text :start start))
               (end (position #\] text :start start))
               (prefix (subseq text 0 start))
               (suffix (subseq text (+ end 1))))
          (if (null sep)
              (values
               (strcat prefix suffix)
               (strcat prefix (subseq text (+ start 1) end) suffix))
              (values
               (strcat prefix (subseq text (+ start 1) sep) suffix)
               (strcat prefix (subseq text (+ sep 1) end) suffix)))))))

(defstruct noun
  article singular plural)

(defun parse-noun (text)
  (if (upper-case-p (char text 0))
      (make-noun :singular text)
      (multiple-value-bind (article noun) (split-article text)
        (multiple-value-bind (singular plural) (make-singular-and-plural noun)
          (make-noun :article (or article (guess-article noun))
                     :singular singular
                     :plural plural)))))

(defun format-noun (noun &key (count 1) (article :indefinite) (capitalize nil))
  (let ((s (cond
             ;; If it has no article, it's a proper noun and always appears as-is.
             ((not (noun-article noun))
              (noun-singular noun))
             ;; In the singular case, prepend the appropriate article.
             ((eql count 1)
              (case article
                (:definite (format nil "the ~a" (noun-singular noun)))
                (:indefinite (format nil "~a ~a" (noun-article noun) (noun-singular noun)))
                (t (noun-singular noun))))
             ;; In the plural case, prepend the actual number unless count is t, in which
             ;; case optionally prepend the definite article.
             ((eq count t)
              (if (eq article :definite)
                  (format nil "the ~a" (noun-plural noun))
                  (noun-plural noun)))
             (t
              (format nil "~a ~a" count (noun-plural noun))))))
    (when capitalize
      (setf (elt s 0) (char-upcase (elt s 0))))
    s))

(defstruct verb
  singular plural)

(defun parse-verb (text)
  (if (null (position #\[ text))
      (let* ((sep (or (position #\space text) (length text)))
             (prefix (subseq text 0 sep))
             (plural (if (string= prefix "is") "are" (guess-singular prefix))))
        (make-verb :singular text :plural (strcat plural (subseq text sep))))
      (multiple-value-bind (singular plural) (make-singular-and-plural text)
        (make-verb :singular singular :plural plural))))

(defun format-verb (verb &key (count 1))
  (if (= count 1) (verb-singular verb) (verb-plural verb)))

(defun special-token-p (ch)
  (not (alpha-char-p ch)))

(defun tokenize-input (input)
  "Returns a list containing the sequence of tokens in a user input string."
  ;; Split the input into an optional leading special character; runs of
  ;; non-space, non-separators; and individual separators.
  (let ((trimmed (string-trim #(#\Space #\Tab #\Return #\Newline) input)))
    (cl-ppcre:all-matches-as-strings "(^[^\\w\\s])|[^\\s,.:;?!]+|[,.:;?!]" trimmed)))

(defun parse-quantity (token)
  "Interprets `token` as a possible quantity. Returns either an integer, :all,
  or nil if the token does not describe a quantity."
  (multiple-value-bind (num length) (parse-integer token :junk-allowed t)
    (cond
      ((= length (length token)) num)
      ((article-p token) 1)
      ((or (string-equal token "all") (string-equal token "every")) :all))))

(defun split-quantity (tokens)
  "Returns two values. If the first token indicates a quantity, the primary
  value is the rest of the tokens, and the secondary is the indicated quantity.
  Otherwise, the primary value is `tokens` and the secondary value is nil."
  (if (null tokens)
      nil
      (let ((quantity (parse-quantity (car tokens))))
        (if (null quantity)
            tokens
            (values (cdr tokens) quantity)))))

(defun format-list (items &key fn (conjunction "and"))
  "Returns a string that formats `items` as a comma-separated list, using
  `conjunction` before the last item with an Oxford comma as appropriate. If
  `fn` is not null, it is applied to each item before formatting."
  (let ((items (if fn (mapcar fn items) items)))
    (case (length items)
      (0 "")
      (1 (car items))
      (2 (format nil "~a ~a ~a" (car items) conjunction (cadr items)))
      (t (format nil "~{~a~^, ~}, ~a ~a" (butlast items) conjunction (car (last items)))))))

(defun split-list (tokens)
  "Given a sequence of tokens that represents one or more objects separated by
  commas or 'and', returns a list where each item is the subsequence of tokens
  associated with a single object."
  (do (objects)
      ((null tokens) (nreverse objects))
    (if-let ((sep (position-if #'(lambda (token)
                                   (or (string-equal token "and")
                                       (string= token ",")))
                               tokens)))
      (progn
        (push (subseq tokens 0 sep) objects)
        (setf tokens (subseq tokens (1+ sep))))
      (progn
        (push tokens objects)
        (setf tokens nil)))))
