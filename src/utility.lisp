(in-package :charm)

(defun as-keyword (symbol) (intern (string symbol) :keyword))

(defmacro dopairs ((first second list-form) &body body)
  "Like dolist but each iteration consumes two values from the list."
  (let ((list-var (gensym)))
    `(let ((,list-var ,list-form))
       (loop while ,list-var do
            (let* ((,first (pop ,list-var)) (,second (pop ,list-var)))
              ,@body)))))

(defun string-replace-keys (message &rest keys-values)
  (dopairs (key value keys-values)
    (let ((pattern (concatenate 'string "(?i)\\$" (string key))))
      (setf message (cl-ppcre:regex-replace-all pattern message value))))
  message)

(defun normalize-whitespace (text)
  "Removes spaces before and after newlines, and replaces each sequence of
multiple spaces with a single space."
  (cl-ppcre:regex-replace-all
   " +"
   (cl-ppcre:regex-replace-all " *\\n *" text (string #\Newline))
   " "))

(defun pophash (key hash-table &optional default)
  "Removes the value associated with `key` from `hash-table` and returns it. If
there was no such value, returns `default`."
  (let ((value (gethash key hash-table default)))
    (remhash key hash-table)
    value))

(defun listify (value)
  (if (listp value) value (list value)))

(defun strcat (&rest strings)
  (apply #'concatenate 'string strings))

(defun string-starts-with (string prefix)
  (and (<= (length prefix) (length string))
       (string= string prefix :end1 (length prefix))))

(defun string-ends-with (string suffix)
  (let ((start (- (length string) (length suffix))))
    (and (>= start 0) (string= string suffix :start1 start))))

(defun split-words (text)
  "Splits a string into a list of words."
  (delete " " (sb-unicode:words text) :test 'equal))

(defun join-words (words)
  "Joins words into a string, with a space separating adjacent words."
  (format nil "~{~a~^ ~}" words))

(defun lowercase-words (words)
  (mapcar #'sb-unicode:lowercase words))

(defun keep-if (fn sequence)
  "Returns a list containing all the non-nil results of evaluating `(fn x)` for
  each `x` in `sequence`."
  (delete nil (mapcar fn sequence)))

(defun find-if-type (type sequence)
  (find-if #'(lambda (x) (eq (type-of x) type)) sequence))

(defun require-type (object type)
  (and (typep object type) object))

;;; Allow reading hash tables using { key value ... }

(defun set-hash-values (hash pairs)
  (dopairs (key value pairs)
    (setf (gethash key hash) value)))

(defun read-hash-table (stream char)
  (declare (ignore char))
  `(let ((hash (make-hash-table)))
     (set-hash-values hash (list ,@(read-delimited-list #\} stream t)))
     hash))

(set-macro-character #\{ #'read-hash-table)
(set-syntax-from-char #\} #\))
