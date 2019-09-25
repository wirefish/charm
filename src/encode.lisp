(in-package :charm)

;;;

(defgeneric encode-slot (slot-name value))

(defmethod encode-slot (slot-name value)
  value)

;;;

(defgeneric encode-object (object stream))

(defmethod encode-object (object stream)
  (write object :stream stream :pretty nil :readably t))

(defmethod encode-object ((object standard-object) stream)
  (let ((class (find-class (type-of object))))
    (format stream "{~s" (class-name class))
    (dolist (slot (class-slots class))
      (when (eq (slot-definition-allocation slot) :instance)
        (let ((name (slot-definition-name slot)))
          (when (slot-boundp object name)
            (format stream " :~s " name)
            (encode-object (encode-slot name (slot-value object name)) stream)))))
    (write-char #\} stream)
    object))

(defmethod encode-object ((object list) stream)
  (if (null object)
      (write nil :stream stream)
      (progn
        (write-char #\( stream)
        (loop for cons on object
              for i from 0
              do
                 (when (> i 0) (write-char #\Space stream))
                 (encode-object (car cons) stream)
                 (when (and (cdr cons) (not (consp (cdr cons))))
                   (write-string " . " stream)
                   (encode-object (cdr cons) stream)))
        (write-char #\) stream)))
  object)

(defmethod encode-object ((object string) stream)
  (write object :stream stream))

(defmethod encode-object ((object vector) stream)
  (write-string "#(" stream)
  (loop for value across object
        for i from 0
        do
           (when (> i 0) (write-char #\Space stream))
           (encode-object value stream))
  (write-char #\) stream))

(defmethod encode-object ((object hash-table) stream)
  (format stream "{HASH-TABLE")
  (maphash #'(lambda (key value)
               (write-char #\Space stream)
               (encode-object key stream)
               (write-char #\Space stream)
               (encode-object value stream))
           object)
  (write-char #\} stream)
  object)

(defun encode (object)
  (with-output-to-string (s)
    (encode-object object s)))

;;;

(defgeneric decode-slot (slot-name value))

(defmethod decode-slot (slot-name value)
  value)

;;;

(defgeneric decode-object (class args))

(defmethod decode-object (class args)
  (let ((object (allocate-instance (find-class class))))
    (doplist (name value args)
        (let ((slot (find-symbol (symbol-name name))))
          (setf (slot-value object slot) (decode-slot slot value))))
    object))

(defmethod decode-object ((class (eql 'hash-table)) args)
  (plist-hash-table args))

(defun decode (s)
  (labels ((read-object (stream char)
             (declare (ignore char))
             (let ((args (read-delimited-list #\} stream t)))
               (decode-object (first args) (rest args)))))
    (let ((*readtable* (copy-readtable)))
      (set-macro-character #\{ #'read-object)
      (set-syntax-from-char #\} #\))
      (with-input-from-string (stream s)
        (read stream)))))
