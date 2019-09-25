(in-package :charm)

(defparameter *db* nil)

(defun open-database (root-directory)
  (setf *db* (sqlite:connect (merge-pathnames  "charm.db" root-directory))))

(defun close-database ()
  (sqlite:disconnect *db*)
  (setf *db* nil))

(defun hash-password (password)
  "Returns a derived hash of the `password` using random salt."
  (let ((salt (ironclad:make-random-salt))
        (password-bytes (babel:string-to-octets password :encoding :utf-8)))
    (ironclad:pbkdf2-hash-password-to-combined-string
     password-bytes :salt salt :digest :sha1 :iterations 10000)))

(defun validate-username (username)
  "Checks that a proposed username meets some arbitrary requirements. Returns
nil if the password is valid, or a user-facing string describing why the
password is invalid."
  (cond
    ((< (length username) 3)
     "Username must be at least 3 characters long.")
    ((> (length username) 20)
     "Username may be no more than 20 characters long.")))

(defun validate-password (password)
  "Checks that a proposed password meets some arbitrary requirements. Returns
nil if the password is valid, or a user-facing string describing why the
password is invalid."
  (cond
    ((< (length password) 8)
     "Password must be at least 8 characters long.")
    ((> (length password) 128)
     "Password may be no more than 128 characters long.")))

(defun create-account (username password avatar)
  "Tries to create an account. Returns t on success or nil on failure, as when
the username is already in use."
  (let ((hashed-password (hash-password password))
        (encoded-avatar (encode avatar)))
    (handler-case
        (progn
          (sqlite:execute-non-query
           *db*
           "insert into accounts (username, password, avatar) values (?, ?, ?)"
           username hashed-password encoded-avatar)
          t)
      (sqlite:sqlite-error (err)
        (format-log :info "cannot create account: ~s" err)
        nil))))

(defun delete-account (username)
  (sqlite:execute-non-query
   *db* "delete from accounts where username = ?" username))

(defun authenticate (username password)
  "Returns t if `password` matches the hashed password stored for `username`, or
nil otherwise."
  (let ((hashed-password (sqlite:execute-single
                          *db*
                          "select password from accounts where username = ?"
                          username)))
    (and hashed-password
         (ironclad:pbkdf2-check-password (babel:string-to-octets password :encoding :utf-8)
                                         hashed-password))))

(defun change-password (username old-password new-password)
  (when (authenticate username old-password)
    (let ((hashed-password (hash-password new-password)))
      (sqlite:execute-non-query
       *db*
       "update accounts set password = ? where username = ?" hashed-password username)
      t)))

(defun load-avatar (username)
  (let ((data (sqlite:execute-single
               *db* "select avatar from accounts where username = ?" username)))
    (decode data)))

(defun save-avatar (username avatar)
  (sqlite:execute-non-query
   *db*
   "update accounts set avatar = ? where username = ?" (encode avatar) username))
