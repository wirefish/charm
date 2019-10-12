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
  "Checks that a proposed username meets some arbitrary requirements. Returns a
  string describing the problem with the username, or nil if the username is
  valid."
  ;; NOTE: These constraints must match those in index.js.
  (cond
    ((< (length username) 3)
     "Username must be at least 3 characters long.")
    ((> (length username) 40)
     "Username may be no more than 40 characters long.")
    ((not (cl-ppcre:scan "^[\\w.,@+-]+$" username))
     "Invalid character in username.")))

(defun validate-password (password)
  "Checks that a proposed password meets some arbitrary requirements. Returns a
  string describing the problem with the password, or nil if the password is
  valid."
  (cond
    ((< (length password) 8)
     "Password must be at least 8 characters long.")
    ((> (length password) 100)
     "Password may be no more than 100 characters long.")
    ((find-if-not #'graphic-char-p password)
     "Invalid character in password.")))

(defun create-account (username password avatar)
  "Tries to create an account. Returns the account ID on success or nil on
  failure, as when the username is already in use."
  (let ((hashed-password (hash-password password))
        (encoded-avatar (encode avatar))
        (location (location avatar)))
    (handler-case
        (sqlite:with-transaction *db*
          (sqlite:execute-non-query
           *db*
           "insert into accounts (username, password) values (?, ?)"
           username hashed-password)
          (let ((account-id (sqlite:last-insert-rowid *db*)))
            (print account-id)
            (sqlite:execute-non-query
             *db*
             "insert into avatars (account_id, location, avatar) values (?, ?, ?)"
             account-id (encode (type-of location)) encoded-avatar)
            account-id))
      (sqlite:sqlite-error (err)
        (format-log :info "cannot create account: ~s" err)
        nil))))

(defun find-account-id (username)
  "Returns the account ID associated with `username`, or nil if no such account
  exists."
  (sqlite:execute-single
   *db*
   "select account_id from accounts where username = ?" username))

(defun delete-account (account-id)
  "Deletes the account and all avatars associated with `account-id`. Returns t
  on success or nil if an error occurs."
  (handler-case
      (sqlite:with-transaction *db*
        (sqlite:execute-non-query
         *db*
         "delete from avatars where account_id = ?" account-id)
        (sqlite:execute-non-query
         *db*
         "delete from accounts where account_id = ?" account-id)
        t)
    (sqlite:sqlite-error (err)
      (format-log :warning "cannot delete account: ~s" err)
      nil)))

(defun authenticate (username password)
  "Returns the account ID if `password` matches the hashed password stored for
  the account associated with `username`, or nil otherwise."
  (multiple-value-bind (account-id hashed-password)
      (sqlite:execute-one-row-m-v
       *db*
       "select account_id, password from accounts where username = ?"
       username)
    (when (and hashed-password
               (ironclad:pbkdf2-check-password (babel:string-to-octets password :encoding :utf-8)
                                               hashed-password))
      account-id)))

(defun change-password (username old-password new-password)
  "Changes the password for the account associated with `username` from
  `old-password` to `new-password`. Returns the account ID on success or nil on
  failure."
  (when-let ((account-id (authenticate username old-password)))
    (let ((hashed-password (hash-password new-password)))
      (sqlite:execute-non-query
       *db*
       "update accounts set password = ? where account_id = ?" hashed-password account-id)
      account-id)))

(defun load-avatar (account-id)
  "Returns the avatar for `account-id`, or nil if no such avatar exists."
  ;; TODO: also load aliases and settings
  (multiple-value-bind (location avatar-data)
      (sqlite:execute-one-row-m-v
       *db*
       "select location, avatar from avatars where account_id = ?"
       account-id)
    (let ((avatar (decode avatar-data)))
      (setf (location avatar) (symbol-value (decode location)))
      avatar)))

(defun save-avatar (account-id avatar)
  "Saves the avatar for `account-id`."
  (sqlite:execute-non-query
   *db*
   "update avatars set location = ?, avatar = ? where account_id = ?"
   (encode (type-of (location avatar))) (encode avatar) account-id))

(defun save-aliases (account-id aliases)
  (let ((data (with-output-to-string (s) (prin1 aliases s))))
    (sqlite:execute-non-query
     *db*
     "update avatars set aliases = ? where account_id = ?" data account-id)))

(defun save-settings (account-id settings)
  (let ((data (with-output-to-string (s) (prin1 settings s))))
    (sqlite:execute-non-query
     *db*
     "update avatars set settings = ? where account_id = ?" data account-id)))
