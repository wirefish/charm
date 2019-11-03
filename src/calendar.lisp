(in-package :charm)

(defparameter *days-per-month* 30)

(defparameter *months*
  #(("Nisanu" "early spring")
    ("Aru" "mid-spring")
    ("Simanu" "late spring")
    ("Dumuzu" "early summer")
    ("Abu" "mid-summer")
    ("Ululu" "late summer")
    ("Tisitum" "early autumn")
    ("Samna" "mid-autumn")
    ("Kislimu" "late autumn")
    ("Tebetum" "early winter")
    ("Sabatu" "mid-winter")
    ("Addaru" "late winter")))

(defparameter *days-per-year* (* *days-per-month* (length *months*)))

(defparameter *hours*
  #(("dawn" :morning)
    ("morning" :morning)
    ("noontide" :day)
    ("afternoon" :day)
    ("evening" :evening)
    ("dusk" :evening)
    ("night" :night)
    ("pre-dawn" :night)))

(defparameter *real-seconds-per-day* (* 3600 5))

(defparameter *year-offset* 300)

(defun get-current-time ()
  (multiple-value-bind (days-since-epoch partial-day)
      (floor (/ (get-universal-time) *real-seconds-per-day*))
    (multiple-value-bind (year day-in-year) (floor days-since-epoch *days-per-year*)
      (multiple-value-bind (month day-in-month) (floor day-in-year *days-per-month*)
        (let ((hour (floor (* partial-day (length *hours*)))))
          (values (+ year *year-offset*) month day-in-month hour))))))

(defcommand (actor ("date"))
  "Display the current date and time. See `help:calendar` for more information."
  (multiple-value-bind (year month day hour) (get-current-time)
    (show-text actor "It is ~a on the ~:r day of ~a in the year ~d."
               (first (elt *hours* hour))
               (1+ day)
               (first (elt *months* month))
               year)))
