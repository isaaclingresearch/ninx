(in-package :decklm)

(defmacro silence-errors (query)
  "since tables can be created on the go, it's important to handle all errors arising from those operations and that they fail
silently"
  `(handler-case ,query
     (error (err)
       (format t "Error from db from. ~%Query: ~a ~%Error: ~a~% " ,query err)
       (declare (ignore err)))))

;; COOKIES

(defun cookies-table ()
  "this creates the cookie table"
  (silence-errors (query (:create-table 'cookies
					((cookie :type (varchar 36) :primary-key t :default (gen-random-uuid))
					 (user-id :type (varchar 36))
					 (saved-on :type timestamp-without-time-zone :default (now)))))))

(defun expire-cookies)
(defun emails-table ()
  "holds user email and user-id information"
  (silence-errors (query (:create-table 'emails
					((email :type string :primary-key t)
					 (user-id :type (varchar 36) :default (gen-random-uuid))
					 (saved-on :type timestamp-without-time-zone :default (now)))))))

(defun consent-table ()
  "holds the consent for the user"
  (silence-errors (query (:create-table 'consent
					((user-id :type (varchar 36 :primary-key t))
					 (consent :type boolean))))))

(defun access-codes-table ()
  "holds the access code for the user, 
the access codes expire after 24 hours, we create a function to delete older ones and a trigger to call it every hour."
  (silence-errors
   (query (:create-table 'access-codes
			 ((user-id :type (varchar 36 :primary-key t))
			  (access-code :type smallint)
			  (saved-on :type timestamp-without-time-zone :Default (now)))))))

(defun create-tables ()
  (cookies-table)
  (emails-table)
  (consent-table)
  (access-codes-tables))
