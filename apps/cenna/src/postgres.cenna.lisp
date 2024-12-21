(in-package :cenna)

(defparameter *db-string* "cenna" "This is the database name currently in use, we need this to reduce code and make tests work.")

(defun create-tables ()
  "this will create tables for all our data"
  (conn (*db-string*)
	(query (:create-table (:if-not-exists 'demographics)
			      ((id :type uuid primary-key t default (:raw "gen_random_uuid()"))
			       (date-of-birth :type date-without-timezone)
			       (sex-assigned-at-birth :type (:enum '("Male" "Female" "Unknown")))
			       (gender-identity :type text) ;; Allow for broader identities
			       (race-ethnicity :type text) ;; More inclusive term
			       (country-of-birth :type text) ;; Helpful for understanding origin
			       (highest-education :type text)
			       (marital-status :type (:enum '("Single" "Married" "Divorced" "Widowed" "Separated" "Other")))
			       (occupation :type text)
			       (current-country :type text) ;; More specific location
			       (current-city :type text) ;; More specific location
			       (current-region :type text)
			       (telephone-number :type text)
			       (email :type text)
			       (created-at :type timestamp-with-timezone :default (:raw "now()"))
			       (updated-at :type timestamp-with-timezone :default (:raw "now()")))))))

(defun create-demographics (data)
  "insert a new raw into the demographics table, data is given as a plist"
  (conn (*db-string*)
	(query (:insert-into 'demographics :set
			     'date-of-birth (getf data :date-of-birth)
			     'sex-assigned-at-birth (getf data :sex)
			     'gender-identity (getf data :gender)
			     'race-ethnicity (getf data :race)
			     'country-of-birth (getf data :country-of-birth)
			     'highest-education (getf data :highest-education)
			     'marital-status (getf data :marital-education)
			     'occupation (getf data :occupation)
			     'current-country (getf data :current-country)
			     'current-city (getf data :current-city)
			     'current-region (getf data :current-region)
			     'telephone-number (getf data :telephone-number)
			     'email (getf data :email)))))

(defun get-demographics (&optional id)
  "if id is specified, return demographics for it; otherwise return all"
  (conn (*db-string*)
	(query (if id
		   (:select '* :from 'demographics :where (:= 'id id))
		   (:select '* :from 'demographics)))))
