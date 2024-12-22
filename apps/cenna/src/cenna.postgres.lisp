(in-package :cenna)

(def-suite :cenna-postgres)

(defmacro conn ((database) &body data)
  `(with-connection (list ,database ,database ,(uiop:getenv "POSTGRES_PASSWORD") "localhost")
     ,@data))

(test start-tests
  (is (equalp "postgres" (change-toplevel-database "postgres" "postgres" (uiop:getenv "POSTGRES_PASSWORD") "localhost"))))
(test delete-db (is (null (drop-database "cenna_testdb"))))
(test drop-role (is (not (null (drop-role "cenna_testdb")))))

(defun initialise-db (role &optional (database "decklm") create-tables)
  "this function will create the database and the appropriate tables."
  (let ((password (uiop:getenv "POSTGRES_PASSWORD")))
    (change-toplevel-database "postgres" "postgres" password "localhost")
    (create-role role password :base-role :admin)
    (create-database database :owner role)
    (when create-tables
      (change-toplevel-database role database password "localhost")
      (create-tables))))

(defparameter *db-string* "cenna" "This is the database name currently in use, we need this to reduce code and make tests work.")
(test initialise-db
  (is (null (prog1 (initialise-db "cenna_testdb" "cenna_testdb" t)
	      (setf *db-string* "cenna_testdb")))))

(defun create-tables ()
  "this will create tables for all our data"
  (conn (*db-string*)
	;; Users table (immutable data)
	(query (:create-table (:if-not-exists 'users)
			      ((id :type uuid :primary-key t :default (:raw "gen_random_uuid()"))
			       (date-of-birth :type timestamp-without-time-zone)
			       (sex-assigned-at-birth :type text)
			       (race-ethnicity :type text)
			       (country-of-birth :type text)
			       (created-at :type timestamp-with-time-zone :default (:raw "now()")))))

	;; Full Name History
	(query (:create-table (:if-not-exists 'user_full_names)
			      ((id :type uuid :primary-key t :default (:raw "gen_random_uuid()"))
			       (user-id :type uuid :references ((users id) :cascade :cascade))
			       (full-name :type text)
			       (valid-from :type timestamp-with-time-zone :default (:raw "now()")))))

	;; Marital Status History
	(query (:create-table (:if-not-exists 'user_marital_statuses)
			      ((id :type uuid :primary-key t :default (:raw "gen_random_uuid()"))
			       (user-id :type uuid :references ((users id) :cascade :cascade))
			       (marital-status :type text)
			       (valid-from :type timestamp-with-time-zone :default (:raw "now()")))))

	;; Occupation History
	(query (:create-table (:if-not-exists 'user_occupations)
			      ((id :type uuid :primary-key t :default (:raw "gen_random_uuid()"))
			       (user-id :type uuid :references ((users id) :cascade :cascade))
			       (occupation :type text)
			       (valid-from :type timestamp-with-time-zone :default (:raw "now()")))))

	;; Current Location History
	(query (:create-table (:if-not-exists 'user_locations)
			      ((id :type uuid :primary-key t :default (:raw "gen_random_uuid()"))
			       (user-id :type uuid :references ((users id) :cascade :cascade))
			       (country :type text)
			       (city :type text)
			       (region :type text)
			       (valid-from :type timestamp-with-time-zone :default (:raw "now()")))))

	;; Contacts History (can have multiple contacts over time)
	(query (:create-table (:if-not-exists 'user_contacts)
			      ((id :type uuid :primary-key t :default (:raw "gen_random_uuid()"))
			       (user-id :type uuid :references ((users id) :cascade :cascade))
			       (telephone-number :type text)
			       (email :type text)
			       (valid-from :type timestamp-with-time-zone :default (:raw "now()")))))

	;; Next of Kin History (can have multiple next of kin over time)
	(query (:create-table (:if-not-exists 'user_next_of_kin)
			      ((id :type uuid :primary-key t :default (:raw "gen_random_uuid()"))
			       (user-id :type uuid :references ((users id) :cascade :cascade))
			       (name :type text)
			       (email :type text)
			       (telephone-number :type text)
			       (relationship :type text)
			       (valid-from :type timestamp-with-time-zone :default (:raw "now()")))))

	;; Gender Identity History
	(query (:create-table (:if-not-exists 'user_genders)
			      ((id :type uuid :primary-key t :default (:raw "gen_random_uuid()"))
			       (user-id :type uuid :references ((users id) :cascade :cascade))
			       (gender-identity :type text)
			       (valid-from :type timestamp-with-time-zone :default (:raw "now()")))))
	))

(test create-tables (is (null (create-tables))))

(defun delete-tables ()
  "delete all tables"
  (dolist (table '(users user_full_names user_occupations user_marital_statuses user_genders user_contacts user_locations user_next_of_kin))
    (conn (*db-string*) (query (:drop-table table :cascade)))))
(test delete-tables (is (null (delete-tables))))

(defun reset-tables ()
  (handler-case (delete-tables) (error (err) (declare (ignore err))))
  (create-tables))

(test reset-tables (is (null (reset-tables))))


;; ------------------- USERS TABLE -------------------

(defun insert-user (date-of-birth sex-assigned-at-birth race-ethnicity country-of-birth)
  "Inserts a new user into the users table."
  (caar (conn (*db-string*)
	      (query (:insert-into 'users
				   :set 'date-of-birth date-of-birth
				   'sex-assigned-at-birth sex-assigned-at-birth
				   'race-ethnicity race-ethnicity
				   'country-of-birth country-of-birth
				   :returning 'id)))))

(test insert-user-test
  (is (not (null (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))))

(defun get-user (user-id)
  "Retrieves a user from the users table by ID."
  (car (conn (*db-string*)
	 (query (:select '* :from 'users
		 :where (:= 'id user-id))))))

(test get-user-test
  (is (not (null (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
		   (let ((user (get-user new-user-id)))
		     (and user
			  (equalp (car user) new-user-id))))))))

;; ------------------- USER FULL NAMES TABLE -------------------

(defun insert-user-full-name (user-id full-name)
  "Inserts a new full name record for a user."
  (caar (conn (*db-string*)
	      (query (:insert-into 'user_full_names
				   :set 'user-id user-id
				   'full-name full-name
				   :returning 'id)))))

(test insert-user-full-name-test
  (is (not (null (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
		   (insert-user-full-name new-user-id "Test Name"))))))

(defun get-current-user-full-name (user-id)
  "Retrieves the current full name for a user."
  (car (conn (*db-string*)
	 (query (:limit (:order-by (:select '* :from 'user_full_names
				    :where (:= 'user-id user-id))
				   (:desc'valid-from)) 1)))))

(test get-current-user-full-name-test
  (is (not (null (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
		   (insert-user-full-name new-user-id "Initial Name")
		   (insert-user-full-name new-user-id "Current Name")
		   (let ((current-name (get-current-user-full-name new-user-id)))
		     (equalp (third current-name) "Current Name")))))))

(defun get-user-full-name-history (user-id)
  "Retrieves the full name history for a user."
  (conn (*db-string*)
	(query (:order-by (:select '* :from 'user_full_names
				   :where (:= 'user-id user-id))
			  'valid-from))))

(test get-user-full-name-history-test
  (is (not (null (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
		   (insert-user-full-name new-user-id "Name 1")
		   (insert-user-full-name new-user-id "Name 2")
		   (let ((history (get-user-full-name-history new-user-id)))
		     (and (= (length history) 2)
			  (equalp (third (first history)) "Name 1")
			  (equalp (third (second history)) "Name 2"))))))))

;; ------------------- USER MARITAL STATUSES TABLE -------------------

(defun insert-user-marital-status (user-id marital-status)
  "Inserts a new marital status record for a user."
  (caar (conn (*db-string*)
	      (query (:insert-into 'user_marital_statuses
				   :set 'user-id user-id
				   'marital-status marital-status
				   :returning 'id)))))

(test insert-user-marital-status-test
  (is (not (null (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
		   (insert-user-marital-status new-user-id "Single"))))))

(defun get-current-user-marital-status (user-id)
  "Retrieves the current marital status for a user."
  (car (conn (*db-string*)
	 (query (:limit (:order-by (:select '* :from 'user_marital_statuses
				    :where (:= 'user-id user-id))
				   (:desc 'valid-from))
			1)))))

(test get-current-user-marital-status-test
  (is (not (null (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
		   (insert-user-marital-status new-user-id "Single")
		   (insert-user-marital-status new-user-id "Married")
		   (let ((current-status (get-current-user-marital-status new-user-id)))
		     (equalp (third current-status) "Married")))))))

(defun get-user-marital-status-history (user-id)
  "Retrieves the marital status history for a user."
  (conn (*db-string*)
	(query (:order-by (:select '* :from 'user_marital_statuses
				   :where (:= 'user-id user-id))
			  'valid-from))))

(test get-user-marital-status-history-test
  (is (not (null (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
		   (insert-user-marital-status new-user-id "Single")
		   (insert-user-marital-status new-user-id "Divorced")
		   (let ((history (get-user-marital-status-history new-user-id)))
		     (and (= (length history) 2)
			  (equalp (third (first history)) "Single")
			  (equalp (third (second history)) "Divorced"))))))))

;; ------------------- USER OCCUPATIONS TABLE -------------------

(defun insert-user-occupation (user-id occupation)
  "Inserts a new occupation record for a user."
  (caar (conn (*db-string*)
	      (query (:insert-into 'user_occupations
				   :set 'user-id user-id
				   'occupation occupation
				   :returning 'id)))))

(test insert-user-occupation-test
  (is (not (null (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
		   (insert-user-occupation new-user-id "Tester"))))))

(defun get-current-user-occupation (user-id)
  "Retrieves the current occupation for a user."
  (car (conn (*db-string*)
	 (query (:limit (:order-by (:select '* :from 'user_occupations
				    :where (:= 'user-id user-id))
				   (:desc 'valid-from))
			1)))))

(test get-current-user-occupation-test
  (is (not (null (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
		   (insert-user-occupation new-user-id "Teacher")
		   (insert-user-occupation new-user-id "Engineer")
		   (let ((current-occupation (get-current-user-occupation new-user-id)))
		     (equalp (third current-occupation) "Engineer")))))))

(defun get-user-occupation-history (user-id)
  "Retrieves the occupation history for a user."
  (conn (*db-string*)
	(query (:order-by (:select '* :from 'user_occupations
				   :where (:= 'user-id user-id))
			  'valid-from))))

(test get-user-occupation-history-test
  (is (not (null (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
		   (insert-user-occupation new-user-id "Artist")
		   (insert-user-occupation new-user-id "Writer")
		   (let ((history (get-user-occupation-history new-user-id)))
		     (and (= (length history) 2)
			  (equalp (third (first history)) "Artist")
			  (equalp (third (second history)) "Writer"))))))))

;; ------------------- USER LOCATIONS TABLE -------------------

(defun insert-user-location (user-id country city region)
  "Inserts a new location record for a user."
  (caar (conn (*db-string*)
	      (query (:insert-into 'user_locations
				   :set 'user-id user-id
				   'country country
				   'city city
				   'region region
				   :returning 'id)))))

(test insert-user-location-test
  (is (not (null (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
		   (insert-user-location new-user-id "USA" "Test City" "Test Region"))))))

(defun get-current-user-location (user-id)
  "Retrieves the current location for a user."
  (car (conn (*db-string*)
	 (query (:limit (:order-by (:select '* :from 'user_locations
				    :where (:= 'user-id user-id))
				   (:desc 'valid-from))
			1)))))

(test get-current-user-location-test
  (is (not (null (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
		   (insert-user-location new-user-id "Canada" "Toronto" "ON")
		   (insert-user-location new-user-id "UK" "London" "England")
		   (let ((current-location (get-current-user-location new-user-id)))
		     (equalp (fourth current-location) "London")))))))

(defun get-user-location-history (user-id)
  "Retrieves the location history for a user."
  (conn (*db-string*)
	(query (:order-by (:select '* :from 'user_locations
				   :where (:= 'user-id user-id))
			  'valid-from))))

(test get-user-location-history-test
  (is (not (null (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
			(insert-user-location new-user-id "Germany" "Berlin" "Berlin")
			(insert-user-location new-user-id "France" "Paris" "Ile-de-France")
			(let ((history (get-user-location-history new-user-id)))
			  (and (= (length history) 2)
			       (equalp (fourth (first history)) "Berlin")
			       (equalp (fourth (second history)) "Paris"))))))))

;; ------------------- USER CONTACTS TABLE -------------------

(defun insert-user-contact (user-id telephone-number email)
  "Inserts a new contact record for a user."
  (caar (conn (*db-string*)
	      (query (:insert-into 'user_contacts
				   :set 'user-id user-id
				   'telephone-number telephone-number
				   'email email
				   :returning 'id)))))

(test insert-user-contact-test
  (is (not (null (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
		       (insert-user-contact new-user-id "123-456-7890" "test@example.com"))))))

(defun get-current-user-contacts (user-id)
  "Retrieves the current contact information for a user (latest entry)."
  (car (conn (*db-string*)
	 (query (:limit (:order-by (:select '* :from 'user_contacts
				    :where (:= 'user-id user-id))
				   (:desc 'valid-from))
			1)))))

(test get-current-user-contacts-test
  (is (not (null
	    (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
	      (insert-user-contact new-user-id "111-111-1111" "old@example.com")
	      (insert-user-contact new-user-id "222-222-2222" "new@example.com")
	      (let ((current-contact (get-current-user-contacts new-user-id)))
		(equalp (fourth current-contact) "new@example.com")))))))

(defun get-user-contact-history (user-id)
  "Retrieves the contact history for a user."
  (conn (*db-string*)
	(query (:order-by (:select '* :from 'user_contacts
				   :where (:= 'user-id user-id))
			  'valid-from))))

(test get-user-contact-history-test
  (is (not (null
	    (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
	      (insert-user-contact new-user-id "333-333-3333" "first@example.com")
	      (insert-user-contact new-user-id "444-444-4444" "second@example.com")
	      (let ((history (get-user-contact-history new-user-id)))
		(and (= (length history) 2)
		     (equalp (fourth (first history)) "first@example.com")
		     (equalp (fourth (second history)) "second@example.com"))))))))

;; ------------------- USER NEXT OF KIN TABLE -------------------

(defun insert-user-next-of-kin (user-id name email telephone-number relationship)
  "Inserts a new next of kin record for a user."
  (caar (conn (*db-string*)
	      (query (:insert-into 'user_next_of_kin
				   :set 'user-id user-id
				   'name name
				   'email email
				   'telephone-number telephone-number
				   'relationship relationship
				   :returning 'id)))))

(test insert-user-next-of-kin-test
  (is (not (null (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
		   (insert-user-next-of-kin new-user-id "Test NOK" "nok@example.com" "555-555-5555" "Friend"))))))

(defun get-current-user-next-of-kin (user-id)
  "Retrieves the current next of kin information for a user (latest entry)."
  (car (conn (*db-string*)
	 (query (:limit (:order-by (:select '* :from 'user_next_of_kin
				    :where (:= 'user-id user-id))
				   (:desc 'valid-from))
			1)))))

(test get-current-user-next-of-kin-test
  (is (not (null
	    (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
	      (insert-user-next-of-kin new-user-id "Old NOK" "oldnok@example.com" "666-666-6666" "Sibling")
	      (insert-user-next-of-kin new-user-id "New NOK" "newnok@example.com" "777-777-7777" "Parent")
	      (let ((current-nok (get-current-user-next-of-kin new-user-id)))
		(equalp (third current-nok) "New NOK")))))))

(defun get-user-next-of-kin-history (user-id)
  "Retrieves the next of kin history for a user."
  (conn (*db-string*)
	(query (:order-by (:select '* :from 'user_next_of_kin
				   :where (:= 'user-id user-id))
			  'valid-from))))

(test get-user-next-of-kin-history-test
  (is (not (null
	    (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
	      (insert-user-next-of-kin new-user-id "NOK A" "a@example.com" "888-888-8888" "Cousin")
	      (insert-user-next-of-kin new-user-id "NOK B" "b@example.com" "999-999-9999" "Friend")
	      (let ((history (get-user-next-of-kin-history new-user-id)))
		(and (= (length history) 2)
		     (equalp  (third (first history)) "NOK A")
		     (equalp (third (second history)) "NOK B"))))))))

;; ------------------- USER GENDERS TABLE -------------------

(defun insert-user-gender (user-id gender-identity)
  "Inserts a new gender identity record for a user."
  (caar (conn (*db-string*)
	      (query (:insert-into 'user_genders
				   :set 'user-id user-id
				   'gender-identity gender-identity
				   :returning 'id)))))

(test insert-user-gender-test
  (is (not (null (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
		   (insert-user-gender new-user-id "Man"))))))

(defun get-current-user-gender (user-id)
  "Retrieves the current gender identity for a user."
  (car (conn (*db-string*)
	 (query (:limit (:order-by (:select '* :from 'user_genders
				    :where (:= 'user-id user-id))
				   (:desc 'valid-from))
			1)))))

(test get-current-user-gender-test
  (is (not (null
	    (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
	      (insert-user-gender new-user-id "Male")
	      (insert-user-gender new-user-id "Non-binary")
	      (let ((current-gender (get-current-user-gender new-user-id)))
		(equalp (third current-gender) "Non-binary")))))))

(defun get-user-gender-history (user-id)
  "Retrieves the gender identity history for a user."
  (conn (*db-string*)
	(query (:order-by (:select '* :from 'user_genders
				   :where (:= 'user-id user-id))
			  'valid-from))))

(test get-user-gender-history-test
  (is (not (null
	    (let ((new-user-id (insert-user "1999-01-25" "Male" "Test Race" "Test Country")))
	      (insert-user-gender new-user-id "Female")
	      (insert-user-gender new-user-id "Agender")
	      (let ((history (get-user-gender-history new-user-id)))
		(and (= (length history) 2)
		     (equalp (third (first history)) "Female")
		     (equalp (third (second history)) "Agender"))))))))

;; last test returns to decklm db
(test return-to-decklm-db (is (equal "cenna"
				     (prog1
					 (change-toplevel-database "cenna" "cenna" (uiop:getenv "POSTGRES_PASSWORD") "localhost")
				       (setf *db-string* "cenna")))))
