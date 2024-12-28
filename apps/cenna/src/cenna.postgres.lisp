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
	;; all created at are unix timestamps presented to the table manually, no defaults.
	;; all versioned data will depend on the user-id and creation times as the primary key (user-id, created-at)

	(query (:create-table (:if-not-exists 'user-ids)
			      ((id :type uuid :primary-key t :dafault (:raw "gen_random_uuid()"))
			       (created-at :type bigint :default (:raw "extract(epoch from now())::bigint")))))
	(query (:create-table (:if-not-exists 'date-of-birth)
			      ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			       (created-at :type bigint :default (:raw "extract(epoch from now())::bigint"))
			       (date-of-birth :type timestamp-without-time-zone)
			       (primary-key user-id created-at))))
	(query (:create-table (:if-not-exists 'sex-assigned-at-birth)
			      ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			       (created-at :type bigint :default (:raw "extract(epoch from now())::bigint"))
			       (sex-assigned-at-birth :type timestamp-without-time-zone)
			       (primary-key user-id created-at))))
	(query (:create-table (:if-not-exists 'race-ethnicity)
			      ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			       (created-at :type bigint :default (:raw "extract(epoch from now())::bigint"))
			       (race-ethnicity :type timestamp-without-time-zone)
			       (primary-key user-id created-at))))
	(query (:create-table (:if-not-exists 'country-of-birth)
			      ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			       (created-at :type bigint :default (:raw "extract(epoch from now())::bigint"))
			       (country-of-birth :type timestamp-without-time-zone)
			       (primary-key user-id created-at))))
	
	(query (:create-table (:if-not-exists 'gender)
			      ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			       (gender-identity :type text)
			       (created-at :type bigint (:raw "extract(epoch from now())::bigint"))
			       (primary-key user-id created-at))))	

	(query (:create-table (:if-not-exists 'full-name)
			      ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			       (full-name :type text)
			       (created-at :type bigint (:raw "extract(epoch from now())::bigint"))
			       (primary-key user-id created-at))))

	(query (:create-table (:if-not-exists 'marital-status)
			      ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			       (marital-status :type text)
			       (created-at :type bigint (:raw "extract(epoch from now())::bigint"))
			       (primary-key user-id created-at))))

	(query (:create-table (:if-not-exists 'occupation)
			      ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			       (occupation :type text)
			       (created-at :type bigint (:raw "extract(epoch from now())::bigint"))
			       (primary-key user-id created-at))))

	;; Current Location History
	(query (:create-table (:if-not-exists 'location)
			      ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			       (country :type text)
			       (city :type text)
			       (region :type text)
			       (created-at :type bigint (:raw "extract(epoch from now())::bigint"))
			       (primary-key user-id created-at))))

	;; Contacts History (can have multiple contacts over time)
	(query (:create-table (:if-not-exists 'contact)
			      ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			       (telephone-number :type text)
			       (email :type text)
			       (created-at :type bigint (:raw "extract(epoch from now())::bigint"))
			       (primary-key user-id created-at))))

	;; Next of Kin History (can have multiple next of kin over time)
	(query (:create-table (:if-not-exists 'next-of-kin)
			      ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			       (name :type text)
			       (email :type text)
			       (telephone-number :type text)
			       (relationship :type text)
			       (created-at :type bigint (:raw "extract(epoch from now())::bigint"))
			       (primary-key user-id created-at))))

	))

(test create-tables (is (null (create-tables))))

(defun delete-tables ()
  "delete all tables"
  (dolist (table '(user-ids user_full_names user_occupations user_marital_statuses user_genders user_contacts user_locations user_next_of_kin))
    (conn (*db-string*) (query (:drop-table table :cascade)))))
(test delete-tables (is (null (delete-tables))))

(defun reset-tables ()
  (handler-case (delete-tables) (error (err) (declare (ignore err))))
  (create-tables))

(test reset-tables (is (null (reset-tables))))


;; ------------------- USER-IDS TABLE -------------------

(defun create-user-id ()
  "return a new user id"
  (caar (conn (*db-string*)
	      (query (:insert-into 'user-ids
		      :returning 'id)))))
(defparameter *test-id* nil)
(test create-user-id
      (is (not (null (let ((id (create-user-id))) (setf *test-id* id) id)))))

(defun set-date-of-birth (user-id date-of-birth)
  (conn (*db-string*)
	(query (:insert-into 'date-of-birth
			     :set 'id user-id
			     'date-of-birth date-of-birth))))
(test set-date-of-birth (is (null (set-date-of-birth *test-id* "1900-01-01"))))

(defun get-date-of-birth (user-id)
  (conn (*db-string*)
	(query (:limit (:order-by (:select 'date-of-birth :from 'date-of-birth :where (:= user-id 'user-id))) (:desc 'created-at) 1))))
(test get-date-of-birth (is (equal "1900-01-01" (get-date-of-birth *test-id*))))

(defun get-date-of-birth-all (user-id)
  (conn (*db-string*)
	(query (:select 'date-of-birth :from 'date-of-birth :where (:= user-id 'user-id)))))
(test get-date-of-birth-all (is (= 1 (length (get-date-of-birth-all *test-id*)))))

(defun set-sex-assigned-at-birth (user-id sex-assigned-at-birth)
  (conn (*db-string*)
	(query (:insert-into 'sex-assigned-at-birth
		:set 'id user-id
		'sex-assigned-at-birth sex-assigned-at-birth))))
(test set-sex-assigned-at-birth (is (null (set-sex-assigned-at-birth *test-id* "male"))))

(defun get-sex-assigned-at-birth (user-id)
  (conn (*db-string*)
	(query (:limit (:order-by (:select 'sex-assigned-at-birth :from 'sex-assigned-at-birth :where (:= user-id 'user-id))) (:desc 'created-at) 1))))
(test get-sex-assigned-at-birth (is (equal "male" (get-sex-assigned-at-birth *test-id*))))

(defun get-sex-assigned-at-birth-all (user-id)
  (conn (*db-string*)
	(query (:select 'sex-assigned-at-birth :from 'sex-assigned-at-birth :where (:= user-id 'user-id)))))
(test get-sex-assigned-at-birth-all (is (= 1 (length (get-sex-assigned-at-birth-all *test-id*)))))

(defun set-race-ethnicity (user-id race-ethnicity)
  (conn (*db-string*)
	(query (:insert-into 'race-ethnicity
		:set 'id user-id
		'race-ethnicity race-ethnicity))))
(test set-race-ethnicity (is (null (set-race-ethnicity *test-id* "african"))))

(defun get-race-ethnicity (user-id)
  (conn (*db-string*)
	(query (:limit (:order-by (:select 'race-ethnicity :from 'race-ethnicity :where (:= user-id 'user-id))) (:desc 'created-at) 1))))
(test get-race-ethnicity (is (equal "african" (get-race-ethnicity *test-id*))))

(defun get-race-ethnicity-all (user-id)
  (conn (*db-string*)
	(query (:select 'race-ethnicity :from 'race-ethnicity :where (:= user-id 'user-id)))))
(test get-race-ethnicity-all (is (= 1 (length (get-race-ethnicity-all *test-id*)))))

(defun set-country-of-birth (user-id country-of-birth)
  (conn (*db-string*)
	(query (:insert-into 'country-of-birth
		:set 'id user-id
		'country-of-birth country-of-birth))))
(test set-country-of-birth (is (null (set-country-of-birth *test-id* "uganda"))))

(defun set-gender (user-id gender)
  (conn (*db-string*)
	(query (:insert-into 'gender
		:set 'id user-id
		'gender gender))))
(test set-gender (is (null (set-gender *test-id* "male"))))

(defun get-gender (user-id)
  (conn (*db-string*)
	(query (:limit (:order-by (:select 'gender :from 'gender :where (:= user-id 'user-id))) (:desc 'created-at) 1))))
(test get-gender (is (equal "male" (get-gender *test-id*))))

(defun get-gender-all (user-id)
  (conn (*db-string*)
	(query (:select 'gender :from 'gender :where (:= user-id 'user-id)))))
(test get-gender-all (is (= 1 (length (get-gender-all *test-id*)))))

(defun set-country-of-birth (user-id country-of-birth)
  (conn (*db-string*)
	(query (:insert-into 'country-of-birth
		:set 'id user-id
		'country-of-birth country-of-birth))))
(test set-country-of-birth (is (null (set-country-of-birth *test-id* "uganda"))))

(defun get-country-of-birth (user-id)
  (conn (*db-string*)
	(query (:limit (:order-by (:select 'country-of-birth :from 'country-of-birth :where (:= user-id 'user-id))) (:desc 'created-at) 1))))
(test get-country-of-birth (is (equal "uganda" (get-country-of-birth *test-id*))))

(defun get-country-of-birth-all (user-id)
  (conn (*db-string*)
	(query (:select 'country-of-birth :from 'country-of-birth :where (:= user-id 'user-id)))))
(test get-country-of-birth-all (is (= 1 (length (get-country-of-birth-all *test-id*)))))


(defun set-full-name (user-id full-name)
  (conn (*db-string*)
	(query (:insert-into 'full-name
		:set 'id user-id
		'full-name full-name))))
(test set-full-name (is (null (set-full-name *test-id* "lubwama"))))

(defun get-full-name (user-id)
  (conn (*db-string*)
	(query (:limit (:order-by (:select 'full-name :from 'full-name :where (:= user-id 'user-id))) (:desc 'created-at) 1))))
(test get-full-name (is (equal "lubwama" (get-full-name *test-id*))))

(defun get-full-name-all (user-id)
  (conn (*db-string*)
	(query (:select 'full-name :from 'full-name :where (:= user-id 'user-id)))))
(test get-full-name-all (is (= 1 (length (get-full-name-all *test-id*)))))


(defun set-marital-status (user-id marital-status)
  (conn (*db-string*)
	(query (:insert-into 'marital-status
		:set 'id user-id
		'marital-status marital-status))))
(test set-marital-status (is (null (set-marital-status *test-id* "uganda"))))

(defun get-marital-status (user-id)
  (conn (*db-string*)
	(query (:limit (:order-by (:select 'marital-status :from 'marital-status :where (:= user-id 'user-id))) (:desc 'created-at) 1))))
(test get-marital-status (is (equal "uganda" (get-marital-status *test-id*))))

(defun get-marital-status-all (user-id)
  (conn (*db-string*)
	(query (:select 'marital-status :from 'marital-status :where (:= user-id 'user-id)))))
(test get-marital-status-all (is (= 1 (length (get-marital-status-all *test-id*)))))

(defun set-occupation (user-id occupation)
  (conn (*db-string*)
	(query (:insert-into 'occupation
		:set 'id user-id
		'occupation occupation))))
(test set-occupation (is (null (set-occupation *test-id* "uganda"))))

(defun get-occupation (user-id)
  (conn (*db-string*)
	(query (:limit (:order-by (:select 'occupation :from 'occupation :where (:= user-id 'user-id))) (:desc 'created-at) 1))))
(test get-occupation (is (equal "uganda" (get-occupation *test-id*))))

(defun get-occupation-all (user-id)
  (conn (*db-string*)
	(query (:select 'occupation :from 'occupation :where (:= user-id 'user-id)))))
(test get-occupation-all (is (= 1 (length (get-occupation-all *test-id*)))))

(defun set-location (user-id country city region)
  (conn (*db-string*)
	(query (:insert-into 'user_locations
		:set 'user-id user-id
		'country country
		'city city
		'region region))))

(test set-location-test (is (null (set-location *test-user* "USA" "Test City" "Test Region"))))


(defun get-location (user-id)
  (conn (*db-string*)
	(query (:limit (:order-by (:select '* :from 'location :where (:= user-id 'user-id))) (:desc 'created-at) 1))))
(test get-location (is (equal *test-id* (caar (get-location *test-id*)))))

(defun get-location-all (user-id)
  (conn (*db-string*)
	(query (:select '* :from 'location :where (:= user-id 'user-id)))))
(test get-location-all (is (= 1 (length (get-location-all *test-id*)))))


(defun set-contact (user-id telephone-number email)
  "Inserts a new contact record for a user."
  (conn (*db-string*)
	(query (:insert-into 'contact
		:set 'user-id user-id
		'telephone-number telephone-number
		'email email))))
(test set-contact-test
  (is (null (set-contact new-user-id "123-456-7890" "test@example.com"))))

(defun get-contact (user-id)
  (conn (*db-string*)
	(query (:limit (:order-by (:select '* :from 'contact :where (:= user-id 'user-id))) (:desc 'created-at) 1))))
(test get-contact (is (equal *test-id* (caar (get-contact *test-id*)))))

(defun get-contact-all (user-id)
  (conn (*db-string*)
	(query (:select '* :from 'contact :where (:= user-id 'user-id)))))
(test get-contact-all (is (= 1 (length (get-contact-all *test-id*)))))


(defun set-next-of-kin (user-id name email telephone-number relationship)
  (conn (*db-string*)
	(query (:insert-into 'next-of-kin
		:set 'user-id user-id
		'name name
		'email email
		'telephone-number telephone-number
		'relationship relationship))))

(test set-next-of-kin-test (is (null (set-next-of-kin *test-user* "lam" "lam" "lam" "lam"))))

(defun get-next-of-kin (user-id)
  (conn (*db-string*)
	(query (:limit (:order-by (:select '* :from 'next-of-kin :where (:= user-id 'user-id))) (:desc 'created-at) 1))))
(test get-next-of-kin (is (equal *test-id* (caar (get-next-of-kin *test-id*)))))

(defun get-next-of-kin-all (user-id)
  (conn (*db-string*)
	(query (:select '* :from 'next-of-kin :where (:= user-id 'user-id)))))
(test get-next-of-kin-all (is (= 1 (length (get-next-of-kin-all *test-id*)))))

;; last test returns to decklm db
(test return-to-decklm-db (is (equal "cenna"
				     (prog1
					 (change-toplevel-database "cenna" "cenna" (uiop:getenv "POSTGRES_PASSWORD") "localhost")
				       (setf *db-string* "cenna")))))

