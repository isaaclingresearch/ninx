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
      (create-tables))
    (initialise-chat-types)))

(defparameter *db-string* "cenna" "This is the database name currently in use, we need this to reduce code and make tests work.")
(test initialise-db
  (is (null (prog1 (initialise-db "cenna_testdb" "cenna_testdb" t)
	      (setf *db-string* "cenna_testdb")))))

(defun create-tables ()
  "this will create tables for all our data"
  (conn (*db-string*)
    ;; Users table (immutable data)
    ;; all versioned data will depend on the user-id and creation times as the primary key (user-id, created-at)
    ;; for now, no data can be null, you either fill the data or don't use the service.

    (query (:create-table (:if-not-exists 'user-ids)
			  ((id :type uuid :primary-key t :default (:raw "gen_random_uuid()"))
			   (created-at :type bigint :default (:raw "extract(epoch from now())::bigint")))))
    (query (:create-table (:if-not-exists 'date-of-birth)
			  ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			   (created-at :type bigint :default (:raw "extract(epoch from now())::bigint"))
			   (date-of-birth :type timestamp-without-time-zone))
			  (:primary-key user-id created-at)))
    (query (:create-table (:if-not-exists 'sex-assigned-at-birth)
			  ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			   (created-at :type bigint :default (:raw "extract(epoch from now())::bigint"))
			   (sex-assigned-at-birth :type text))
			  (:primary-key user-id created-at)))
    (query (:create-table (:if-not-exists 'race-ethnicity)
			  ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			   (created-at :type bigint :default (:raw "extract(epoch from now())::bigint"))
			   (race-ethnicity :type text))
			  (:primary-key user-id created-at)))
    (query (:create-table (:if-not-exists 'country-of-birth)
			  ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			   (created-at :type bigint :default (:raw "extract(epoch from now())::bigint"))
			   (country-of-birth :type text))
			  (:primary-key user-id created-at)))
    
    (query (:create-table (:if-not-exists 'gender)
			  ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			   (gender :type text)
			   (created-at :type bigint :default (:raw "extract(epoch from now())::bigint")))
			  (:primary-key user-id created-at)))	

     (query (:create-table (:if-not-exists 'level-of-education)
			  ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			   (level-of-education :type text)
			   (created-at :type bigint :default (:raw "extract(epoch from now())::bigint")))
			  (:primary-key user-id created-at)))	

    (query (:create-table (:if-not-exists 'full-name)
			  ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			   (full-name :type text)
			   (created-at :type bigint :default (:raw "extract(epoch from now())::bigint")))
			  (:primary-key user-id created-at)))

    (query (:create-table (:if-not-exists 'marital-status)
			  ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			   (marital-status :type text)
			   (created-at :type bigint :default (:raw "extract(epoch from now())::bigint")))
			  (:primary-key user-id created-at)))

    (query (:create-table (:if-not-exists 'occupation)
			  ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			   (occupation :type text)
			   (created-at :type bigint :default (:raw "extract(epoch from now())::bigint")))
			  (:primary-key user-id created-at)))

    ;; Current Location History
    (query (:create-table (:if-not-exists 'location)
			  ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			   (country :type text)
			   (city :type text)
			   (region :type text)
			   (created-at :type bigint :default (:raw "extract(epoch from now())::bigint")))
			  (:primary-key user-id created-at)))

    ;; Contacts History (can have multiple contacts over time)
    (query (:create-table (:if-not-exists 'contact)
			  ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			   (telephone-number :type text)
			   (email :type text)
			   (created-at :type bigint :default (:raw "extract(epoch from now())::bigint")))
			  (:primary-key user-id created-at)))

    ;; Next of Kin History (can have multiple next of kin over time)
    (query (:create-table (:if-not-exists 'next-of-kin)
			  ((user-id :type uuid :references ((user-ids id) :cascade :cascade))
			   (name :type text)
			   (email :type text)
			   (telephone-number :type text)
			   (relationship :type text)
			   (created-at :type bigint :default (:raw "extract(epoch from now())::bigint")))
			  (:primary-key user-id created-at)))

    #|
       while storing chats, is it important to separate the chats according to the type
       say like medical-history-chats, surgical-history-chats. i think it is.
       the messages too will have a different table referencing the parent table.
       i feel like maybe i shouldn't separate the types, just indicate the type it is.
    |#					
    (query (:create-table (:if-not-exists 'chat-type) 
			  ((id :type serial :primary-key t)
			   (type :type text :unique t))))
    (query (:create-table (:if-not-exists 'chats)
			  ((id :type uuid :primary-key t :default (:raw "gen_random_uuid()"))
			   (user-id :type uuid :references ((user-ids id) :cascade :cascade))
			   (chat-type :type integer :references ((chat-type id) :cascade :cascade))
			   (created-at :type bigint :default (:raw "extract(epoch from now())::bigint")))))
    (query (:create-table (:if-not-exists 'chat-messages)
			  ((chat-id :type uuid :references ((chats id) :cascade :cascade))
			   (created-at :type bigint :default (:raw "extract(epoch from now())::bigint"))
			   (sender :type :boolean);; sender is 0 for model 1 for user
			   (message :type text))
			  (:primary-key chat-id created-at)))
    ))

(test create-tables (is (null (create-tables))))

(defun delete-tables ()
  "delete all tables"
  (dolist (table '(user-ids country-of-birth date-of-birth sex-assigned-at-birth full_name occupation marital_status gender contact level-of-education location next_of_kin race-ethnicity))
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
	  (query (:insert-into 'user-ids :set 
			       :returning 'id)))))
(defparameter *test-id* nil)
(test create-user-id
  (is (not (null (let ((id (create-user-id))) (setf *test-id* id) (format t "id: ~a~%~%~%" id) id)))))

(defun set-date-of-birth (user-id date-of-birth)
  (conn (*db-string*)
    (query (:insert-into 'date-of-birth
	    :set 'user-id user-id
	    'date-of-birth date-of-birth
	    :returning 'user-id))))
(test set-date-of-birth (is (not (null (set-date-of-birth *test-id* "01-01-1999")))))

(defun get-date-of-birth (user-id)
  (caar (conn (*db-string*)
	  (query (:fetch (:order-by (:select 'date-of-birth.date-of-birth :from 'date-of-birth :where (:= user-id 'user-id))
				    (:desc 'created-at)))))))
(test get-date-of-birth (is (equal  3124137600 (get-date-of-birth *test-id*))))

(defun get-date-of-birth-all (user-id)
  (conn (*db-string*)
    (query (:select 'date-of-birth :from 'date-of-birth :where (:= user-id 'user-id)))))
(test get-date-of-birth-all (is (= 1 (length (get-date-of-birth-all *test-id*)))))

(defun set-country-of-birth (user-id country-of-birth)
  (conn (*db-string*)
    (query (:insert-into 'country-of-birth
	    :set 'user-id user-id
	    'country-of-birth country-of-birth
	    :returning 'user-id))))
(test set-country-of-birth (is (not (null (set-country-of-birth *test-id* "uganda")))))

(defun get-country-of-birth (user-id)
  (caar (conn (*db-string*)
	  (query (:fetch (:order-by (:select 'country-of-birth.country-of-birth :from 'country-of-birth :where (:= user-id 'user-id))
				    (:desc 'created-at)))))))
(test get-country-of-birth (is (equal "uganda" (get-country-of-birth *test-id*))))

(defun get-country-of-birth-all (user-id)
  (conn (*db-string*)
    (query (:select 'country-of-birth :from 'country-of-birth :where (:= user-id 'user-id)))))
(test get-country-of-birth-all (is (= 1 (length (get-country-of-birth-all *test-id*)))))

(defun set-sex-assigned-at-birth (user-id sex-assigned-at-birth)
  (conn (*db-string*)
    (query (:insert-into 'sex-assigned-at-birth
	    :set 'user-id user-id
	    'sex-assigned-at-birth sex-assigned-at-birth))))
(test set-sex-assigned-at-birth (is (null (set-sex-assigned-at-birth *test-id* "male"))))

(defun get-sex-assigned-at-birth (user-id)
  (caar (conn (*db-string*)
	  (query (:fetch (:order-by (:select 'sex-assigned-at-birth :from 'sex-assigned-at-birth :where (:= user-id 'user-id))
				    (:desc 'created-at)))))))
(test get-sex-assigned-at-birth (is (equal "male" (get-sex-assigned-at-birth *test-id*))))

(defun get-sex-assigned-at-birth-all (user-id)
  (conn (*db-string*)
    (query (:select 'sex-assigned-at-birth :from 'sex-assigned-at-birth :where (:= user-id 'user-id)))))
(test get-sex-assigned-at-birth-all (is (= 1 (length (get-sex-assigned-at-birth-all *test-id*)))))

(defun set-race-ethnicity (user-id race-ethnicity)
  (conn (*db-string*)
    (query (:insert-into 'race-ethnicity
	    :set 'user-id user-id
	    'race-ethnicity race-ethnicity))))
(test set-race-ethnicity (is (null (set-race-ethnicity *test-id* "african"))))

(defun get-race-ethnicity (user-id)
  (caar (conn (*db-string*)
	  (query (:fetch (:order-by (:select 'race-ethnicity :from 'race-ethnicity :where (:= user-id 'user-id)) (:desc 'created-at)))))))
(test get-race-ethnicity (is (equal "african" (get-race-ethnicity *test-id*))))

(defun get-race-ethnicity-all (user-id)
  (conn (*db-string*)
    (query (:select 'race-ethnicity :from 'race-ethnicity :where (:= user-id 'user-id)))))
(test get-race-ethnicity-all (is (= 1 (length (get-race-ethnicity-all *test-id*)))))

(defun set-gender (user-id gender)
  (conn (*db-string*)
    (query (:insert-into 'gender
	    :set 'user-id user-id
	    'gender gender))))
(test set-gender (is (null (set-gender *test-id* "male"))))

(defun get-gender (user-id)
  (caar (conn (*db-string*)
	  (query (:fetch (:order-by (:select 'gender :from 'gender :where (:= user-id 'user-id)) (:desc 'created-at)))))))
(test get-gender (is (equal "male" (get-gender *test-id*))))

(defun get-gender-all (user-id)
  (conn (*db-string*)
    (query (:select 'gender :from 'gender :where (:= user-id 'user-id)))))
(test get-gender-all (is (= 1 (length (get-gender-all *test-id*)))))

(defun set-level-of-education (user-id level-of-education)
  (conn (*db-string*)
    (query (:insert-into 'level-of-education
	    :set 'user-id user-id
	    'level-of-education level-of-education))))
(test set-level-of-education (is (null (set-level-of-education *test-id* "male"))))

(defun get-level-of-education (user-id)
  (caar (conn (*db-string*)
	  (query (:fetch (:order-by (:select 'level-of-education :from 'level-of-education :where (:= user-id 'user-id)) (:desc 'created-at)))))))
(test get-level-of-education (is (equal "male" (get-level-of-education *test-id*))))

(defun get-level-of-education-all (user-id)
  (conn (*db-string*)
    (query (:select 'level-of-education :from 'level-of-education :where (:= user-id 'user-id)))))
(test get-level-of-education-all (is (= 1 (length (get-level-of-education-all *test-id*)))))


(defun set-full-name (user-id full-name)
  (conn (*db-string*)
    (query (:insert-into 'full-name
	    :set 'user-id user-id
	    'full-name full-name))))
(test set-full-name (is (null (set-full-name *test-id* "lubwama"))))

(defun get-full-name (user-id)
  (caar (conn (*db-string*)
	  (query (:fetch (:order-by (:select 'full-name :from 'full-name :where (:= user-id 'user-id)) (:desc 'created-at)))))))
(test get-full-name (is (equal "lubwama" (get-full-name *test-id*))))

(defun get-full-name-all (user-id)
  (conn (*db-string*)
    (query (:select 'full-name :from 'full-name :where (:= user-id 'user-id)))))
(test get-full-name-all (is (= 1 (length (get-full-name-all *test-id*)))))


(defun set-marital-status (user-id marital-status)
  (conn (*db-string*)
    (query (:insert-into 'marital-status
	    :set 'user-id user-id
	    'marital-status marital-status))))
(test set-marital-status (is (null (set-marital-status *test-id* "uganda"))))

(defun get-marital-status (user-id)
  (caar (conn (*db-string*)
	  (query (:fetch (:order-by (:select 'marital-status :from 'marital-status :where (:= user-id 'user-id)) (:desc 'created-at)))))))
(test get-marital-status (is (equal "uganda" (get-marital-status *test-id*))))

(defun get-marital-status-all (user-id)
  (conn (*db-string*)
    (query (:select 'marital-status :from 'marital-status :where (:= user-id 'user-id)))))
(test get-marital-status-all (is (= 1 (length (get-marital-status-all *test-id*)))))

(defun set-occupation (user-id occupation)
  (conn (*db-string*)
    (query (:insert-into 'occupation
	    :set 'user-id user-id
	    'occupation occupation))))
(test set-occupation (is (null (set-occupation *test-id* "uganda"))))

(defun get-occupation (user-id)
  (caar (conn (*db-string*)
	  (query (:fetch (:order-by (:select 'occupation :from 'occupation :where (:= user-id 'user-id)) (:desc 'created-at)))))))
(test get-occupation (is (equal "uganda" (get-occupation *test-id*))))

(defun get-occupation-all (user-id)
  (conn (*db-string*)
    (query (:select 'occupation :from 'occupation :where (:= user-id 'user-id)))))
(test get-occupation-all (is (= 1 (length (get-occupation-all *test-id*)))))

(defun set-location (user-id country city region)
  (conn (*db-string*)
    (query (:insert-into 'location
	    :set 'user-id user-id
	    'country country
	    'city city
	    'region region))))

(test set-location-test (is (null (set-location *test-id* "USA" "Test City" "Test Region"))))

(defun get-location (user-id)
  (conn (*db-string*)
    (query (:fetch (:order-by (:select '* :from 'location :where (:= user-id 'user-id)) (:desc 'created-at))))))
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
  (is (null (set-contact *test-id* "123-456-7890" "test@example.com"))))

(defun get-contact (user-id)
  (conn (*db-string*)
    (query (:fetch (:order-by (:select '* :from 'contact :where (:= user-id 'user-id)) (:desc 'created-at))))))
(test get-contact (is (equal *test-id* (caar (get-contact *test-id*)))))

(defun get-contact-all (user-id)
  (conn (*db-string*)
    (query (:select '* :from 'contact :where (:= user-id 'user-id)))))
(test get-contact-all (is (equal 1 (length (get-contact-all *test-id*)))))


(defun set-next-of-kin (user-id name email telephone-number relationship)
  (conn (*db-string*)
    (query (:insert-into 'next-of-kin
	    :set 'user-id user-id
	    'name name
	    'email email
	    'telephone-number telephone-number
	    'relationship relationship))))

(test set-next-of-kin-test (is (null (set-next-of-kin *test-id* "lam" "lam" "lam" "lam"))))

(defun get-next-of-kin (user-id)
  (conn (*db-string*)
    (query (:fetch (:order-by (:select '* :from 'next-of-kin :where (:= user-id 'user-id)) (:desc 'created-at))))))
(test get-next-of-kin (is (equal *test-id* (caar (get-next-of-kin *test-id*)))))

(defun get-next-of-kin-all (user-id)
  (conn (*db-string*)
    (query (:select '* :from 'next-of-kin :where (:= user-id 'user-id)))))
(test get-next-of-kin-all (is (equal 1 (length (get-next-of-kin-all *test-id*)))))

(defun get-user-data (user-id)
  "get all the above user data"
  (car (conn (*db-string*)
	 (query
	  (:select 'date-of-birth
		   'sex-assigned-at-birth
		   'country-of-birth
		   'full-name
		   'gender
		   'level-of-education
		   'marital-status
		   'ct.email 'ct.telephone-number
		   'country 'city 'region
		   (:as 'nok.name 'next-of-kin-name)
		   (:as 'nok.relationship 'next-of-kin-relationship)
		   (:as 'nok.telephone-number 'next-of-kin-telephone-number)
		   (:as 'nok.email 'next-of-kin-email)
		   :from (:as (:select 'date-of-birth 'user-id :from 'date-of-birth :where (:= 'user-id user-id)) 'dob)
		   :inner-join  (:as (:select 'sex-assigned-at-birth 'user-id :from 'sex-assigned-at-birth :where (:= 'user-id user-id)) 'sab)
		   :on (:= 'sab.user-id 'dob.user-id)
		   :inner-join  (:as (:select 'country-of-birth 'user-id :from 'country-of-birth :where (:= 'user-id user-id)) 'cob)
		   :on (:= 'cob.user-id 'dob.user-id)
		   :inner-join  (:as (:select 'full-name 'user-id :from 'full-name :where (:= 'user-id user-id)) 'fn)
		   :on (:= 'fn.user-id 'dob.user-id)
		   :inner-join  (:as (:select 'gender 'user-id :from 'gender :where (:= 'user-id user-id)) 'gn)
		   :on (:= 'gn.user-id 'dob.user-id)
		   :inner-join  (:as (:select 'level-of-education 'user-id :from 'level-of-education :where (:= 'user-id user-id)) 'loe)
		   :on (:= 'loe.user-id 'dob.user-id)
		   :inner-join  (:as (:select 'marital-status 'user-id :from 'marital-status :where (:= 'user-id user-id)) 'ms)
		   :on (:= 'ms.user-id 'dob.user-id)
		   :inner-join  (:as (:select 'country 'city 'region 'user-id :from 'location :where (:= 'user-id user-id)) 'lc)
		   :on (:= 'lc.user-id 'dob.user-id)
		   :inner-join  (:as (:select 'email 'telephone-number 'user-id :from 'contact :where (:= 'user-id user-id)) 'ct)
		   :on (:= 'ct.user-id 'ct.user-id)
		   :inner-join (:as (:select 'name 'relationship 'telephone-number 'email 'user-id :from 'next-of-kin :where (:= 'user-id user-id)) 'nok)
		   :on (:= 'nok.user-id 'dob.user-id))
	  :plists))))
(test get-user-data (is (equal '(:DATE-OF-BIRTH 3124137600 :SEX-ASSIGNED-AT-BIRTH "male" :COUNTRY-OF-BIRTH
				 "uganda" :FULL-NAME "lubwama" :GENDER "male" :LEVEL-OF-EDUCATION "male" :MARITAL-STATUS "uganda" :EMAIL
				 "test@example.com" :TELEPHONE-NUMBER "123-456-7890" :COUNTRY "USA" :CITY
				 "Test City" :REGION "Test Region" :NEXT-OF-KIN-NAME "lam"
				 :NEXT-OF-KIN-RELATIONSHIP "lam" :NEXT-OF-KIN-TELEPHONE-NUMBER "lam"
				 :NEXT-OF-KIN-EMAIL "lam")
			       (get-user-data *test-id*))))

;;;; CHATS
(defun set-chat-type (type)
  (caar (conn (*db-string*)
	      (query (:insert-into 'chat-type :set 'type type
				   :on-conflict-do-nothing
				   :returning 'id)))))
(test set-chat (is (integerp (set-chat-type "test"))))
;; initialise all table types
(defun initialise-chat-types ()
  (dolist (name (list "medical history" "surgical history" "family history" "social history" "presenting complaint" "history of presenting complaint"))
    (set-chat-type name)))
(test intialise-chat-types (is (null (initialise-chat-types))))

(defun get-chat-type (type)
  (caar (conn (*db-string*)
	      (query (:select 'id :from 'chat-type :where (:= 'type type))))))
(test get-chat-type (is (integerp (get-chat-type "medical history"))))

(defun create-chat (user-id chat-type &aux (chat-id (get-chat-type chat-type)))
  (caar (conn (*db-string*)
	      (query (:insert-into 'chats :set 'user-id user-id 'chat-type chat-id
				   :returning 'id)))))
(defparameter *test-chat* nil)
(test create-chat (is (not (null (let ((id (create-chat *test-id* "medical history")))
		       (setq *test-chat* id)
		       id)))))

(defun set-message (chat-id sender message &aux (sender-id (if (string= sender "user") t nil)))
  (conn (*db-string*)
	(query (:insert-into 'chat-messages :set 'sender sender-id 'chat-id chat-id 'message message))))
(test set-message (is (null (set-message *test-chat* "user" "test"))))

(defun get-chat-messages (chat-id)
  (conn (*db-string*)
    (query (:order-by (:select 'sender 'message :from 'chat-messages :where (:= 'chat-id chat-id))
		      'created-at))))
(test get-chat-message (is (equal 1 (length (get-chat-messages *test-chat*)))))

;; last test returns to decklm db
(test return-to-decklm-db (is (equal "cenna"
				     (prog1
					 (change-toplevel-database "cenna" "cenna" (uiop:getenv "POSTGRES_PASSWORD") "localhost")
				       (setf *db-string* "cenna")))))

