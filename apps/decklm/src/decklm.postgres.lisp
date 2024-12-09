(in-package :decklm)

;; connect to decklm database.
(handler-case (connect-toplevel "postgres" "postgres" (uiop:getenv "POSTGRES_PASSWORD") "localhost")
  (error (err)
    (format t "Error connecting to database, with the error: ~a.~% If database is not created, create it." err)))

(handler-case (change-toplevel-database "decklm" "decklm" (uiop:getenv "POSTGRES_PASSWORD") "localhost")
  (error (err)
    (format t "Error connecting to database, with the error: ~a.~% If database is not created, create it." err)))

(defmacro conn ((database) &body data)
  `(with-connection (list ,database ,database ,(uiop:getenv "POSTGRES_PASSWORD") "localhost")
     ,@data))

(defun make-date ()
  "return date as YYYY-MM-DD"
  (car (str:split "T" (format nil "~a" (local-time:today)))))

(deftest start-tests (equalp "postgres" (change-toplevel-database "postgres" "postgres" (uiop:getenv "POSTGRES_PASSWORD") "localhost")) t)
(deftest delete-db (query (:drop-database "testdb")) nil)
(deftest drop-role (query (:drop-role "testdb")) nil)

(defun initialise-db (role &optional (database "decklm") create-tables)
  "this function will create the database and the appropriate tables."
  (let ((password (uiop:getenv "POSTGRES_PASSWORD")))
    (change-toplevel-database "postgres" "postgres" password "localhost")
    (create-role role password :base-role :admin)
    (create-database database :owner role)
    (when create-tables
      (change-toplevel-database role database password "localhost")
      (create-tables))))

(defparameter *db-string* "decklm" "This is the database name currently in use, we need this to reduce code and make tests work.")
(deftest initialise-db (prog1 (initialise-db "testdb" "testdb" t)
			 (setf *db-string* "testdb")) nil)

(defun create-tables ()
  "this function will create tables for storing the data, the user-uuid is the main identifier of the user."
  (conn (*db-string*) (query
		       (:create-table (:if-not-exists 'user-ids)
				      ((user-id :type uuid :primary-key t :default (:raw "gen_random_uuid()"))
				       (email :type varchar :unique t)
				       (creation-date :type timestamp-without-time-zone :default (:raw "CURRENT_TIMESTAMP")))))
    ;; records if a user has used their free trial or not
    (query (:create-table (:if-not-exists 'free-trials)
			  ((user-id :type uuid :primary-key t)
			   (has-trial :type boolean :default t))))
    (query (:create-table (:if-not-exists 'access-codes)
			  ((email :type text :primary-key t)
			   (access-code :type smallint)
			   (creation-date :type timestamp-without-time-zone :default (:raw "CURRENT_TIMESTAMP")))))
    (query (:create-table (:if-not-exists 'consent)
			  ((user-id :type uuid :primary-key t)
			   (consent :type boolean))))
    (query (:create-table (:if-not-exists 'cookies)
			  ((cookie :type uuid :primary-key t :default (:raw "gen_random_uuid()"))
			   (user-id :type uuid)
			   (persist :type boolean :default nil)
			   (creation-date :type timestamp-without-time-zone :default (:raw "CURRENT_TIMESTAMP")))))
    (query (:create-table (:if-not-exists 'user-docs)
			  ((id :type uuid :primary-key t :default (:raw "gen_random_uuid()"))
			   (user-id :type uuid)
			   (title :type string)
			   (data :type bytea)
			   (creation-date :type timestamp-without-time-zone :default (:raw "CURRENT_TIMESTAMP")))))
    (query (:create-table (:if-not-exists 'feedback)
			  ((id :type serial :primary-key t) ;; use serial because we don't expect feeback to grow that rapidly
			   (user-id :type (or uuid db-null))
			   (feedback :type text)
			   (read :type boolean :default nil)
			   (creation-date :type timestamp-without-time-zone :default (:raw "CURRENT_TIMESTAMP")))))
    (query (:create-table (:if-not-exists 'user-tokens)
			  ((user-id :type uuid :primary-key t)
			   (count :type integer))))
    (query (:create-table (:if-not-exists 'user-transactions)
			  ((transaction-id :type varchar :primary-key t)
			   (user-id :type (or uuid db-null))
			   (amount :type float)
			   (close-date :type timestamp-with-time-zone))))
    (query (:create-table (:if-not-exists 'daily-input-tokens)
			  ((id :type serial :primary-key t)
			   (date :type timestamp-without-time-zone)
			   (model :type varchar :default "gemini-pro-1.5")
			   (count :type integer))
			  (:constraint dinput_unique :unique 'date 'model)))
    (query (:create-table (:if-not-exists 'daily-output-tokens)
			  ((id :type serial :primary-key t)
			   (date :type timestamp-without-time-zone)
			   (model :type varchar :default "gemini-pro-1.5")
			   (count :type integer))
			  (:constraint doutput_unique :unique 'date 'model)))
    (query (:create-table (:if-not-exists 'daily-trial-input-tokens)
			  ((id :type serial :primary-key t)
			   (date :type timestamp-without-time-zone)
			   (model :type varchar :default "gemini-pro-1.5")
			   (count :type integer))
			  (:constraint dtinput_unique :unique 'date 'model)))
    (query (:create-table (:if-not-exists 'daily-trial-output-tokens)
			  ((id :type serial :primary-key t)
			   (date :type timestamp-without-time-zone)
			   (model :type varchar :default "gemini-pro-1.5")
			   (count :type integer))
			  (:constraint dtoutput_unique :unique 'date 'model)))
    (query (:create-table (:if-not-exists :daily-parsing-errors)
			  ((id :type serial :primary-key t)
			   (date :type timestamp-without-time-zone)
			   (model :type text)
			   (count :type integer))
			  (:constraint dparsing_errors :unique 'date 'model)))

    ;; analytics tables
    ;; daily events records all visits to a page
    (query (:create-table (:if-not-exists 'daily-events)
			  ((id :type serial :primary-key t)
			   (date :type timestamp-without-time-zone)
			   (page :type varchar)
			   (count :type integer :default 1))
			  (:constraint devents_unique :unique 'date 'page)))
    

    ;; active records all visits to a page lasting atleast 1 second
    (query (:create-table (:if-not-exists 'daily-active)
			  ((id :type serial :primary-key t)
			   (date :type timestamp-without-time-zone)
			   (page :type varchar)
			   (count :type integer :default 1))
			  (:constraint dactive_unique :unique 'date 'page)))

    ;; duration records the number of milliseconds a user spends on a page
    ;; they're recorded as sums and counts, so prevent excessive calculations,
    ;; you can compute if need be
    (query (:create-table (:if-not-exists 'daily-durations)
			  ((id :type serial :primary-key t)
			   (date :type timestamp-without-time-zone)
			   (page :type varchar)
			   (sum :type float)
			   (count :type integer :default 1))
			  (:constraint ddurations_unique :unique 'date 'page)))
    (query (:create-table (:if-not-exists 'daily-unique-visitors-by-ip)
			  ((id :type serial :primary-key t)
			   (ip :type text)
			   (date :type timestamp-without-time-zone)
			   (count :type integer))
			  (:constraint dunique_by_ip :unique 'ip 'date)))
    (query (:create-table (:if-not-exists 'daily-unique-visitors-by-country)
			  ((id :type serial :primary-key t)
			   (country :type text)
			   (ip :type text)
			   (date :type timestamp-without-time-zone)
			   (count :type integer))
			  (:constraint unique_visitors_by_country :unique 'country 'date 'ip)))
    (query (:create-table (:if-not-exists 'daily-deck-creations-by-country)
			  ((id :type serial :primary-key t)
			   (country :type text)
			   (date :type timestamp-without-time-zone)
			   (count :type integer))
			  (:constraint decks_by_country :unique 'country 'date)))))
(deftest create-tables (create-tables) nil)

(defun delete-tables ()
  "delete all tables"
  (dolist (table '(user-ids free-trials daily-durations daily-active daily-events daily-trial-input-tokens daily-trial-output-tokens daily-input-tokens daily-output-tokens user-tokens feedback access-codes consent cookies user-docs user-transactions daily-unique-visitors-by-ip daily-unique-visitors-by-country daily-deck-creations-by-country))
    (conn (*db-string*) (query (:drop-table table)))))
(deftest delete-tables (delete-tables) nil)

(defun reset-tables ()
  (handler-case (delete-tables) (error (err) (declare (ignore err))))
  (create-tables))

(deftest reset-tables (reset-tables) nil)

(defparameter *test-email* "test@example.com")
(defparameter *test-cookie* nil)
(defparameter *test-user-id* nil)

;; DATA FUNCTIONS
(defun save-new-email (email)
  "when given a user email, insert into the user-ids and then get the id, insert it into the free trial table"
  (unless (query (:select 'user-id :from 'user-ids :where (:= 'email email)))
    (conn (*db-string*) (query
			 (:insert-into 'user-ids :set 'email email)))
    (caar (conn (*db-string*) (query (:insert-into 'free-trials
						   (:select 'user-id :from 'user-ids
						    :where (:= 'email email))
						   :returning 'user-id))))))
(deftest save-new-email (progn (setf *test-user-id* (save-new-email *test-email*)) nil) nil)

(defun get-user-id (email)
  (caar (conn (*db-string*) (query (:select 'user-id :from 'user-ids :where (:= 'email email))))))
(deftest get-user-id (equal *test-user-id* (get-user-id *test-email*)) t)

(defun get-user-email (user-id)
  (caar (conn (*db-string*) (query (:select 'email :from 'user-ids :where (:= 'user-id user-id))))))
(deftest get-user-email (equal *test-email* (get-user-email *test-user-id*)) t)

(defun delete-user (user-id)
  (conn (*db-string*) (query (:delete-from 'user-ids :where (:= 'user-id user-id)))))
(deftest delete-user (prog1 (delete-user *test-user-id*)) nil)
(deftest save-new-email-again (progn (setf *test-user-id* (save-new-email *test-email*)) nil) nil)


(defun get-user-emails ()
  "get all emails from the tables."
  (conn (*db-string*) (query (:select 'email :from 'user-ids))))
(deftest get-user-emails (equal (get-user-emails) '(("test@example.com"))) t)

(defun save-cookie (email &key persist)
  (caar (conn (*db-string*) (query (:insert-into 'cookies
				    :set 'user-id (:select 'user-id :from 'user-ids :where (:= 'user-ids.email email)) 'persist persist
				    :returning 'cookie)))))
(deftest save-cookie (let ((cookie (save-cookie *test-email*)))
		       (setf *test-cookie* cookie)
		       (equal (get-email-from-cookie cookie) *test-email*)) t)

(defun get-user-id-from-cookie (cookie)
  (caar (conn (*db-string*) (query (:select 'user-id :from 'cookies
				    :where (:= 'cookie cookie))))))
(deftest get-user-id-from-cookie (equal *test-user-id* (get-user-id-from-cookie *test-cookie*)) t)

(defun get-email-from-cookie (cookie &aux (user-id (get-user-id-from-cookie cookie)))
  (when user-id
    (get-user-email user-id)))
(deftest get-email-from-cookie (equal *test-email* (get-email-from-cookie *test-cookie*)) t)

(defun delete-cookie (cookie)
  (conn (*db-string*) (query (:delete-from 'cookies :where (:= 'cookie cookie)))))
(deftest delete-cookie (null (delete-cookie *test-cookie*)) t)
(deftest restore-cookie (prog1 t (setf *test-cookie* (save-cookie *test-email*))) t)

(defun save-to-free-trial (cookie &optional value)
  (conn (*db-string*) (query (:insert-into 'free-trials
			      :set 'user-id (:select 'user_id :from 'cookies :where (:= 'cookies.cookie cookie)) 'has-trial value
			      :on-conflict 'user-id
			      :update-set 'has-trial value
			      ))))
(deftest save-to-free-trial (null (save-to-free-trial *test-cookie*)) t)

(defun has-free-trial-p (cookie)
  (caar (conn (*db-string*) (query (:select 'has-trial :from 'free-trials
				    :where (:= 'user-id  (:select 'user_id :from 'cookies :where (:= 'cookies.cookie cookie))))))))
(deftest has-free-trial-p (has-free-trial-p *test-cookie*) t)

(defparameter *test-access-code* 1234)
(defun save-access-code (email code)
  (conn (*db-string*) (query (:insert-into 'access-codes
			      :set 'email email 'access-code code
			      :on-conflict 'email
			      :update-set 'access-code code
			      ))))
(deftest save-access-code (null (save-access-code *test-email* *test-access-code*)) t)

(defun get-access-code (email)
  (caar (conn (*db-string*) (query (:select 'access-code :from 'access-codes :where (:= 'email email))))))
(deftest get-access-code (equal *test-access-code* (get-access-code *test-email*)) t)

(defun verify-access-code (email code)
  (equal code (get-access-code email)))
(deftest verify-access-code (verify-access-code *test-email* *test-access-code*) t)

(defun save-consent (email consent)
  (conn (*db-string*) (query (:insert-into 'consent
			      :set 'user-id (:select 'user-id :from 'user-ids :where (:= 'user-ids.email email)) 'consent consent
			      :on-conflict 'user-id
			      :update-set 'consent consent
			      ))))
(deftest save-consent (null (save-consent *test-email* t)) t)

(defun get-consent (email)
  (caar (conn (*db-string*)
	  (query
	   (:select 'consent :from 'consent :where (:= 'user-id (:select 'user-id :from 'user-ids :where (:= 'user-ids.email email)) ))))))
(deftest get-consent (get-consent *test-email*) t)

(defun revoke-consent (email)
  (conn (*db-string*) (query (:update 'consent
			      :set 'consent nil
			      :where (:= 'user-id (:select 'user-id :from 'user-ids :where (:= 'user-ids.email email)))))))
(deftest revoke-consent (null (revoke-consent *test-email*)) t)
(deftest get-consent-1 (get-consent *test-email*) nil)

;; DOCUMENT HANDLING
(defun compress-pdf-to-zstd (path)
  "When given the path of a file, compress it with zstd, return it as a byte array"
  (let* ((bytes (llms::read-binary-file-to-octets path))
	 (zstd-bytes (zstd:compress-buffer bytes)))
    zstd-bytes))

(defun compress-string-to-zstd (string)
  "when given a string, compress it to byte array"
  (zstd:compress-buffer (flexi-streams:string-to-octets string :external-format :utf8)))

(defun decompress-bytes (bytes)
  "decompressed a zstd compressed bytes and returns it as a byte array"
  (zstd:decompress-buffer bytes))

(defparameter *test-doc* #p"~/common-lisp/ninx/apps/decklm/test.pdf")
(defun save-document (cookie title data)
  (caar (conn (*db-string*) (query
			     (:insert-into 'user-docs
			      :set 'user-id (:select 'user_id :from 'cookies :where (:= 'cookies.cookie cookie)) 'title title 'data (compress-string-to-zstd data)
			      :returning 'id )))))
(deftest save-document (null (save-document *test-cookie* "test document" "test document")) nil)

(defun get-document-data (doc-id)
  (flexi-streams:octets-to-string
   (decompress-bytes
    (caar (conn (*db-string*)
	    (query (:select 'data :from 'user-docs
		    :where (:= 'id doc-id))))))
   :external-format :utf8))
(deftest get-document-data (equalp "test document"
				   (get-document-data (caar (query (:select 'id :from 'user-docs
								    :where (:= 'user-id *test-user-id*)))))) t)

(defun get-user-documents (cookie)
  (conn (*db-string*) (query (:select 'id 'title 'creation-date
			      :from 'user-docs
			      :where (:= 'user-id (:select 'user_id :from 'cookies :where (:= 'cookies.cookie cookie)))))))

(defun get-document-details (doc-id)
  (car (conn (*db-string*) (query (:select 'id 'title 'creation-date
				   :from 'user-docs
				   :where (:= 'id doc-id))))))

(defun count-user-documents (cookie)
  (length (get-user-documents cookie)))
(deftest count-user-documents (count-user-documents *test-cookie*) 1)

(defun save-feedback (feedback cookie)
  (conn (*db-string*)
    (if cookie
	(query (:insert-into 'feedback
		:set 'feedback feedback 'user-id (:select 'user_id :from 'cookies :where (:= 'cookies.cookie cookie))))
	(query (:insert-into 'feedback
		:set 'feedback feedback)))))
(deftest save-feedback (null (save-feedback "test-feedback" *test-cookie*)) t)
(deftest save-feedback-with-no-cookie (null (save-feedback "test-feedback" nil)) t)

;; TRANSACTIONS
(defun save-transaction (transaction-id email amount date)
  (conn (*db-string*)
    (query (:insert-into 'user-transactions
	    :set 'transaction-id transaction-id 'user-id (:select 'user-id :from 'user-ids :where (:= 'user-ids.email email)) 'amount amount 'close-date date
	    :on-conflict-do-nothing
	    ))))
(deftest save-transaction (null (save-transaction "test-transaction" *test-email* "1.00" "2024-10-15")) t)
(deftest save-transaction-with-no-email (null (save-transaction "test-transaction" "non-existent-email" "1.00" "2024-10-15")) t)

(defun transaction-saved-p (id)
  (caar (conn (*db-string*)
	  (query (:select 'amount :from 'user-transactions
		  :where (:= 'transaction-id id))))))
(deftest transaction-saved-p (null (transaction-saved-p "test-transaction")) nil)

;; TOKENS
(defun incr-user-tokens (user-id tokens &optional cookie &aux (id (if cookie (get-user-id-from-cookie cookie) user-id)))
  (conn (*db-string*) (query (:insert-into 'user-tokens
			      :set 'user-id id 'count tokens
			      :on-conflict 'user-id
			      :update-set 'count (:+ tokens 'user-tokens.count)
			      ))))
(deftest incr-user-tokens (null (incr-user-tokens *test-user-id* 1)) t)
(deftest incr-user-tokens-cookie (null (incr-user-tokens nil 1 *test-cookie*)) t)

(defun get-user-tokens (user-id &optional cookie &aux (id (if cookie (get-user-id-from-cookie cookie) user-id)))
  (caar (conn (*db-string*) (query (:select 'count :from 'user-tokens
				    :where (:= 'user-id id))))))
(deftest get-user-tokens (get-user-tokens *test-user-id*) 2)
(deftest get-user-tokens-with-cookie (get-user-tokens *test-user-id* *test-cookie*) 2)

(defun has-tokens-p (user-id &optional cookie &aux (id (if cookie (get-user-id-from-cookie cookie) user-id)))
  (not (null (get-user-tokens id))))
(deftest has-tokens-p (has-tokens-p *test-user-id*) t)
(deftest has-tokens-p-with-cookie (has-tokens-p *test-user-id* *test-cookie*) t)

(defun incr-input-tokens (tokens &key (model "gemini-pro-1.5") (date (make-date)))
  (conn (*db-string*) (query (:insert-into 'daily-input-tokens
			      :set 'date date 'count tokens 'model model
			      :on-conflict 'date 'model
			      :update-set 'count (:+ tokens 'daily-input-tokens.count)
			      ))))
(deftest incr-input-tokens (null (incr-input-tokens 1)) t)

(defun get-input-tokens (&key (duration 1) (model "gemini-pro-1.5"))
  (trivia:match (caar (conn (*db-string*)
			(query (:select (:sum 'count)
				:from 'daily-input-tokens
				:where (:and
					(:= 'daily-input-tokens.model model)
					(:> 'daily-input-tokens.date (:raw (format nil "(DATE '~a' - INTERVAL '~a day')" (make-date) duration))))))))
    (:null 0)
    (else else)))
(deftest get-input-tokens (get-input-tokens) 1)

(defun get-all-input-tokens (&key (duration 1))
  "this will return all models used in a given duration and a sum of their counts in that duration"
  (trivia:match (conn (*db-string*)
		   (query (:select 'model (:sum 'count)
				   :from 'daily-input-tokens
				   :where (:> 'daily-input-tokens.date (:raw (format nil "(DATE '~a' - INTERVAL '~a day')" (make-date) duration)))
				   :group-by 'daily-input-tokens.model)))
    ((list :null) ())
    (else else)))
(deftest get-all-input-tokens (get-all-input-tokens) (("gemini-pro-1.5" 1)))

(defun incr-output-tokens (tokens &key (model "gemini-pro-1.5") (date (make-date)))
  (conn (*db-string*) (query (:insert-into 'daily-output-tokens
			      :set 'date date 'count tokens 'model model
			      :on-conflict 'date 'model
			      :update-set 'count (:+ tokens 'daily-output-tokens.count)
			      ))))
(deftest incr-output-tokens (null (incr-output-tokens 1)) t)

(defun get-output-tokens (&key (duration 1) (model "gemini-pro-1.5"))
  (trivia:match (caar (conn (*db-string*)
			(query (:select (:sum 'count)
				:from 'daily-output-tokens
				:where (:and
					(:= 'model model)
					(:> 'daily-output-tokens.date (:raw (format nil "(DATE '~a' - INTERVAL '~a day')" (make-date) duration))))))))
    (:null 0)
    (else else)))
(deftest get-output-tokens (get-output-tokens) 1)

(defun get-all-output-tokens (&key (duration 1))
  "this will return all models used in a given duration and a sum of their counts in that duration"
  (trivia:match (conn (*db-string*)
		  (query (:select 'model (:sum 'count)
				  :from 'daily-output-tokens
				  :where (:> 'daily-output-tokens.date (:raw (format nil "(DATE '~a' - INTERVAL '~a day')" (make-date) duration)))
				  :group-by 'daily-output-tokens.model)))
    ((list :null) ())
    (else else)))
(deftest get-all-output-tokens (get-all-output-tokens) (("gemini-pro-1.5" 1)))

(defun incr-parsing-errors (&key (model "gemini-pro-1.5") (date (make-date)))
  (conn (*db-string*) (query (:insert-into 'daily-parsing-errors
			      :set 'date date 'count 1 'model model
			      :on-conflict 'date 'model
			      :update-set 'count (:+ 1 'daily-parsing-errors.count)
			      ))))
(deftest incr-parsing-errors (null (incr-parsing-errors)) t)

(defun get-parsing-errors (&key (duration 1) (model "gemini-pro-1.5"))
  (trivia:match (caar (conn (*db-string*)
			(query (:select (:sum 'count)
				:from 'daily-parsing-errors
				:where (:and
					(:= 'model model)
					(:> 'daily-parsing-errors.date (:raw (format nil "(DATE '~a' - INTERVAL '~a day')" (make-date) duration))))))))
    (:null 0)
    (else else)))
(deftest get-parsing-errors (get-parsing-errors) 1)

(defun get-all-parsing-errors (&key (duration 1))
  "this will return all errors of models used in a given duration and a sum of their counts in that duration"
  (trivia:match (conn (*db-string*)
		  (query (:select 'model (:sum 'count)
				  :from 'daily-parsing-errors
				  :where (:> 'daily-parsing-errors.date (:raw (format nil "(DATE '~a' - INTERVAL '~a day')" (make-date) duration)))
				  :group-by 'daily-parsing-errors.model)))
    ((list :null) ())
    (else else)))
(deftest get-all-parsing-errors (get-all-parsing-errors) (("gemini-pro-1.5" 1)))

(defun incr-trial-input-tokens (tokens &key (model "gemini-pro-1.5") (date (make-date)))
  (conn (*db-string*) (query (:insert-into 'daily-trial-input-tokens
			      :set 'date date 'count tokens 'model model
			      :on-conflict 'date 'model
			      :update-set 'count (:+ tokens 'daily-trial-input-tokens.count)
			      ))))
(deftest incr-trial-input-tokens (null (incr-trial-input-tokens 1)) t)

(defun get-trial-input-tokens (&key (duration 1) (model "gemini-pro-1.5"))
  (trivia:match (caar (conn (*db-string*)
			(query (:select (:sum 'count)
				:from 'daily-trial-input-tokens
				:where (:and
					(:= 'model model)
					(:> 'daily-trial-input-tokens.date (:raw (format nil "(DATE '~a' - INTERVAL '~a day')" (make-date) duration))))))))
    (:null 0)
    (else else)))
(deftest get-trial-input-tokens (get-trial-input-tokens) 1)

(defun get-all-trial-input-tokens (&key (duration 1))
  "this will return all models used in a given duration and a sum of their counts in that duration"
  (trivia:match (conn (*db-string*)
		   (query (:select 'model (:sum 'count)
				   :from 'daily-trial-input-tokens
				   :where (:> 'daily-trial-input-tokens.date (:raw (format nil "(DATE '~a' - INTERVAL '~a day')" (make-date) duration)))
				   :group-by 'daily-trial-input-tokens.model)))
    ((list :null) ())
    (else else)))
(deftest get-all-trial-input-tokens (get-all-trial-input-tokens) (("gemini-pro-1.5" 1)))

(defun incr-trial-output-tokens (tokens &key (model "gemini-pro-1.5") (date (make-date)))
  (conn (*db-string*) (query (:insert-into 'daily-trial-output-tokens
			      :set 'date date 'count tokens 'model model
			      :on-conflict 'date 'model
			      :update-set 'count (:+ tokens 'daily-trial-output-tokens.count)
			      ))))
(deftest incr-trial-output-tokens (null (incr-trial-output-tokens 1)) t)

(defun get-trial-output-tokens (&key (duration 1) (model "gemini-pro-1.5"))
  (trivia:match (caar (conn (*db-string*)
			(query (:select (:sum 'count)
				:from 'daily-trial-output-tokens
				:where (:and
					(:= 'model model)
					(:> 'daily-trial-output-tokens.date (:raw (format nil "(DATE '~a' - INTERVAL '~a day')" (make-date) duration))))))))
    (:null 0)
    (else else)))
(deftest get-trial-output-tokens (get-trial-output-tokens) 1)

(defun get-all-trial-output-tokens (&key (duration 1))
  "this will return all models used in a given duration and a sum of their counts in that duration"
  (trivia:match (conn (*db-string*)
		   (query (:select 'model (:sum 'count)
				   :from 'daily-trial-output-tokens
				   :where (:> 'daily-trial-output-tokens.date (:raw (format nil "(DATE '~a' - INTERVAL '~a day')" (make-date) duration)))
				   :group-by 'daily-trial-output-tokens.model)))
    ((list :null) ())
    (else else)))
(deftest get-all-trial-output-tokens (get-all-trial-output-tokens) (("gemini-pro-1.5" 1)))

;; ANALYTICS
(defun incr-events (page &aux (date (make-date)))
  (conn (*db-string*) (query (:insert-into 'daily-events
			      :set 'date date 'page page 'count 1
			      :on-conflict 'date 'page
			      :update-set 'count (:+ 1 'daily-events.count)
			      ))))
(deftest incr-events (null (incr-events "test-page")) t)

(defun get-events (page &key (duration 1))
  (trivia:match (caar (conn (*db-string*)
			(query (:select (:sum 'count)
				:from 'daily-events
				:where (:and
					(:= 'page page)
					(:> 'daily-events.date (:raw (format nil "(DATE '~a' - INTERVAL '~a day')" (make-date) duration))))))))
    (:null 0)
    (else else)))
(deftest get-events (get-events "test-page") 1)

(defun incr-active (page &aux (date (make-date)))
  (conn (*db-string*) (query (:insert-into 'daily-active
			      :set 'date date 'page page 'count 1
			      :on-conflict 'date 'page
			      :update-set 'count (:+ 1 'daily-active.count)
			      ))))
(deftest incr-active (null (incr-active "test-page")) t)

(defun get-active (page &key (duration 1))
  (trivia:match (caar (conn (*db-string*)
			(query (:select (:sum 'count)
				:from 'daily-active
				:where (:and
					(:= 'page page)
					(:> 'daily-active.date (:raw (format nil "(DATE '~a' - INTERVAL '~a day')" (make-date) duration))))))))
    (:null 0)
    (else else)))
(deftest get-active (get-active "test-page") 1)

(defun save-duration (page duration &optional (date (make-date)))
  (conn (*db-string*) (query (:insert-into 'daily-durations
			      :set 'page page 'sum duration 'date date 'count 1
			      :on-conflict 'date 'page
			      :update-set 'sum (:+ 'daily-durations.sum duration) 'count (:+ 'daily-durations.count 1)
			      ))))
(deftest save-duration (null (save-duration "test-page" 1)) t)

(defun get-duration (page &key (duration 1))
  (trivia:match (car (conn (*db-string*)
			(query (:select 'sum 'count
					:from 'daily-durations
					:where (:and
						(:= 'page page)
						(:> 'daily-durations.date (:raw (format nil "(DATE '~a' - INTERVAL '~a day')" (make-date) duration))))))))
    (:null 0)
    (else else)))
(deftest get-duration (get-duration "test-page") (1.0 1))

(defun save-ip-visit (ip &optional (date (make-date)))
  (conn (*db-string*)
    (query (:insert-into 'daily-unique-visitors-by-ip
	    :set 'ip ip 'count 1 'date date
	    :on-conflict 'date 'ip
	    :update-set 'count (:+ 'daily-unique-visitors-by-ip.count 1)
	    ))))
(deftest save-ip-visit (null (save-ip-visit "1.1.1.1")) t)

(defun get-unique-visitors-by-ip (&optional (duration 1))
  (caar (conn (*db-string*)
	  (query (:select (:count '*)
		  :from 'daily-unique-visitors-by-ip
		  :where (:> 'daily-unique-visitors-by-ip.date (:raw (format nil "(DATE '~a' - INTERVAL '~a day')" (make-date) duration))))))))
(deftest get-unique-visitors-by-ip (get-unique-visitors-by-ip) 1)

(defun get-country-from-ip (ip)
  (handler-case (cdr (assoc :en (cdr (find :names (cdr (find :country (mmdb-query *city-db* ip) :key #'car)) :key #'car))))
    (error (err)
      (declare (ignore err))
      "Unknown Country")))

(defun save-country-visit (ip &optional (date (make-date)) &aux (country (get-country-from-ip ip)))
  (conn (*db-string*)
    (query (:insert-into 'daily-unique-visitors-by-country
	    :set 'ip ip 'country country 'date date 'count 1
	    :on-conflict 'ip 'country 'date
	    :update-set 'count (:+ 'daily-unique-visitors-by-country.count 1)
	    ))))
(deftest save-country-visit (null (save-country-visit "192.100.11.1")) t)

(defun get-top-country-visitors (&key (count 5) (duration 1))
  "get the top count number of countries witht the highest unique visitors in duration"
  (conn (*db-string*)
    (query (:limit
	    (:order-by
	     (:select 'country (:count 'ip :distinct)
		      :from 'daily-unique-visitors-by-country
		      :where (:> 'daily-unique-visitors-by-country.date (:raw (format nil "(DATE '~a' - INTERVAL '~a day')" (make-date) duration)))
		      :group-by 'daily-unique-visitors-by-country.country)
	     (:desc (:count 'ip :distinct)))
	    count))))
(deftest get-top-country-visitors (equal '(("United States" 1)) (get-top-country-visitors)) t)

(defun save-country-deck-creations (ip &optional (date (make-date)) &aux (country (get-country-from-ip ip)))
  (conn (*db-string*)
    (query (:insert-into 'daily-deck-creations-by-country
	    :set 'country country 'date date 'count 1
	    :on-conflict 'country 'date
	    :update-set 'count (:+ 'daily-deck-creations-by-country.count 1)
	    ))))
(deftest save-country-deck-creations (null (save-country-deck-creations "192.100.11.1")) t)

(defun get-country-deck-creations (&key (count 5) (duration 1))
  "get the top count number of countries witht the highest deck creations in duration"
  (conn (*db-string*)
    (query (:limit
	    (:order-by
	     (:select 'country (:sum 'count)
		      :from 'daily-deck-creations-by-country
		      :where (:> 'daily-deck-creations-by-country.date (:raw (format nil "(DATE '~a' - INTERVAL '~a day')" (make-date) duration)))
		      :group-by 'daily-deck-creations-by-country.country)
	     (:desc (:sum 'count)))
	    count))))
(deftest get-country-deck-creations (equal '(("United States" 1)) (get-country-deck-creations)) t)

;; last test returns to decklm db
(deftest return-to-decklm-db (equal "decklm"
				    (prog1
					(change-toplevel-database "decklm" "decklm" (uiop:getenv "POSTGRES_PASSWORD") "localhost")
				      (setf *db-string* "decklm"))) t)

;; dev functions
(defun clear-test-email ()
  (let* ((email "lam@ninx.xyz")
	 (user-id (get-user-id email)))
    (conn (*db-string*)
      (query (:delete-from 'user-ids :where (:= 'user-id user-id)))
      (query (:delete-from 'access-codes :where (:= 'email email)))
      (query (:delete-from 'user-docs :where (:= 'user-id user-id)))
      (query (:delete-from 'free-trials :where (:= 'user-id user-id)))
      (query (:delete-from 'cookies :where (:= 'user-id user-id)))
      (query (:delete-from 'consent :where (:= 'user-id user-id)))
      (query (:delete-from 'user-tokens :where (:= 'user-id user-id))))))

(defun test-code ()
  (get-access-code "lam@ninx.xyz"))
