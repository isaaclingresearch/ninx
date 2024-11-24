(in-package :redis)

(def-cmd ZUNION (num &rest keys) :anything
  "Return the union between the Zsets stored at key1, key2, ..., keyN.")

(in-package :decklm)


(defun start-kvrocks ()
  (sb-ext:run-program (namestring (truename "~/.bin/kvrocks"))
		      (list "-c" (namestring (truename "~/common-lisp/decklm/conf/kvrocks.conf")))
		      :wait nil)
  (sleep 3)		    ; allow some time for the program to start
  (handler-case (connect-kvrocks)
    (error (err)
      (print err))))
(deftest start-kvrocks  (equal 'redis-connection (type-of (start-kvrocks))) t)

(defun stop-kvrocks ()
  (redis:red-shutdown)
  (sleep 3)
  (redis:disconnect))

(defun connect-kvrocks ()
  "connect to the server"
  (redis:connect :port 6666 :auth (uiop:getenv "KVROCKS_DEFAULT_PASSWORD")))

(defmacro with-kvrocks-txn ((&key namespace-token) &body body)
  "when given a namespace, switch to that namespace, else run the commands in a 'redis transaction'"
  `(progn
     (redis:red-multi)
     (when ,namespace-token
       (redis:red-auth ,namespace-token))
     ,@body
     (when ,namespace-token
       (redis:red-auth ,(uiop:getenv "KVROCKS_DEFAULT_PASSWORD")))
     (redis:red-exec)))

(defun make-date ()
  "return date as YYYY-MM-DD"
  (car (str:split "T" (format nil "~a" (local-time:today)))))

;; data functions

(defun data-structure ()
  "
This documents the structure of all saved to kvrocks:
1. EMAILS
    Emails are stored in the {email} slot. The key is the email and the value is the user uuid, a string of uuid v4.

2. USER UUIDS
    The users' uuids are stored in the {user-uuid} slot. With key as uuid and value as email. To simply application design, user uuids are only handled in this file, the app file only deals with emails. 

3. ACCESS CODES
    The access codes are stored in {access-code} with email as key and value as code, which is a four digit number. 
    The access codes expire after 24 hours.

4. CONSENT 
    Consent is saved in the {consent} slot with key as user uuid and value as boolean.

5. COOKIES 
    Cookies are saved in {cookie} with key as cookie, a uuid v4 string and value as the user uuid. 
    The cookie can expire in one day if the user revoked consent to track or in 10 years if the user agreed to be tracked.

6. DOCUMENTS 
    These are saved in the {doc-slot-(1 to 10)} so that they are not held in the same slot, to allow for distribution. The slots are not the same as
    kvrocks slots, and are assigned randomly. The key is the document id, a v4 uuid string.

    The value is a hash, with three items: data, date and title, this makes it easy for us to query the title and date of the document.
    The data is a string, first, the document (PDF) is created, read into bytes, the bytes are then zstd compressed and then converted to a string.
    See notes about data storage, in docs for more details about compression. The string is the value to the doc id.

    The the document is saved to a list of user's documents in {user-docs}

7. FEEDBACK
   Feedback is being collected from users. It will be saved as hashes in {feedback} with fields data, date and an optional user-id

8. USER TOKENS
     Stored in {user-tokens} slots with key as user-id and value an integer.

9. INPUT TOKENS 
     These store daily input tokens by model {input-tokens}:{model-name}:YYYY-MM-DD with value as an integer

10. OUTPUT TOKENS
     These store daily output tokens by model {output-tokens}:{model-name}:YYYY-MM-DD with value as an integer

11. FREE TRIAL TOKENS
     We have to know how much we're burning in free tokens. save in {trial-tokens-(input/output)}:{model-name}:YYYY-MM-DD with value as an integer


ANALYTICS
1. ACTIVE USERS: these are users who have spent atleast 1s on the site. otherwise, we register it as an event
{analytics}:{active}:{page}:YYYY-MM-DD with value as the count.
{analytics}:{event}:{page}:YYYY-MM-DD with value as count, active users are also recorded in events.
For a session to be registered as active, a beacon has to be sent from the user, other all requests are counted in events. 

2. DURATION
The time a user spends on a page. sent via the beacon when the page is closed.
Durations will be averages and so we will store the data in hash tables, storing the sum and then count, we just add the
latest duration to the sum and incr the count. Durations are floats
{analytics}:{duration}:{page}:YYYY-MM-DD with key as hash with fields: count, sum

3. EVENT DURATIONS:
These are timed events, forexample: how much time does a user take when they arrive on the page before they click on a link. or sending their first 
event.
I am more interested in knowing how much time a user spends on the landing page between sending data and time in between.

4. FREE TRIALS
     Every user gets a free trial. Only one, we keep track of them in a set at {analytics}:free-trial, everyone in the list has used up their's.
We store user-ids

")

(defparameter *test-email* "test@example.com")
(defparameter *test-user-id* nil)
(defparameter *test-cookie* "test-cookie")


(defun save-new-email (email)
  (let ((uuid (to-string (make-v4))))
    (redis:red-set (format nil "{email}:~a" email) uuid)
    (redis:red-set (format nil "{user-uuid}:~a" uuid) email)))
(deftest save-new-email (save-new-email *test-email*) "OK")

(defun get-user-uuid (email)
  (redis:red-get (format nil "{email}:~a" email)))
(deftest get-user-uuid (progn
			 (setq *test-user-id* (redis:red-get "{email}:test@example.com"))
			 (equal *test-user-id* (get-user-uuid *test-email*))) t)

(defun get-user-email (uuid)
  (redis:red-get (format nil "{user-uuid}:~a" uuid)))
(deftest get-user-email (equal *test-email* (get-user-email *test-user-id*)) t)

(defun save-access-code (email code)
  "Use email for access code because the code is email specific2. Save access code to {access-code} space with an expiry of 24 hours"
  (let* ((saved-uuid (redis:red-get (format nil "{email}:~a" email)))
	 (access-code-key (format nil "{access-code}:~a" email)))
    (unless saved-uuid
      (save-new-email email))
    (redis:red-set access-code-key code)
    (redis:red-expireat access-code-key (+ (get-universal-time) 86400))))
(deftest save-access-code (save-access-code *test-email* 1234) t)

(defun get-access-code (email)
  "return the access code for email"
  (redis:red-get (format nil "{access-code}:~a" email)))
(deftest get-access-code (get-access-code *test-email*) "1234")

(defun verify-access-code (email code)
  "return t/nil if email and code match"
  (string= code (get-access-code email)))
(deftest verify-access-code (verify-access-code *test-email* "1234") t)

(defun clear-test-email ()
  (let ((uuid (get-user-uuid "lam@ninx.xyz")))
    (redis:red-del "{email}:lam@ninx.xyz")
    (redis:red-del (format nil "{user-uuid}:~a" uuid))
    (redis:red-del (format nil "{consent}:~a" uuid)))
  (let ((keys (redis:red-keys "{cookie*")))
    (when keys
      (apply #'redis:red-del keys)))
  (let ((keys (redis:red-keys "{user-d*")))
    (when keys
      (apply #'redis:red-del keys))))

(defun test-code ()
  (redis:red-get (format nil "{access-code}:~a" "lam@ninx.xyz")))

(defun save-consent (email consent)
  "save whether accepts to be tracked for login or not"
  (redis:red-set (format nil "{consent}:~a" (get-user-uuid email)) consent))
(deftest save-consent (save-consent *test-email* t) "OK")

(defun get-consent (email)
  "check whether user consents to track logins or not"
  (redis:red-get (format nil "{consent}:~a" (get-user-uuid email))))
(deftest get-consent (get-consent *test-email*) "T")

(defun revoke-consent (email)
  "revoke consent"
  (redis:red-del (format nil "{consent}:~a" (get-user-uuid email))))
(deftest revoke-consent (revoke-consent *test-email*) 1)
(deftest get-consent-after-revoke (get-consent *test-email*) nil)

(defun save-cookie (email cookie persist)
  "save the cookie against an email, then if it is not to be persisted, set expiry for one day else expire in 10 years"
  (let* ((key (format nil "{cookie}:~a" cookie)))
    (redis:red-set key (get-user-uuid email))
    (redis:red-expireat key (+ (get-universal-time) (if persist 86400 315360000)))))
(deftest save-cookie (save-cookie "test@example.com" "test-cookie" t) t)

(defun get-uuid-from-cookie (cookie)
  "return a user uuid from a cookie"
  (redis:red-get (format nil "{cookie}:~a" cookie)))
(deftest get-uuid-from-cookie (equal *test-user-id* (get-uuid-from-cookie "test-cookie")) t)

(defun get-email-from-cookie (cookie)
  "return an email from a cookie"
  (get-user-email (redis:red-get (format nil "{cookie}:~a" cookie))))
(deftest get-email-from-cookie (equal "test@example.com" (get-email-from-cookie *test-cookie*)) t)

(defun delete-cookie (cookie)
  "delete a cookie"
  (redis:red-del (format nil "{cookie}:~a" cookie)))
(deftest delete-cookie (prog1 (delete-cookie *test-cookie*)
			 (save-cookie "test@example.com" *test-cookie* t)) 1)

(defun save-to-free-trial (cookie)
  "save a user's uuid to the free trial after they have used up their's"
  (redis:red-sadd "{analytics}:free-trial" (get-uuid-from-cookie cookie)))
(deftest save-to-free-trial (save-to-free-trial *test-cookie*) 1)

(defun has-free-trial-p (cookie)
  "check if a user has a free trial, if a uuid is already stored, return nil else t"
  (null (redis:red-sismember "{analytics}:free-trial" (get-uuid-from-cookie cookie))))
(deftest has-free-trial-p (has-free-trial-p *test-cookie*) nil)

;; DOCUMENT HANDLING
(defun compress-pdf-to-string (path)
  "When given the path of a file, compress it with zstd, return it as a string"
  (let* ((bytes (llms::read-binary-file-to-octets path))
	 (zstd-bytes (zstd:compress-buffer bytes))
	 (zstd-string (flexi-streams::octets-to-string  zstd-bytes)))
    zstd-string))

(defun decompress-string-to-bytes (string)
  "decompressed a zstd compressed string and returns it as a byte array"
  (zstd:decompress-buffer (flexi-streams::string-to-octets string)))

(defun save-document (path title)
  "when given a document, save it to the database, assigning it a uuid v4 and return (cons slot uuid). the document is saved as a string.
the documents should be distributed across several slots so that we don't congest one slot with the documents data since they will be heavy.
so we generate a random number upto 99, that's, we have 100 slots from which we can put the file.

note that the slot numbers are not the same as the kvrocks slot numbers, they are independent"
  (let* ((uuid (to-string (make-v4)))
	 (slot (random 100))
	 (id (format nil "{doc-slot-~a}:~a" slot uuid)))
    (redis:red-hset id "data" (compress-pdf-to-string path))
    (redis:red-hset id "title" title)
    (redis:red-hset id "date" (local-time:now))
    (cons slot uuid)))

(defun get-document-bytes (id)
  "when given a document id, return it's bytes."
  (let ((string (redis:red-get id)))
    (when string
      (decompress-string-to-bytes string))))

(defun save-user-document (cookie path title)
  "note that we begin from cookie, since that's what the application returns from the user"
  (let ((user-id (get-uuid-from-cookie cookie)))
    (format t "~%~a~%" user-id)
    (trivia:match (save-document path title)
      ((cons slot doc-uuid) 
       (let ((slot+doc-uuid (format nil "~a:~a" slot doc-uuid)))
	 (redis:red-rpush (format nil "{user-docs}:~a" user-id) slot+doc-uuid))))))

(defun get-user-documents (user-id)
  "get all documents belonging to a user"
  (redis:red-lrange (format nil "{user-docs}:~a" user-id) 0 -1))

(defun get-user-doc-details (cookie)
  "return the slot, id, title and date of a document"
  (mapcar (lambda (id)
	    (trivia:match (str:split ":" id)
	      ((list slot uuid)
	       (let ((doc-id (format nil "{doc-slot-~a}:~a" slot uuid)))
		 `(:slot ,slot :doc-id ,uuid :date ,(redis:red-hget doc-id "date") :title ,(redis:red-hget doc-id "title"))))))
	  (get-user-documents (get-uuid-from-cookie cookie))))

(defun get-doc-data (slot doc-id)
  (let* ((id (format nil "{doc-slot-~a}:~a" slot doc-id))
	 (string (redis:red-hget id "data")))
    `(:title ,(redis:red-hget id "title")
      :data ,(zstd:decompress-buffer (flexi-streams::string-to-octets string)))))

(defun get-only-doc-bytes (slot doc-id)
  (zstd:decompress-buffer (flexi-streams::string-to-octets (redis:red-hget (format nil "{doc-slot-~a}:~a" slot doc-id) "data"))))

(defun count-user-documents (cookie)
  "count the user documents"
  (length (get-user-documents (get-uuid-from-cookie cookie))))


;; FEEDBACK

(defun save-feedback (feedback cookie)
  "when cookie is given, save the user uuid as a field"
  (let ((id (to-string (make-v4))))
    (redis:red-hset id "feedback" feedback)
    (redis:red-hset id "date" (local-time:now))
    (when cookie
      (redis:red-hset id "user-id" (get-uuid-from-cookie cookie)))))

;; TOKENS

(defun incr-user-tokens (user-id tokens &optional cookie)
  "increment user tokens"
  (let ((id (format nil "{user-tokens}:~a" (if cookie (get-uuid-from-cookie cookie) user-id))))
    (if (redis:red-get id)
	(redis:red-incrby id tokens)
	(redis:red-set id tokens))))
(deftest incr-user-tokens (incr-user-tokens *test-user-id* 1) "OK")
(deftest incr-user-tokens-with-cookie (incr-user-tokens nil -1 *test-cookie*) 0)

(defun get-user-tokens (user-id &optional cookie)
  "return user tokens"
  (let ((tokens (redis:red-get (format nil "{user-tokens}:~a" (if cookie (get-uuid-from-cookie cookie) user-id)))))
    (if tokens tokens "0")))
(deftest get-user-tokens (get-user-tokens *test-user-id*) "0")
(deftest get-user-tokens-with-cookie (get-user-tokens nil *test-cookie*) "0")

(defun has-tokens-p (user-id &optional cookie)
  "check if a user has tokens, if no tokens, return nil else t"
  (null (equal "0" (get-user-tokens user-id cookie))))
(deftest has-tokens-p (has-tokens-p *test-user-id*) nil)
(deftest has-tokens-p-with-cookie (has-tokens-p nil *test-cookie*) nil)

(defun incr-input-tokens (tokens &optional (model "gtp-4o"))
  "add to the daily input tokens"
  (let ((id (format nil "{input-tokens}:{~a}:~a" model (make-date))))
    (if (redis:red-get id)
	(redis:red-incrby id tokens)
	(redis:red-set id tokens))))
;; when tests are run for the first time, this will return 0, otherwise will return 1
(deftest reset-test-model (let ((date (make-date)))
			    (redis:red-del (format nil "{input-tokens}:{test-model}:~a" date))
			    (redis:red-del (format nil "{output-tokens}:{test-model}:~a" date))
			    (redis:red-del (format nil "{trial-output-tokens}:{test-model}:~a" date))
			    (redis:red-del (format nil "{trial-input-tokens}:{test-model}:~a" date))) 1)
(deftest incr-input-tokens (incr-input-tokens 1 "test-model") "OK")

(defun get-input-tokens (&key (date (make-date)) (model "gpt-4o"))
  "return the input tokens for a given day, or the current date"
  (let ((tokens (redis:red-get (format nil "{input-tokens}:{~a}:~a" model date))))
    (if tokens tokens 0)))
(deftest get-input-tokens (get-input-tokens :model "test-model") "1")

(defun incr-output-tokens (tokens &optional (model "gtp-4o"))
  "add to the daily output tokens"
  (let ((id (format nil "{output-tokens}:{~a}:~a" model (make-date))))
    (if (redis:red-get id)
	(redis:red-incrby id tokens)
	(redis:red-set id tokens))))
(deftest incr-output-tokens (incr-output-tokens 1 "test-model") "OK")

(defun get-output-tokens (&key (date (make-date)) (model "gpt-4o"))
  "return the output tokens for a given day, or the current date"
  (let ((tokens (redis:red-get (format nil "{output-tokens}:{~a}:~a" model date))))
    (if tokens tokens 0)))
(deftest get-output-tokens (get-output-tokens :model "test-model") "1")

(defun incr-trial-input-tokens (tokens &optional (model "gtp-4o"))
  "add to the daily free trial input tokens"
  (let ((id (format nil "{trial-input-tokens}:{~a}:~a" model (make-date))))
    (if (redis:red-get id)
	(redis:red-incrby id tokens)
	(redis:red-set id tokens))))
(deftest incr-trial-input-tokens (incr-trial-input-tokens 1 "test-model") "OK")

(defun get-trial-input-tokens (&key (date (make-date)) (model "gpt-4o"))
  "return the daily trial tokens for a given day, or the current date"
  (let ((tokens (redis:red-get (format nil "{trial-input-tokens}:{~a}:~a" model date))))
    (if tokens tokens 0)))
(deftest get-trial-input-tokens (get-trial-input-tokens :model "test-model") "1")

(defun incr-trial-output-tokens (tokens &optional (model "gtp-4o"))
  "add to the daily free trial output tokens"
  (let ((id (format nil "{trial-output-tokens}:{~a}:~a" model (make-date))))
    (if (redis:red-get id)
	(redis:red-incrby id tokens)
	(redis:red-set id tokens))))
(deftest incr-trial-output-tokens (incr-trial-output-tokens 1 "test-model") "OK")

(defun get-trial-output-tokens (&key (date (make-date)) (model "gpt-4o"))
  "return the free trial output tokens for a given day, or the current date"
  (let ((tokens (redis:red-get (format nil "{trial-output-tokens}:{~a}:~a" model date))))
    (if tokens tokens 0)))
(deftest get-trial-output-tokens (get-trial-output-tokens :model "test-model") "1")

;; ANALYTICS
(defun incr-events (page)
  "add 1 to events of a given page."
  (let ((key (format nil "{analytics}:{event}:{~a}:~a" page (make-date))))
    (if (redis:red-get key)
	(redis:red-incr key)
	(redis:red-set key 1))))
;; same as the first reset test
(deftest reset-analytics (let ((date (make-date)))
			   (redis:red-del (format nil "{analytics}:{event}:{test-page}:~a" date))
			   (redis:red-del (format nil "{analytics}:{active}:{test-page}:~a" date))
			   (redis:red-del (format nil "{analytics}:{duration}:{test-page}:~a" date))
			   ) 0)
(deftest incr-events (incr-events "test-page") "OK")

(defun get-events (page &optional (date (make-date)))
  (redis:red-get (format nil "{analytics}:{event}:{~a}:~a" page date)))
(deftest get-events (get-events "test-page") "1")

(defun incr-active (page)
  "add 1 to active of a given page."
  (let ((key (format nil "{analytics}:{active}:{~a}:~a" page (make-date))))
    (if (redis:red-get key)
	(redis:red-incr key)
	(redis:red-set key 1))))
(deftest incr-active (incr-active "test-page") "OK")

(defun get-active (page &optional (date (make-date)))
  (redis:red-get (format nil "{analytics}:{active}:{~a}:~a" page date)))
(deftest get-active (get-active "test-page") "1")

(defun save-duration (page duration &optional (date (make-date)))
  "save a duration for a page visit, durations will be sent as string floats"
  (let* ((key (format nil "{analytics}:{duration}:{~a}:~a" page date))
	 (sum (redis:red-hget key "sum"))
	 (count (redis:red-hget key "count")))
    (format nil "~%~% here: 1~%~%")
    (cond
      ((and sum count)
       (redis:red-hset key "sum" (+ duration (read-from-string sum)))
       (redis:red-hset key "count" (+ (parse-integer count) 1)))
      (t (redis:red-hset key "sum" duration)
	 (redis:red-hset key "count" 1)))))
(deftest save-duration (save-duration "test-page" 1.00) t)

(defun get-duration (page &optional (date (make-date)))
  (let* ((key (format nil "{analytics}:{duration}:{~a}:~a" page date))
	 (sum (redis:red-hget key "sum"))
	 (count (redis:red-hget key "count")))
    (when (and sum count)
      (format nil "~,2f" (/ (read-from-string sum) (parse-integer count))))))
(deftest get-duration (get-duration "test-page") "1.00")


#|
we have to store data in the following ways. ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
1. the original data so that in case we need to reformat the data, we can do it later on. ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
2. indexes for the data: 		; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
i.  the first index is for auto complete. auto complete index will return the product name on querying it. ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
ii. the second index is the for ids, you want to match text to ids, such that when a user queries, you can return all products that match ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
string fragment				; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
					; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
the keys are				; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
1. {index}:{search} for which the zset will contain ids ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
2. {index}:{autocomplete} for which the zset will contain names					; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
it is important that the we use {index} to put all of them in the same slot to be able to use multikey functions ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
|#

(defun save-data (product-id name description price location site image-url url status condition)
  "when saving product data, we need an id to be used to update our saved product, more so the status for jiji.

all data os saved in {products} 
fragment indexes are created from name and description and saved in {index} for full text search"
  (let ((id (format nil "{product}:~a" product-id)))
    (if (redis:red-exists id)
	(format t "Product: ~a already saved ~%" name)
	(progn
	  (redis:red-hset id "product-id" product-id)
	  (redis:red-hset id "name" name)
	  (redis:red-hset id "description" description)
	  (redis:red-hset id "price" price)
	  (redis:red-hset id "location" location)
	  (redis:red-hset id "site" site)
	  (redis:red-hset id "image-url" image-url)
	  (redis:red-hset id "url" url)
	  (redis:red-hset id "status" status)
	  (redis:red-hset id "condition" condition)
	  (create-index id name)
	  (format t "Saved Product: ~a ~%" name)))))

(defun create-index (id name)
  "start with an id and name
tokenize the name. then for each token, save its fragments against both the id and name in two different sets.
forexample: tokens is saved in tok, toke, token, tokens fragments"
  (let ((tokens (nlp:tokenize name)))
    (dolist (token tokens)
      (when (>= (length token) 1)
	(save-to-index id name token)))))

(defun save-to-index (id name token &key (pos 1))
  #|given a word, start at length 1 then save the word fragments to {index}{name} and {index}{autocomplete}, 
   we use sorted sets, such that we can track the words appearing most in the dataset.|#
  (unless (> pos (length token))
    (let ((subtoken (str:substring 0 pos token)))
      (redis:red-zincrby (format nil "{index}{autocomplete}:~a" subtoken) 1 name)
      (redis:red-zincrby (format nil "{index}{search}:~a" subtoken) 1 id))
    (save-to-index id name token :pos (1+ pos))))

(defun get-autocomplete (txt)
  "given a fragment, get all product names for which it is part, return only 10 of the most frequent"
  (let* ((tokens (nlp:tokenize txt))
	 (names (mapcar (lambda (token)
			  (to-alist
			   (redis:red-zrange (format nil "{index}{autocomplete}:~a" (string-downcase token)) 0 -1 :withscores)))
			tokens)))
    (unless (equal '(nil) names)
      (let ((combined-alist (combine-alists names)))
	(if (<= (length combined-alist) 10)
	    (mapcar #'car combined-alist)
	    (mapcar #'car (subseq combined-alist 0 10)))))))

(defun get-search (txt)
  "given a fragment, get all product ids of it"
  (let* ((tokens (nlp:tokenize txt))
	 (names (mapcar (lambda (token)
			  (to-alist
			   (redis:red-zrange (format nil "{index}{search}:~a" (string-downcase token)) 0 -1 :withscores)))
			tokens)))
    (unless (equal '(nil) names)
      (mapcar #'car (combine-alists names)))))

(defun to-alist (lst)
  "Convert a list of elements into an alist assuming alternating key-value pairs."
  (loop for (key value) on lst by #'cddr
        collect (cons key value)))

(defun combine-alists (alist-list)
  "Combine a list of alists, summing integer values for each key."
  (let ((result (make-hash-table :test 'equal)))
    (dolist (alist alist-list)
      (dolist (pair alist)
        (let* ((key (car pair))
               (value (parse-integer (cdr pair)))
               (current (gethash key result 0)))
          (setf (gethash key result) (+ current value)))))
    ;; Convert hash table to alist
    (let ((combined-alist nil))
      (maphash (lambda (key value)
                 (push (cons key value) combined-alist))
               result)
      (sort combined-alist (lambda (a b) (> (cdr a) (cdr b)))))))
