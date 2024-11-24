;; handles communication with paypal api.

(in-package :paypal)

(defvar *paypal-client-id* (uiop:getenv "PAYPAL_CLIENT_ID"))
(defvar *paypal-client-secret* (uiop:getenv "PAYPAL_CLIENT_SECRET"))

(defun get-access-token ()
  "get the access token from paypal"
  (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
      (drakma:http-request "https://api.paypal.com/v1/oauth2/token"
			   :content  "grant_type=client_credentials"
			   :method :post
			   :basic-authorization (list *paypal-client-id* *paypal-client-secret*))
    (declare (ignore response-code response-headers request-uri flexi-response response-bool status-text))
    (gethash "access_token" (jzon:parse (flexi-streams:octets-to-string response)))))
  
(defun get-transaction-details (id)
  "get details of a single transaction"
    (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
      (drakma:http-request "https://api-m.paypal.com/v1/reporting/transactions"
			   :parameters `(("transaction_id" . ,id)
					 ("fields" . "all")
					 ("start_date" . ,(make-date (* 86400 2)))
					 ("end_date" . ,(make-date (* 2 -86400)));; this ensures all transactions to date are returned as it's two days ahead
					 )
			   :additional-headers `(("Authorization". ,(format nil "Bearer ~a" (get-access-token)))))
      (declare (ignore response-code response-headers request-uri flexi-response response-bool status-text))
      (let ((details (aref (gethash "transaction_details" (jzon:parse (flexi-streams:octets-to-string response))) 0)))
	`(:transaction-id ,id :transaction-date ,(car (str:split "T" (hash-get details '("transaction_info" "transaction_updated_date"))))
			  :amount ,(hash-get details '("transaction_info" "transaction_amount" "value"))
			  :email ,(hash-get details '("payer_info" "email_address"))))))

(defun get-transactions (&key (days 2))
  "get a list of transactions, days makes how many days ago should the transactions be. return a list of transactions"
  (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
      (drakma:http-request "https://api-m.paypal.com/v1/reporting/transactions"
			   :parameters `(("fields" . "all")
					 ("start_date" . ,(make-date (* 86400 days)))
					 ("end_date" . ,(make-date (* days -86400))) ;; this ensures all transactions to date are returned as it's two days ahead
					 )
			   :additional-headers `(("Authorization". ,(format nil "Bearer ~a" (get-access-token)))))
    (declare (ignore response-code response-headers request-uri flexi-response response-bool status-text))
    (let ((transactions (gethash "transaction_details" (jzon:parse (flexi-streams:octets-to-string response)))))
      (unless (equalp #() transactions)
	(loop for transaction across transactions collect
	      (hash-get transaction '("transaction_info" "transaction_id")))))))

(defun create-success-webhook ()
  "this function will create a webhook for listening to success"
  (let ((body (jzon:stringify (hash-create `(("url" "https://ninx.xyz")
					     ("event_types" (,(hash-create `(("name" "PAYMENT.CAPTURE.COMPLETED")
									     ("description" "Payment completed"))))))))))
    (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
	(drakma:http-request "https://api-m.paypal.com/v1/notifications/webhooks"
			     :method :post
			     :content-type "application/json"
			     :content body
			     :additional-headers `(("Authorization". ,(format nil "Bearer ~a" (get-access-token)))))
      (declare (ignore response-code response-headers request-uri flexi-response response-bool status-text))
      (format t "body: ~a~%response: ~a~%~%R: ~a~%~%"
	      body response
	      (flexi-streams:octets-to-string response))
      )))


(defun get-webhooks ()
  "this function lists webhooks"
  (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
      (drakma:http-request "https://api-m.paypal.com/v1/notifications/webhooks"
			   :content-type "application.json"
			   :additional-headers `(("Authorization". ,(format nil "Bearer ~a" (get-access-token)))))
    (declare (ignore flexi-response response-code response-headers request-uri response-bool status-text))
    (format t "~a~%~%~%F response: ~a~%~%"
	    response
	    (flexi-streams:octets-to-string response)
	    )
    ))

(defparameter *test-hook* "19W52136L5237754B")

(defun delete-webhook (id)
  "this function lists webhooks"
  (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
      (drakma:http-request (format nil "https://api-m.paypal.com/v1/notifications/webhooks/~a" id)
			   :method :delete
			   :additional-headers `(("Authorization". ,(format nil "Bearer ~a" (get-access-token)))))
    (declare (ignore flexi-response response-code response-headers request-uri response-bool status-text))
    (format t "~a~%~%~%F response: ~a~%~%"
	    response
	    (flexi-streams:octets-to-string response)
	    )
    ))

(defun simulate-webhook (url)
  "this function lists webhooks"
  (let ((body (jzon:stringify (hash-create `(("url" ,url)
					     ("event_type" "PAYMENT.CAPTURE.COMPLETED")
					     ("resource_version" "1.0"))))))
    (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
	(drakma:http-request "https://api-m.paypal.com/v1/notifications/simulate-event"
			     :method :post
			     :content-type "application/json"
			     :content body
			     :additional-headers `(("Authorization". ,(format nil "Bearer ~a" (get-access-token)))))
      (declare (ignore flexi-response response-code response-headers request-uri response-bool status-text))
      (format t "response: ~a~%~%"
	      (stringify (gethash "resource" (jzon:parse (flexi-streams:octets-to-string response))))
	      )
      )))

(defun make-date (&optional diff)
  "make a date compliant with the paypal api, when given the diff, subtract the given seconds from the current unix timestamp."
  (let ((d (local-time:format-rfc3339-timestring nil (local-time:universal-to-timestamp (- (get-universal-time) (if diff diff 0))))))
    (str:replace-all ".000000" "" d)))
