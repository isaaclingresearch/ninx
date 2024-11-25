(in-package :decklm)

;; define the variables that are used in this specific application.
(defvar *decklm-url* (uiop:getenv "DECKLM_HOST"))
(defvar *decklm-host* (format nil "~a:~a" *decklm-url* ninx:*ninx-https-port*))
(defparameter *payments-timer* nil)
(defparameter *city-db* (make-mmdb (namestring (truename "~/common-lisp/ninx/apps/decklm/geoip/GeoLite2-City.mmdb"))))

(defun start-decklm (&key (schedule-payments t))
  "this will start decklm specific components"
  (when schedule-payments
    (setf *payments-timer* (schedule-payments-update)))
  ;; start the python client and load python-pptx
  (py4cl2:pystart)
  (py4cl2:pyexec "import importlib.util")
  (py4cl2:pyexec "import sys")
  (setf (pyeval "module_path") (format nil "'~a'" (namestring (truename #p"~/common-lisp/ninx/apps/decklm/src/py/decklm.py"))))
  (setf (pyeval "spec") "importlib.util.spec_from_file_location('decklm', module_path)")
  (setf (pyeval "module") "importlib.util.module_from_spec(spec)")
  (py4cl2:pyeval "spec.loader.exec_module(module)")
  (setf (pyeval "sys.modules['decklm']") "module")
  (pyexec "import decklm")
  (pyexec "decklm.test_make_pptx()"))

(defun stop-decklm ()
  "Stop decklm specific components"
  (when *payments-timer*
    (sb-ext:unschedule-timer *payments-timer*))
  (py4cl2:pystop))

(defun restart-decklm (&key (schedule-payments t))
  (stop-decklm)
  (start-decklm :schedule-payments schedule-payments))

(flet ((test-path (request)
	 (cl-ppcre:scan "^(/test/)" (script-name* request))))

  (hunchentoot:define-easy-handler (test :uri #'test-path :acceptor-names '(ninx::ninx)) ()
    "testing12"))


(defun ws-decklm (endpoint ws-user message-json)
  "process a text message sent via websocket"
  (let* ((message (jzon:parse message-json))
	 (message-type (gethash "type" message nil)))
    (trivia:match message-type
      ("consent" (let* ((email (gethash "email" message))
			(choice (gethash "choice" message)))
		   (save-consent email choice)
		   (let* ((cookie (save-cookie email :persist choice)))
		     (hunchensocket:send-text-message ws-user
						      (jzon:stringify (hash-create `(("type" "consent")
										     ("remember" ,choice)
										     ("cookie" ,cookie))))))))
      ("email" (let* ((access-code (+ 999 (random 9000)))
		      (email (gethash "email" message))
		      (email-old-p (get-user-id email)))
		 (save-new-email email)
		 (save-access-code email access-code)
		 (send-access-code email access-code)
		 (hunchensocket:send-text-message ws-user
						  (jzon:stringify (hash-create `(("type" "access-code-sent")
										 ("new" ,(null email-old-p))))))))
      ("access-code" (let* ((access-code (gethash "code" message))
			    (email (gethash "email" message))
			    (consent (get-consent email))
			    (cookie (save-cookie email :persist consent)))
		       (hunchensocket:send-text-message ws-user
							(jzon:stringify (hash-create (if (verify-access-code email (parse-integer access-code))
											 `(("cookie" ,cookie)
											   ("remember" ,consent)
											   ("type" "correct-code"))
											 `(("type" "incorrect-code")))))))))))

(defun send-access-code (email code)
  "send an email with access code to the given email"
  (cl-smtp:send-email "mail.decklm.com" "care@decklm.com" email "DeckLM: Access Code."
		      ""
		      :authentication `("care@decklm.com" ,(uiop:getenv "POSTGRES_PASSWORD"))
		      :ssl :tls
		      :display-name "DeckLM"
		      :html-message (access-code-html code)
		      ))

(defun access-code-html (code)
  (let ((black "#1a1a1a")
	(green "#00b800")
	(blue "#1e90ff"))
    (with-html-output-to-string (*standard-output*)
      (htm (:body :style (cl-css:inline-css `(:background-color ,black :color ,green))
		  (:table :cellpadding "0" :cellspacing "0"
		    :style (cl-css:inline-css `(:width 100% :height 100%))
		    (:tr 
		     (:td :align "center"
			  (:table :width "600" :cellpadding "0" :cellspacing "0"
			    (:tr
			     (:td :align "center" :style (cl-css:inline-css '(:padding 20px))
				  (:h1 "DeckLM ")))
			    (:tr
			     (:td :align "center"
				  (:p "This code has sent been after you attempted to Signin/Signup with this email. Use it to access your account.")))
			    (:tr (:td :align "center" :style (cl-css:inline-css '(:padding 20px))
				      (:a :href "#" :style (cl-css:inline-css `(:margin-top 10px :padding "10px 20px" :background-color ,blue
										:color ,black :text-decoration none :border-radius 4px)) 
					  (str code))))
			    (:tr
			     (:td :align "center" :style (cl-css:inline-css '(:padding 20px))
				  (:p "Use this email when you have a question or complaint.")
				  (:p "© 2024 Ninxtech Ltd.")
				  )))))))))))

(defun get-current-year ()
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time (get-universal-time))
    year))


(defun beacon-js ()
  "this details a function a for a client to send back a beacon after 1 second of activity"
  (ps:ps
    (defun-async send-beacon-after-delay ()
      (await (new (-promise (=> resolve (set-timeout resolve 1000)))))
      (ps:chain navigator (send-beacon "/register-active"
				       (new (-u-r-l-search-params (create pathname (aref (ps:chain window location pathname (split "/")) 1)))))))

    (send-beacon-after-delay)))


(defun duration-js ()
  "this js will measure how long a user has lasted on a given page"
  (ps:ps
    (let ((page-load-time (ps:chain performance (now))))
      (defun send-time-spent ()
	(let* ((time-spent (- (ps:chain performance (now)) page-load-time))
	       (data (new (-u-r-l-search-params (create duration time-spent
							pathname (aref (ps:chain window location pathname (split "/")) 1))))))
	  (ps:chain navigator (send-beacon "/register-duration" data))))
      (ps:chain window (add-event-listener "beforeunload" send-time-spent)))))

(defun ws-js-code ()
  "generate the code for websockets and handling of the back and forth between client and server"
  (parenscript:ps
    (defun privacy-and-data ()
      "this function: gives the user some details about the site. asks for consent to store a cookie.
consent is stored server side, such that we track the user across all devices."
      (let ((control (ps:chain document (get-element-by-id "control"))))
	(ps:chain control (insert-adjacent-h-t-m-l
			   "beforebegin"
			   (who-ps-html
			    (:p :class "message-p" "Code verified, account created successfully.")
			    :br
			    (:div :class "consent" :id "consent"
				  (:b (:u "Privacy and Data Policy"))
				  (:div :id "privacy-div" :class "privacy-div"
					(:p "We use one cookie to keep you logged in. We require consent to store this cookie and remember you on your device, otherwise, you will have to login every time.")
					(:p "Consent is asked for once and stored on our servers. You can later revoke this consent by clicking on 'Don't save cookie' on the home page. We also Google analytics to measure performance of our campaigns") 
					(:p "All images and documents submitted are only stored during making of the slide deck, after which they are deleted. The decks generated are stored on our servers for you to access later on.")
					(:p "Do you wish to be remembered when you login? Enter Yes or No below and press Enter to continue."))
				  (:div "Yes/No: "
					(:input :type "text" :id "consent-input" :placeholder "Enter Yes/No here." :value "Yes")
	               			(:img :class "send-icon" :id "send-icon" :src "/decklm/icons/send.png" :alt "Send" :onclick "consentVerification()"))))))
	(scroll-to-bottom)
	(bind-consent-input)))
    
    (defun set-js-cookie (name value exdays)
      "set a cookie with name value and expiry days"
      (let ((d (new (-date)))
	    (exp (* exdays 24 60 60 1000)))
	(ps:chain d (set-time (+ (ps:chain d (get-time)) exp)))
	(let ((expires (+ "expires=" (ps:chain d (to-u-t-c-string)))))
	  (setf (ps:chain document cookie) (+ name "=" value ";" expires  ";path=/")))))
    
    (setf *socket* (new (-web-socket "/ws/decklm")))
    (setf (ps:chain *socket* onopen) (lambda () (ps:chain console (log "connected to server"))))
    (setf (ps:chain *socket* onmessage)
	  (lambda (event)
	    (let* ((data (ps:chain -j-s-o-n (parse (ps:chain event data))))
		   (type (ps:chain data type))
		   (control (ps:chain document (get-element-by-id "control"))))
	      (cond
		((eql "consent" type)
		 (let ((cookie (ps:chain data cookie))
		       (remember (ps:chain data remember)))
		   (ps:chain console (log remember))
		   (if remember
		       (set-js-cookie "cookie" cookie 3650)
		       (set-js-cookie "cookie" cookie 0))))
		
		((eql "access-code-sent" type)
		 (setf *new-account-p* (ps:chain data new))
		 (setf (ps:chain document (get-element-by-id "email-loading-caret") disabled) t)
		 (ps:chain control (insert-adjacent-h-t-m-l
				    "beforebegin"
				    (who-ps-html
				     :br
				     (:p :class "access-code-message" :id "access-code-message")
				     (:div
				      "Enter access here: "
				      (:input :type "number" :id "access-code" :class "access-code" :placeholder "Enter access code here")
				      (:img :class "send-icon" :id "send-icon" :src "/decklm/icons/send.png" :alt "Send" :onclick "accessCodeVerification()")))))
		 (type-writer "access-code-message" "An access code has been sent to your email. Enter it below to continue" 0)
		 ;;	 (ps:chain document (get-element-by-id "access-code") (focus))
		 (scroll-to-bottom)
		 (bind-access-code-input))
		((eql "correct-code" type)
		 ;; when the code is correct, set the cookies depending on how the user wants to be remembered.
		 (setf (ps:chain document (get-element-by-id "code-loading-caret") disabled) t) ;; disable the caret
		 (let ((cookie (ps:chain data cookie))
		       (remember (ps:chain data remember)))
		   (if remember
		       (set-js-cookie "cookie" cookie 3650)
		       (set-js-cookie "cookie" cookie 0)))
		 (if *new-account-p*
		     (privacy-and-data)
		     (setf (ps:chain window location href) "/home")
		     ))
		((eql "incorrect-code" type)
		 (let ((error-paragraph (ps:chain document (get-element-by-id "code-error-paragraph")))
		       (code-input (ps:chain document (get-element-by-id "access-code"))))
		   (when error-paragraph
		     (setf (ps:chain error-paragraph id) ""))
		   (setf (ps:chain code-input id) "")
		   (setf (ps:chain code-input class-name) "")
		   (setf (ps:chain document (get-element-by-id "code-loading-caret") disabled) t)
		   (ps:chain control (insert-adjacent-h-t-m-l
				      "beforebegin"
				      (who-ps-html
				       :br
				       (:p :class "error-paragraph" :id "code-error-paragraph"))))
		   (type-writer "code-error-paragraph" "The code you entered is incorrect, try again with the correct code." 0)
		   (ps:chain control (insert-adjacent-h-t-m-l "beforebegin"
							      (who-ps-html
							       (:p :class "access-code-message"
								   :id "access-code-message"
								   (:div "Enter access code: "
									 (:input :type "number"
										 :id "access-code"
										 :class "access-code"
										 :placeholder "Enter access code here")
									 (:img :class "send-icon" :id "send-icon" :src "/decklm/icons/send.png" :alt "Send" :onclick "accessCodeVerification()"))))))
		   (ps:chain document (get-element-by-id "access-code") (focus))
		   (bind-access-code-input)
		   (scroll-to-bottom)))))))
    (setf (ps:chain *socket* onclose) (lambda () ((ps:chain console log) "socket closed!")))
    (setf (ps:chain *socket* onerror) (lambda (err) ((ps:chain console log) err)))))

(defun pre-js ()
  "js code before the forms are loaded, anything that needs to run before the body goes here."
  (parenscript:ps
    (defun scroll-to-bottom ()
      (ps:chain window (scroll-to (create
				   top (ps:chain document body scroll-height)
				   behavior "smooth"))))
    (defun check-cookie (name)
      "check if cookie exists"
      (let* ((cookies (ps:chain document cookie))
	     (cookie-array (ps:chain cookies (split ";"))))
	(loop for cookie across cookie-array do
	  (let* ((kv (ps:chain cookie (split "=")))
		 (key (ps:chain (aref kv 0) (trim))))
	    (if (eql name key)
		(return t))))
	))
    ;; redirect the user if cookie is set
    (if (check-cookie "cookie")
	(setf (ps:chain window location href) "/home"))
    ))


(defun typing-effect-js ()
  "Write the data to a page after the DOM has been fully loaded"
  (parenscript:ps
    (setf speed 10)
    (setf email-regex (regex "/^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/"))
    
    (defun type-writer (elem str i)
      (let ((element (ps:chain document (get-element-by-id elem))))
	(if (< i (ps:chain str length))
	    (progn
	      (setf (ps:chain element inner-h-t-m-l)
		    (+ (ps:chain element inner-h-t-m-l) (ps:chain str (char-at i))))
	      (set-timeout (lambda () (type-writer elem str (1+ i))) speed))
	    (progn
	      (setf (ps:chain document (get-element-by-id "first-form") style display) "block")
	      ;;  (ps:chain document (get-element-by-id "email") (focus))
	      ))))

    ;; TODO: FINAL VERIFICATION.

    (defun final-verification ()
      (disable-send-icon)
      (let* ((final (ps:chain document (get-element-by-id "final-caret")))
	     (choice (ps:chain final value (to-lower-case)))
	     (control (ps:chain document (get-element-by-id "control"))))
	(cond
	  ((or (eql choice "no")) (setf (ps:chain window location href) "/home"))
	  ((eql choice "yes") (setf (ps:chain window location href) "https://www.paypal.com/ncp/payment/H6QDGXUTHGCWJ"))
	  (t ;; this means the user entered a wrong option, we must refresh the final-caret
	   (setf (ps:chain final id) "")
	   (ps:chain control
		     (insert-adjacent-h-t-m-l
		      "beforebegin"
		      (who-ps-html
		       (:p :class "error-paragraph" "Please answer Yes/No. ")
		       (:div "Yes/No: "
			     (:input :type "text" :id "final-caret" :placeholder "Enter Yes/No here." :value "Yes")
	        	     (:img :class "send-icon" :id "send-icon" :src "/decklm/icons/send.png" :alt "Send" :onclick "finalVerification()")))))
	   (ps:chain document (get-element-by-id "final-caret") (focus))
	   (scroll-to-bottom)
	   (bind-final-input)))))
    
    (defun bind-final-input ()
      (ps:chain document (get-element-by-id "final-caret")
		(add-event-listener
		 "keyup"
		 (lambda (event)
		   (when (eql (ps:chain event key) "Enter")
		     (final-verification))))))

    (defun pricing-verification ()
      (disable-send-icon)
      (let ((control (ps:chain document (get-element-by-id "control"))))
	(ps:chain document (get-element-by-id "pricing-caret") (blur))
	(ps:chain control
		  (insert-adjacent-h-t-m-l
		   "beforebegin"
		   (who-ps-html
		    :br
		    (:div
		     (:b (:u "Tutorial"))
		     (:p
		      "You can generate slides from images or documents."
		      " Supported documents are PDF, Word, and Excel documents"
		      :br :br
		      "You can optionally give a topic for which the slides will be made. If topic is not given, the slides will be about everything in the image(s) or document(s). " "Images are cheaper than documents."
		      :br :br
		      "To Buy tokens, enter Yes. To start your free trial, enter No.")
		     (:div "Yes/No: "
			   (:input :type "text" :id "final-caret" :placeholder "Yes/No." :value "Yes")
			   (:img :class "send-icon" :id "send-icon" :src "/decklm/icons/send.png" :alt "Send" :onclick "finalVerification()"))))))
	(scroll-to-bottom)
	(bind-final-input)))
    
    (defun bind-pricing-input ()
      (ps:chain document (get-element-by-id "pricing-caret")
		(add-event-listener
		 "keyup"
		 (lambda (event)
		   (when (eql (ps:chain event key) "Enter")
		     (pricing-verification))))))

    (defun consent-verification ()
      (disable-send-icon)
      (let* ((consent (ps:chain document (get-element-by-id "consent-input")))
	     (choice (ps:chain consent value (to-lower-case)))
	     (email (ps:chain document (get-element-by-id "email") value))
	     (control (ps:chain document (get-element-by-id "control"))))
	(setf (ps:chain consent disabled) t)
	(cond
	  ((or (eql choice "yes") (eql choice "no"))
	   (let ((choice-var (if (eql choice "yes") t nil)))
	     (ps:chain *socket* (send (ps:chain -j-s-o-n (stringify (create type "consent"
									    email email
									    choice choice-var))))))
	   (ps:chain control
		     (insert-adjacent-h-t-m-l
		      "beforebegin"
		      (who-ps-html
		       :br
		       (:p :class "message-p" (if (eql choice "yes")
						  "You will be remembered when you login."
						  "You will be required to login after every session ends."))
		       :br
		       (:div :class "pricing"
			     (:b (:u "Pricing and Payment"))
			     (:p "Create your first slide deck for free!")
			     (:p "Each token covers about 0.75 words. Tokens are used based on the number of words in your uploaded images/documents and in the slide deck generated. Tokens will be automatically deducted from your balance.")
			     (:p "Get 1 million tokens (enough for approximately 750,000 words) for just $25. Token bundles are available in $5, $25, $100, and $1000 options.")
			     (:p "We only accept payments via PayPal. Be sure to use the same email here as in your PayPal transaction.")
			     )

		       (:div "Enter Yes and press Enter to continue: "
			     (:input :id "pricing-caret" :placeholder "Enter Yes here." :value "Yes")
			     (:img :class "send-icon" :id "send-icon" :src "/decklm/icons/send.png" :alt "Send" :onclick "pricingVerification()")))))
	   (scroll-to-bottom)
	   (bind-pricing-input) ;add an event listener to the pricing-caret
	   )
	  (t ;; this means the user entered a wrong option, we must refresh the consent-input
	   (setf (ps:chain consent id) "")
	   (ps:chain control
		     (insert-adjacent-h-t-m-l
		      "beforebegin"
		      (who-ps-html
		       (:p :class "error-paragraph" "Please answer Yes/No. ")
		       (:div "Yes/No: "
			     (:input :type "text" :id "consent-input" :placeholder "Enter Yes/No here and press Enter")
			     (:img :class "send-icon" :id "send-icon" :src "/decklm/icons/send.png" :alt "Send" :onclick "consentVerification()")))))
	   (ps:chain document (get-element-by-id "consent-input") (focus))
	   (scroll-to-bottom)
	   (bind-consent-input)))))

    (defun bind-consent-input ()
      (ps:chain document (get-element-by-id "consent-input")
		(add-event-listener
		 "keyup"
		 (lambda (event)
		   (when (eql (ps:chain event key) "Enter")
		     (consent-verification))))))

    (defun access-code-verification ()
      "send access code for verification"
      (let* ((email (ps:chain document (get-element-by-id "email") value))
	     (code-input (ps:chain document (get-element-by-id "access-code")))
	     (code (ps:chain code-input value))
	     (code-loading-div (ps:chain document (get-element-by-id "code-loading-div")))
	     (code-loading-caret (ps:chain document (get-element-by-id "code-loading-caret")))
	     (control (ps:chain document (get-element-by-id "control"))))
	(setf (ps:chain code-input disabled) t)
	(disable-send-icon)
	(when (not (eql "" code))
	  (when (and code-loading-div code-loading-caret)
	    (setf (ps:chain code-loading-div id) "")
	    (setf (ps:chain code-loading-caret id) ""))
	  (ps:chain control (insert-adjacent-h-t-m-l "beforebegin"
						     (who-ps-html
						      :br
						      (:div
						       (:i :class "code-loading-div" :id "code-loading-div")
						       (:input :class "code-loading-caret" :id "code-loading-caret")))))
	  (type-writer "code-loading-div" "Verifying code, please wait..." 0)
	  (scroll-to-bottom)
	  (ps:chain document (get-element-by-id "code-loading-caret") (focus))
	  (ps:chain *socket* (send (ps:chain -j-s-o-n (stringify (create type "access-code"
									 email email
									 code code))))))))
    (defun bind-access-code-input ()
      (ps:chain document (get-element-by-id "access-code")
		(add-event-listener
		 "keyup"
		 (lambda (event)
		   (when (eql (ps:chain event key) "Enter")
		     (access-code-verification))))))
    
    (defun disable-send-icon ()
      "this function will remove the onclick properties of send icon to allow it to be reusable"
      (let* ((send-icon (ps:chain document (get-element-by-id "send-icon"))))
	(setf (ps:chain send-icon onclick) null)
	(setf (ps:chain send-icon id) null)))
    
    (defun email-verification ()
      "verify that email is an email"
      (let* ((email-input (ps:chain document (get-element-by-id "email")))
	     (email (ps:chain email-input value))
	     (parent (ps:chain document (get-element-by-id "parent")))
	     (error-paragraph (ps:chain document (get-element-by-id "error-paragraph")))
	     (control (ps:chain document (get-element-by-id "control"))))
	(setf (ps:chain email-input disabled) t)
	(disable-send-icon)
	(if (ps:chain email-regex (test email))
	    (progn
	      (ps:chain control (insert-adjacent-h-t-m-l "beforebegin"
							 (who-ps-html
							  (:div
							   (:i :class "email-loading-div" :id "email-loading-div")
							   (:input :id "email-loading-caret" :class "email-loading-caret")))))
	      (type-writer "email-loading-div" "Loading, please wait..." 0)
	      (ps:chain document (get-element-by-id "email-loading-caret") (focus))
	      (scroll-to-bottom)
	      (ps:chain *socket* (send (ps:chain -j-s-o-n (stringify (create email email
									     type "email"))))))
	    (progn
	      (when error-paragraph
		(setf (ps:chain error-paragraph id) ""))
	      (setf (ps:chain email-input id) "")
	      (setf (ps:chain email-input class-name) "")
	      (ps:chain control (insert-adjacent-h-t-m-l "beforebegin"
							 (who-ps-html
							  (:p :class "error-paragraph"  :id "error-paragraph"))))
	      (type-writer "error-paragraph" "You have entered an incorrect email. Enter a correct email and try again." 0)
	      (ps:chain control (insert-adjacent-h-t-m-l "beforebegin"
							 (who-ps-html
							  (:div :class "email-input"
								"Enter correct email here: "
								(:input :type "email" :id "email" :class "email-input" :autocomplete "off" :placeholder "Email goes here.")
								(:img :class "send-icon" :id "send-icon" :src "/decklm/icons/send.png" :alt "Send" :onclick "emailVerification()")))))
	      (scroll-to-bottom)
	      (bind-email-input)))))

    (defun bind-email-input ()
      "this function resets email verification onto the new email input after an error"
      (ps:chain document (get-element-by-id "email")
		(add-event-listener
		 "keyup"
		 (lambda (event)
		   (when (eql (ps:chain event key) "Enter")
		     (email-verification))))))
    (bind-email-input)))


(defun accounts-css ()
  "the css for the accounts page"
  (let ((fg-color "#1a1a1a")
	(red "#FF6060")
	(blue "#CCCCFF")
	(bg-color "#e8e8e8")
	(link-blue "#1e90ff"))
    (cl-css:css
     `((body :background-color ,bg-color :color ,fg-color :margin 20px :font-size 16px)
       ("@media only screen and (min-width: 768px)" (body :font-size 18px :width 80% :margin-left 10%))
       (p :text-align "justify")
       (.first-form :display none)
       (.error-paragraph :color ,red)
       (".message-p" :color ,link-blue)
       (.separator :color ,link-blue)
       (input :width 50% :background-color ,bg-color :color ,fg-color :border 0 :border-bottom "1px solid" :outline none :font-size 16px)
       ("input::placeholder" :color ,fg-color)
       ("input:focus" :width 50% :outline none :background-color ,bg-color :color ,fg-color :caret-color ,fg-color)
       ("input:disabled" :width 50% :outline none :background-color ,bg-color :color ,fg-color :caret-color ,fg-color :border 0)
       ("a:hover::after" :color ,link-blue)
       ("a:visited" :color ,link-blue :decoration none)
       ("a:hover" :color ,link-blue :decoration underline)
       ("a.logo-link" :color ,fg-color :text-decoration none :margin-left 0)
       ("a.logo-link:hover" :color ,link-blue)
       (a :color ,link-blue :text-decoration none :decoration none :margin-left 10px)
       ("a:not(.logo-link)::after" :content "\"↪\"" :font-weight "bold" :color "inherit" :vertical-align baseline)
       (.logo :display flex :justify-content left :flex-direction :row :align-items :center :gap "10px")
       ("a.logo-link" :text-align center :color ,link-blue :text-decoration none :font-size 30px)
       (.logo-image :width 50px)

       (.code-loading-caret :border 0)
       (.email-loading-caret :border 0)
       (.send-icon :width 24px :height 24px :margin-top 12px :cursor pointer)
       ("@media only screen and (max-width: 768px)"
	(input :width 90%)
	("input:focus" :width 90%)
	("input:disabled" :width 90%))))))

(define-easy-handler (accounts :uri "/accounts"
			       :acceptor-names '(ninx::ninx)
			       :host *decklm-host*) ()
  (incr-events "accounts")
  (save-ip-visit (remote-addr* *request*))
  (save-country-visit (remote-addr*))
  (with-html-output-to-string (*standard-output*)
    "<!DOCTYPE html>"
    (:html :lang "en"
           (:head
            (:title "DeckLM")
            (:meta :charset "UTF-8")
            (:link :rel "manifest" :href "/decklm/manifest.json")
	    (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
            (:meta :name "description" :content "The accounts page for DeckLM")
            (:link :rel "icon" :href "/decklm/static/icons/web/favicon.ico" :sizes "any")
            (:link :rel "apple-touch-icon" :href "/decklm/static/icons/web/apple-touch-icon.png")
            (:style (str (accounts-css)))
	    (:script (str (pre-js)))
	    (:script (str (duration-js))))
           (:body :id "parent"
		  (:a :href "/" :class "logo-link"
		      (:div :class "logo" (:img :src "/decklm/icon-512.png" :class "logo-image")
			    "DeckLM"))
		  (if (is-mobile-browser *request*)
		      (htm (:div :class "home-links"
				 (:a :href "https://www.paypal.com/ncp/payment/H6QDGXUTHGCWJ" "Buy tokens")
			    :br
			    (:a :href "/pricing" "Pricing")
			    :br
			    (:a :href "/terms-and-privacy" "Terms and Privacy")))
		      (htm (:div :class "home-links"
				 (:a :href "https://www.paypal.com/ncp/payment/H6QDGXUTHGCWJ" "Buy tokens")
				 (:span :class "separator" " |")
				 (:a :href "/pricing" "Pricing")
				 (:span :class "separator" " |")
				 (:a :href "/terms-and-privacy" "Terms and Privacy"))))
		  (:p :id "first-paragraph" :class "first-paragraph")
		  (:div :id "first-form" :class "first-form"
			"Enter email here: "
			(:input :type "email" :id "email" :class "email-input" :autocomplete "off" :placeholder "Email goes here.")
			(:img :class "send-icon" :src "/decklm/icons/send.png" :alt "Send" :id "send-icon" :onclick "emailVerification()"))
		  (:div :id "control")
		  (:script (str (ws-js-code)))
		  (:script (str (beacon-js)))
		  (:script (str (typing-effect-js)))
		  (:script
		   (str
		    (ps:ps
		      (ps:chain document (add-event-listener "DOMContentLoaded"
							     (lambda ()
							       ;; Call type-writer function after DOM is loaded
							       (type-writer "first-paragraph" "Make a slide deck from your image and PDF references in a minute. Enter your email below to start." 0))))
		      )))))))

(defun is-mobile-browser (request &optional agent)
  "check if a given agent is a mobile browser"
  (let ((user-agent (if agent agent (cdr (find :user-agent (headers-in* request) :key #'car)))))
    (cond
      ((ppcre:scan "(?i)android" user-agent) t)
      ((ppcre:scan "(?i)iPad|iPhone|iPod" user-agent) t)
      ((ppcre:scan "(?i)windows phone" user-agent) t)
      ((ppcre:scan "(?i)blackberry|bb10|playbook" user-agent) t)
      (t nil))))

(defun footer (request cookie)
  "footer for all pages except accounts"
  (let* ((user-tokens (when cookie (get-user-tokens nil cookie)))
	 (tokens (if user-tokens user-tokens 0))
	 (has-free-trial (when cookie (has-free-trial-p cookie)))
	 (has-free-trial-or-tokens (or has-free-trial (> tokens 0)))
	 )
    (with-html-output (*standard-output*)
      (if (is-mobile-browser request)
	  (htm
	   (:script (str (beacon-js)))
	   (:script (str (duration-js)))
	   (:footer
	    (if cookie
		(htm (:a :href "/home" "Create deck") :br)
		(htm (:a :href "/accounts" "Signin/Signup") :br))
	    (when cookie
	      (htm
	       (:a :href "/logout" "Logout") (:span :class "separator" " |")
	       (:a :href "/user-decks" (str (format nil "Saved decks(~a)" (count-user-documents cookie)))))
	      (htm :br)
	      (if (get-consent (get-email-from-cookie cookie))
		  (htm (:a :href "/revoke-consent" "Don't allow cookies"))
		  (htm (:a :href "/consent" "Allow cookies"))))
	    :br
	    (when cookie
	      (htm (:span :id "balance-holder" :class (cond
							(has-free-trial-or-tokens "balance")
							((null has-free-trial-or-tokens) "error-p")
							(t  "balance"))
			  "Your balance: " (:span :id "balance-span" (str (if has-free-trial
									      "1 free trial"
									      (format nil "~a tokens" tokens))))) :br))
	    (:a :href "https://www.paypal.com/ncp/payment/H6QDGXUTHGCWJ"
		(:span :class
		       (if cookie
			   (cond
			     ((null has-free-trial-or-tokens) "buy-tokens-large")
			     (t "buy-tokens"))
			   "but-tokens")
		       "Buy tokens")) (:span :class "separator" " |")
		       (:a :href "/pricing" "Pricing")
	    :br
	    (:a :href "/terms-and-privacy" "Terms and Privacy")
	    :br
	    (:a :class
		;; set the size of this depending on whether the user has tokens or not.
		;; if not, the priority is to buy tokens and that should be bigger.
		(cond
		  (has-free-trial-or-tokens "feedback")
		  (t ""))
		:href "/feedback" "What can be improved?")
	    :br 
	    (:div :class "copyright" (str (format nil "© Ninxtech Ltd, ~a." (get-current-year))))))
	  (htm
	   (:script (str (beacon-js)))
	   (:script (str (duration-js)))
	   (:footer
	    (if cookie
		(htm (:a :href "/home" "Create deck") (:span :class "separator" " |"))
		(htm (:a :href "/accounts" "Signin/Signup") (:span :class "separator" " |")))
	    (when cookie
	      (htm
	       (:a :href "/user-decks" "Saved decks(" (:span :id "user-doc-count" (str (count-user-documents cookie))) ")")
	       (:span :class "separator" " |")
	       (:a :href "/logout" "Logout")
	       (:span :class "separator" " |")
	       (if (get-consent (get-email-from-cookie cookie))
		   (htm (:a :href "/revoke-consent" "Don't allow cookies"))
		   (htm (:a :href "/consent" "Allow cookies")))))
	    :br
	    (when cookie
	      (htm (:span :id "balance-holder" :class (cond
							(has-free-trial-or-tokens "balance")
							((or (<= tokens 0) (null has-free-trial)) "error-p")
							(t  "balance"))
			  "Your balance: " (:span :id "balance-span" (str (if has-free-trial
									      "1 free trial"
									      (format nil "~a tokens" tokens))))) (:span :class "separator" " |")))
	    (:a :href "https://www.paypal.com/ncp/payment/H6QDGXUTHGCWJ"
		(:span :class
		       (if cookie
			   (cond
			     ((null has-free-trial-or-tokens) "buy-tokens-large")
			     (t "buy-tokens"))
			   "but-tokens")
		       "Buy tokens")) (:span :class "separator" " |")
		       (:a :href "/pricing" "Pricing")
	    :br
	    (:a :href "/terms-and-privacy" "Terms and Privacy")
	    :br
	    (:a :class
		(cond
		  (has-free-trial-or-tokens "feedback")
		  (t ""))
		:href "/feedback" "What can be improved?")
	    (:div :class "copyright" (str (format nil "© Ninxtech Ltd, ~a." (get-current-year)))))))))  )

(defun pricing-css ()
  (let ((fg-color "#1a1a1a")
	(red "#FF6060")
	(blue "#CCCCFF")
	(bg-color "#e8e8e8")
	(link-blue "#1e90ff"))
    (cl-css:css
     `((body :text-align center :background-color ,bg-color :color ,fg-color)
       (h2 :color ,fg-color)
       (table :width 60% :margin "20px auto" :border-collapse collapse :color black)
       ("th, td" :border "1px solid #ccc" :padding "10px" :text-align left)
       (th :background-color ,bg-color :color ,fg-color)
       ("tr:nth-child(even)" :background-color ,bg-color :color ,fg-color)
       ("tr:nth-child(odd)" :background-color ,fg-color :color ,bg-color)
       (body :margin 20px :font-size 16px)
       ("@media only screen and (min-width: 768px)" (body :font-size 18px))
       (.error-p :color ,red)
       (p :text-align "justify")
       (footer :text-align center)
       (a :color ,link-blue :text-decoration none :decoration none :margin-left 10px)
       ("a:not(.logo-link)::after" :content "\"↪\"" :font-weight "bold" :color "inherit" :vertical-align baseline)
       (.logo :display flex :justify-content center :flex-direction :row :align-items :center :gap "10px")
       ("a.logo-link" :text-align center :color ,link-blue :text-decoration none :font-size 30px)
       (.logo-image :width 50px)
       ("a:hover::after" :color ,link-blue)
       ("a:visited" :color ,link-blue :decoration none)
       (.separator :color ,link-blue)
       ("span.balance" :color ,fg-color)
       ("a.feedback" :font-size 18px :font-weight bold)
       ("a:hover" :color ,link-blue :decoration underline)
       (".copyright" :color ,fg-color :text-align center)
       (.buy-tokens-large :font-size 21px :font-weight bold) 
       (.pricing :text-align center :width 80% :margin-left 10%)
       (p :margin-bottom 15px)
       ("@media only screen and (max-width: 768px)"
	(footer :margin-top 50% :text-align left :font-size 15px)
	("a.feedback" :font-size 16px :font-weight bold)
	(a :margin-left 2px :margin-right 2px)
	(.pricing :width 95% :margin 0)
	(".copyright" :color ,fg-color :text-align left))))))

(define-easy-handler (pricing :uri "/pricing"
			      :acceptor-names '(ninx::ninx)
			      :host *decklm-host*) ()
  (incr-events "pricing")
  (save-ip-visit (remote-addr* *request*))
  (save-country-visit (remote-addr*))
  (let ((cookie (cookie-in "cookie")))
    (with-html-output-to-string (*standard-output*)
      "<!DOCTYPE html>"
      (:html :lang "en"
	     (:head
	      (:title "Pricing | DeckLM")
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
              (:link :rel "manifest" :href "/decklm/manifest.json")
	      (:meta :name "description" :content "The pricing page for DeckLM")
	      (:link :rel "icon" :href "/decklm/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/decklm/static/icons/web/apple-touch-icon.png")
	      (:style (str (pricing-css))))
	     (:body :id "parent"
		    (:a :href "/home" :class "logo-link"
			(:div :class "logo"
			      (:img :class "logo-image" :src "/decklm/icon-512.png" :alt "Logo Icon")
			      "DeckLM"))
		    (:h2 "Pricing")
		    (:div :class "pricing"
			  (:b (:u "Pricing and Payment"))
			  (:p "Create your first slide deck for free!")
			  (:p "Each token covers about 0.75 words. Tokens are used based on the number of words in your uploaded images/documents and in the slide deck generated. Tokens will be automatically deducted from your balance.")
			  (:p "Get 1 million tokens (enough for approximately 750,000 words) for just $25. Token bundles are available in $5, $25, $100, and $1000 options. Tokens do not expire, but get used up.")
			  (:p "We only accept payments via PayPal. Be sure to use the same email here as in your PayPal transaction.")
			  )
		    (:table
			(:thead
			 (:tr
			  (:th "Tier") (:th "Price") (:th "Tokens") (:th "Number of words")))
		      (:tbody
		       (:tr (:td "1") (:td "$5") (:td "250,000") (:td "~187,500"))
		       (:tr (:td "2") (:td "$25") (:td "1,000,000") (:td "~750,000"))
		       (:tr (:td "3") (:td "$100") (:td "4,000,000") (:td "~3,000,000"))
		       (:tr (:td "4") (:td "$1,000") (:td "40,000,000") (:td "~30,000,000"))))
		    (footer *request* cookie))))))

(defun tacs-css ()
  (let ((fg-color "#1a1a1a")
	(red "#FF6060")
	(blue "#CCCCFF")
	(bg-color "#e8e8e8")
	(link-blue "#1e90ff"))
    (declare (ignore red))
    (cl-css:css
     `((body :text-align center :background-color ,bg-color :color ,fg-color)
       (h2 :color ,fg-color)
       (body :margin 20px :font-size 16px)
       (.separator :color ,link-blue)
       ("@media only screen and (min-width: 768px)" (body :font-size 18px :width 80% :margin-left 10%))
       (p :text-align "justify")
       (.error-p :color ,red)
       (footer :text-align center :color)
       (a :color ,link-blue :text-decoration none :decoration none :margin-left 10px)
       ("a:not(.logo-link)::after" :content "\"↪\"" :font-weight "bold" :color "inherit" :vertical-align baseline)
       (.logo :display flex :justify-content center :flex-direction :row :align-items :center :gap "10px")
       ("a.logo-link" :text-align center :color ,link-blue :text-decoration none :font-size 30px)
       (.logo-image :width 50px)
       ("a:hover::after" :color ,link-blue)
       ("a:visited" :color ,link-blue :decoration none)
       (.buy-tokens-large :font-size 21px :font-weight bold)
       ("span.balance" :color ,fg-color)
       ("a:hover" :color ,link-blue :decoration underline)
       (".copyright" :color ,fg-color :text-align center)
       ("@media only screen and (max-width: 768px)"
	(footer :margin-top 10% :text-align left :font-size 15px)
	("a.feedback" :font-size 16px :font-weight bold)
	(a :margin-left 2px :margin-right 2px)
	(".copyright" :color ,fg-color :text-align left))))))

(define-easy-handler (terms-and-privacy :uri "/terms-and-privacy"
					:acceptor-names '(ninx::ninx)
					:host *decklm-host*) ()
  (incr-events "terms-and-privacy")
  (save-ip-visit (remote-addr* *request*))
  (save-country-visit (remote-addr*))
  (let ((cookie (cookie-in "cookie")))
    (with-html-output-to-string (*standard-output*)
      "<!DOCTYPE html>"
      (:html :lang "en"
	     (:head
	      (:title "TACS | DeckLM")
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:meta :name "description" :content "The terms and conditions page for DeckLM")
              (:link :rel "manifest" :href "/decklm/manifest.json")
	      (:link :rel "icon" :href "/decklm/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/decklm/static/icons/web/apple-touch-icon.png")
	      (:style (str (tacs-css))))
	     (:body :id "parent"
		    (:a :href "/home" :class "logo-link"
			(:div :class "logo"
			      (:img :class "logo-image" :src "/decklm/icon-512.png" :alt "Logo Icon")
			      "DeckLM"))
		    (:h2 "Terms, Conditons and Privacy")
		    (:p :id "privacy-div" :class "privacy-div"
			"We use one cookie to keep you logged in. We require consent to store this cookie and remember you on your device, otherwise, you will have to login every time."
			:br :br
			"Consent is asked for once and stored on our servers. You can later revoke this consent by clicking on 'Don't save cookie' on the home page. We don't use third party cookies." 
			:br :br
			"All images and documents submitted are only stored during making of the slide deck, after which they are deleted. The decks generated are stored on our servers for you to access later on."
			:br :br
			"We only accept payment via Paypal and do not store any credit card details on our servers. Use the same email you used here when paying."
			:br :br)
		    (footer *request* cookie)
		    )))))

(defun home-js ()
  "the js for the home page"
  (ps:ps
    (defun is-mobile-browser ()
      "check if the browser is mobile"
      (let ((user-agent (or (ps:chain navigator user-agent) (ps:chain navigator vendor) (ps:chain window opera))))
	(cond
	  ((ps:chain (regex "/android/i") (test user-agent)) t)
	  ((ps:chain (regex "/iPad|iPhone|iPod/") (test user-agent)) t)
	  ((ps:chain (regex "/windows phone/i") (test user-agent)) t)
	  ((ps:chain (regex "/blackberry|bb10|playbook/i") (test user-agent)) t)
	  (t false))))

    (defun incr-user-doc-count ()
      (let* ((user-doc-count (ps:chain document (get-element-by-id "user-doc-count")))
	     (count (parse-int (ps:chain user-doc-count inner-h-t-m-l))))
	(setf (ps:chain user-doc-count inner-h-t-m-l) (+ 1 count))))

    (defun change-user-balance (tokens)
      ;; if the user balance is 0 or less, disable the forms.
      (ps:chain console (log tokens))
      (let ((balance-span (ps:chain document (get-element-by-id "balance-span"))))
	(setf (ps:chain balance-span inner-h-t-m-l) (+ tokens " tokens"))
	(unless (> (parse-int tokens) 0)
	  (setf (ps:chain document (get-element-by-id "file-input") disabled) t)
	  (ps:chain document (get-element-by-id "balance-holder") class-list (replace "balance" "error-p"))
	  (setf (ps:chain document (get-element-by-id "no-balance-p") style display) "block")
	  (setf (ps:chain document (get-element-by-id "description") disabled) t))))
    
    ;; Only show the drop zone if it's not a mobile browser.
    (ps:chain document (add-event-listener "DOMContentLoaded" (lambda ()
								(let ((drop-zone (ps:chain document (get-element-by-id "drop-zone")))
								      (style (if (is-mobile-browser) "none" "block")))
								  (setf (ps:chain drop-zone style display) style)))))

    (defun generate-random-id ()
      "generate a random id from date"
      (+ "id" (ps:chain -date (now)) "-" (ps:chain -math (random) (to-string 36) (substr 2 9))))

    (setf files-array (array)) ;; this will hold the files
    
    (defun handle-files (files)
      "this function will handle addition of files when a user submits or drops them"
      (let ((files-container (ps:chain document (get-element-by-id "files-container"))))
	(ps:chain -array (from files)
		  (for-each
		   (lambda (file)
		     ;; first add the file to form-data created above
		     (ps:chain files-array (push file))
		     (let ((file-frame (ps:chain document (create-element "div")))
			   (close-button (ps:chain document (create-element "button")))
			   (frame-id (generate-random-id))
			   (btn-id (generate-random-id)))
		       ;; handle frame
		       (ps:chain file-frame class-list (add "file-frame"))
		       (ps:chain file-frame (set-attribute "id" frame-id))
		       (ps:chain file-frame (set-attribute "draggable" "true")) ;; Allow frames to be draggable

		       ;; handle close button
		       (ps:chain close-button class-list (add "close-btn"))
		       (ps:chain close-button (set-attribute "id" btn-id))
		       (setf (ps:chain close-button inner-h-t-m-l) "&times;")
		       (ps:chain close-button (add-event-listener "click"
								  (lambda ()
								    ;; remove the file from files-array
								    (let ((index (ps:chain files-array (index-of file))))
								      (if (> index -1)
									  (ps:chain files-array (splice index 1))))
								    (ps:chain file-frame (remove))))))
 		     (cond
		       ((ps:chain file type (starts-with "image/"))
			(let ((img (ps:chain document (create-element "img")))
			      (reader (new -file-reader)))
			  (setf (ps:chain reader onload) (lambda (e) (setf (ps:chain img src) (ps:chain e target result))))
			  (ps:chain reader (read-as-data-u-r-l file))
			  (ps:chain file-frame (append-child img))))
		       ((eql (ps:chain file type) "application/pdf")
			(let ((iframe (ps:chain document (create-element "iframe")))
			      (file-name (ps:chain document (create-element "span"))))
			  (setf (ps:chain iframe src) (ps:chain -u-r-l (create-object-u-r-l file)))
			  (ps:chain file-frame (append-child iframe))
			  ;; file name
			  (ps:chain file-name class-list (add "file-name"))
			  (setf (ps:chain file-name text-content) (ps:chain file name (to-lower-case)))
			  (ps:chain file-frame (append-child file-name)))))
		     (ps:chain file-frame (append-child close-button))
		     (ps:chain files-container (append-child file-frame)))))))

    ;; Enable drag-and-drop file rearranging within files-container
    (setf files-container (ps:chain document (get-element-by-id "files-container")))
    (setf dragged-item nil)

    (ps:chain files-container (add-event-listener "dragstart"
						  (lambda (event)
						    (setf dragged-item (ps:chain event target))
						    ;; Add visual feedback when dragging starts
						    (set-timeout (lambda () (setf (ps:chain dragged-item style opacity) "0.5")) 0))))

    (ps:chain files-container (add-event-listener "dragend"
						  (lambda ()
						    ;; Reset opacity and clear dragged item after dropping
						    (set-timeout (lambda ()
								   (setf (ps:chain dragged-item style opacity) "1")
								   (setf dragged-item nil)) 0))))

    (ps:chain files-container (add-event-listener "dragover"
						  (lambda (event)
						    ;; Allow dropping by preventing the default behavior
						    (ps:chain event (prevent-default)))))

    (ps:chain files-container (add-event-listener "drop"
						  (lambda (event)
						    (ps:chain event (prevent-default)) ;; Prevent default drop behavior
						    (let ((target (ps:chain event target)))
						      (when (and dragged-item (ps:chain target class-list (contains "file-frame")))
							(let ((rect (ps:chain target get-bounding-client-rect))
							      (offset (ps:chain event client-y)))
							  ;; Determine whether to insert before or after the target based on the Y position
							  (if (< offset (+ (ps:chain rect top) (/ (ps:chain rect height) 2)))
							      (ps:chain files-container (insert-before dragged-item target))
							      (let ((next-sibling (ps:chain target next-sibling)))
								(if next-sibling
								    (ps:chain files-container (insert-before dragged-item next-sibling))
								    (ps:chain files-container (append-child dragged-item)))))))))))

    ;; Add files via "Add a file" button click
    (ps:chain document (get-element-by-id "upload-btn")
	      (add-event-listener "click"
				  (lambda ()
				    (setf (ps:chain document (get-element-by-id "success-indicator") style display) "none")
				    (let ((file-input (ps:chain document (get-element-by-id "file-input"))))
				      (ps:chain file-input (click))
				      (setf (ps:chain file-input onchange)
					    (lambda ()
					      (handle-files (ps:chain file-input files))
					      (setf (ps:chain file-input value) "")))))))

    ;; Enable file drag-and-drop in the drop zone
    (setf drop-zone (ps:chain document (get-element-by-id "drop-zone")))

    (ps:chain drop-zone (add-event-listener "dragenter" (lambda (event)
							  (ps:chain event (prevent-default))
							  (ps:chain drop-zone class-list (add "dragging")))))

    (ps:chain drop-zone (add-event-listener "dragover" (lambda (event)
							 (ps:chain event (prevent-default)))))

    (ps:chain drop-zone (add-event-listener "dragleave" (lambda (event)
							  (ps:chain event (prevent-default))
							  (ps:chain drop-zone class-list (remove "dragging")))))

    (ps:chain drop-zone (add-event-listener "drop"
					    (lambda (event)
					      (ps:chain event (prevent-default))
					      (ps:chain drop-zone class-list (remove "dragging"))
		 			      (handle-files (ps:chain event data-transfer files)))))

    (defun show-toast (message duration)
      "show a toast to the user"
      (let ((toast-container (ps:chain document (get-element-by-id "toast-container")))
	    (toast (ps:chain document (create-element "div"))))
	(ps:chain toast class-list (add "toast"))
	(setf (ps:chain toast text-content) message)
	(ps:chain toast-container (append-child toast))
	(set-timeout (lambda () "remove the toast"
		       (ps:chain toast class-list (add "fade-out"))
		       (set-timeout (lambda () "wait fo the fade-out to complete"
				      (ps:chain toast (remove))) 500))
		     duration)))

    
    ;; Upload files to the "/files" endpoint using plain JS
    (let ((submit-btn (ps:chain document (get-element-by-id "submit-btn")))
	  (progress-container (ps:chain document (get-element-by-id "progress-container")))
	  (progress-bar (ps:chain document (get-element-by-id "upload-progress")))
	  (loading-indicator (ps:chain document (get-element-by-id "loading-indicator")))
	  (error-container (ps:chain document (get-element-by-id "error-container")))
	  (error-indicator (ps:chain document (get-element-by-id "error-indicator")))
	  (success-indicator (ps:chain document (get-element-by-id "success-indicator")))
	  (loading-container (ps:chain document (get-element-by-id "loading-container")))
	  )
      (defun base64-to-array (base64data)
	"This function processes base64data returned from the server and converts it into a Uint8Array."
	(let* ((binary-string (ps:chain window (atob base64data)))
               (len (ps:chain binary-string length))
               (bytes (new (-uint8-array len)))
               (i 0))
	  (loop for i from 0 to len do
	    (setf (aref bytes i) (ps:chain binary-string (char-code-at i))))
	  bytes))

      (defun download-file (blob name)
	"This function triggers the download of the file returned by the server. Shows a toast with the filename,
   hides the progress indicator, and resets forms and file array."
	(let ((blob-data (new (-blob (array (base64-to-array blob)) 
				     (create :type "application/vnd.openxmlformats-officedocument.presentationml.presentation"))))
              (link (ps:chain document (create-element "a")))
              (filename (+ name ".pptx")))
	  
	  (setf (ps:chain link href) (ps:chain window -u-r-l (create-object-u-r-l blob-data)))
	  (setf (ps:chain link download) filename)
	  (ps:chain document body (append-child link))
	  (ps:chain link (click))
	  (ps:chain document body (remove-child link))
	  (setf (ps:chain progress-container style display) "none")
	  (setf (ps:chain loading-indicator style display) "none")
	  (setf (ps:chain loading-container style display) "none")
	  (let ((txt (+ "Your deck is saved as " filename " in your downloads folder as " name ".")))
	    (show-toast txt 500)
	    (setf (ps:chain success-indicator inner-h-t-m-l) txt))
	  
	  (setf (ps:chain success-indicator style display) "block")
	  (setf files-array (array))
	  (setf (ps:chain document (get-element-by-id "description") value) "")
	  (setf (ps:chain document (get-element-by-id "files-container") inner-h-t-m-l) ""))
	)


      (defun upload-files ()
	"upload the files and track progress of the upload"
	(ps:chain console (log files-array))
	(let ((form-data (new -form-data))
	      (xhr (new -x-m-l-http-request))
	      (description-value (ps:chain document (get-element-by-id "description") value)) ;; get the description value to be sent to the server
	      )
	  ;; Add files to FormData
	  (when (> (ps:chain files-array length) 0)
	    (ps:chain form-data (set "number-of-files" (ps:chain files-array length)))
	    (loop for i from 0 below (ps:chain files-array length) do
	      (ps:chain form-data (append (+ "file_" i) (aref files-array i)))))
	  
	  (when description-value
	    (ps:chain form-data (append "description" description-value)))
	  ;; Open the request
	  (ps:chain xhr (open "POST" "/files" t))
	  ;; Track progress
	  (setf (ps:chain xhr upload onprogress)
		(lambda (event)
		  (when (ps:chain event length-computable)
		    (setf (ps:chain progress-bar value)
			  (/ (* 100 (ps:chain event loaded)) (ps:chain event total)))
		    (when (eql (ps:chain event loaded) (ps:chain event total))
		      (setf (ps:chain progress-container style display) "none")
		      (setf (ps:chain loading-indicator style display) "block")
		      (setf (ps:chain loading-container style display) "block")
		      (setf (ps:chain loading-indicator inner-h-t-m-l) "Generating deck, sit back and wait...")))))

	  (setf (ps:chain xhr onloadstart) (lambda ()
					     "Show loading indicator when uploading starts"
					     (setf (ps:chain loading-container style display) "none")
					     (setf (ps:chain success-indicator style display) "none")
					     (setf (ps:chain progress-container style display) "block")
					     (setf (ps:chain loading-indicator style display) "block")
					     (setf (ps:chain error-container style display) "none")
					     (setf (ps:chain error-indicator style display) "none")
					     ))

	  (setf (ps:chain xhr onload)
		(lambda ()
		  "handle upload completion and process response"
		  (if (and (eql (ps:chain xhr ready-state) 4) (eql (ps:chain xhr status) 200))
		      (let* ((response-json (ps:chain xhr response-text))
			     (response (ps:chain -j-s-o-n (parse response-json))))
			(ps:chain console (log response))
			(setf (ps:chain progress-container style display) "none")
			(setf (ps:chain loading-indicator style display) "none")
			(if (eql (ps:chain response success) t)
			    (setf (ps:chain window location href) (+ "/deck/" (ps:chain response docid) "/" (ps:chain response title)))
			    ;; (progn (download-file (ps:chain response data) (ps:chain response title))
			    ;; 	   (incr-user-doc-count)
			    ;; 	   (change-user-balance (ps:chain response balance)))
			    (progn
			      (ps:chain console (log response))
			      (show-toast "An error occured during making the deck, please try again" 3000)
			      (let* ((error-code (ps:chain response error-code))
				     (error-text (cond
						   ((eql error-code 429) "Server currently busy, please try again later.")
						   ((eql error-code 500) "Your documents are too long, they will take forever to process. Decrease the number and/or size of the documents and try again.")
						   ((eql error-code 503) "Server currently experiencing some technical problems. Try again later.")
						   ((eql error-code 504) "The documents are taking very long to process, won't finish in time. Decrease number and/or size of the documents and try again.")
						   (t "An error occurred while processing the request. Please try again.")))))
			      (setf (ps:chain progress-container style display) "none")
			      (setf (ps:chain loading-indicator style display) "none")
			      (setf (ps:chain loading-container style display) "none")
			      (setf (ps:chain error-indicator inner-h-t-m-l) error-text)
			      (setf (ps:chain error-container style display) "block")
			      (setf (ps:chain error-indicator style display) "block")
			      )))
		      (progn (show-toast "An error occured during making the deck, please try again" 3000)
			     (setf (ps:chain loading-container style display) "none")
			     (setf (ps:chain progress-container style display) "none")
			     (setf (ps:chain loading-indicator style display) "none")
			     (setf (ps:chain error-container style display) "block")
			     (setf (ps:chain error-indicator style display) "block")
			     ))))

	  ;; Send the request only if there's a description or files to send
	  (when (or (> (length files-array) 0)
		    description-value)
	    (ps:chain xhr (send form-data))
	    (show-toast "Data is being submitted" 1500))))

      (ps:chain submit-btn (add-event-listener "click" (lambda ()
							 "submit button click listener"
							 (upload-files))))

      (let ((textarea (ps:chain document (get-element-by-id "description"))))
	(ps:chain textarea (add-event-listener "input" (lambda ()
							 "auto resize the textarea vertically as the user types in"
							 (setf (ps:chain this style height) "auto")
							 (setf (ps:chain this style height) (+ (ps:chain this scroll-height) "px"))))))
      )))


(defun home-css ()
  "the css for the /home endpoint"
  (let ((fg-color "#1a1a1a")
	(red "#FF6060")
	(blue "#CCCCFF")
	(bg-color "#e8e8e8")
	(link-blue "#1e90ff"))
    (cl-css:css
     `((body :text-align center :background-color ,bg-color :color ,fg-color :margin 20px :font-size 16px)
       (button :background-color "#00b800" :border "none" :color "#cccccc" :padding "10px 20px" :text-align "center" :text-decoration "none" :display "inline-block" :font-size "16px" :margin "4px 2px" :cursor "pointer" :border-radius "12px" :font-weight bold)
       (.submit-btn :float right :background-color "#00b800")
       (.upload-btn :float left)
       (.add-symbol :margin-right 8px :font-size :16px :font-weight bold)
       ("button:hover" :background-color "#45a049")
       (.file-frame :display inline-block :margin 10px :padding 10px :border "1px solid #1a1a1a" :position relative :text-align center :max-width "150px")
       (.file-name :font-size "12px" :color ,fg-color :margin-top "5px" :word-wrap "normal" :max-width "150px" :overflow "hidden" :text-overflow "ellipsis")
       (.close-btn :position absolute :top 5px :right 5px :background-color red :color white :border none :cursor pointer)
       ("img, iframe" :max-width 150px :max-height 150px)
       (.drop-zone :width "100%" :height "200px" :border "2px dashed #1a1a1a" :display "flex" :align-items "center" :justify-content "center" :text-align "center" :margin "20px 0" :cursor "pointer")
       (.drop-zone.dragging :border-color "#666")
       (h2 :color ,fg-color)
       (p :text-align "justify")
       (.separator :color ,link-blue)
       (.error-p :color ,red)
       (.top :font-weight bold)
       (.buy-tokens-large :font-size 21px :font-weight bold)
       (footer :text-align center :margin-top 10%)
       (a :color ,link-blue :text-decoration none :decoration none :margin-left 10px :margin-right 10px)
       ("a:hover::after" :color ,link-blue)
       ("a:visited" :color ,link-blue :decoration none)
       ("a:hover" :color ,link-blue :decoration underline)
       ("a:not(.logo-link)::after" :content "\"↪\"" :font-weight "bold" :color "inherit" :vertical-align baseline)
       (.logo :display flex :justify-content center :flex-direction :row :align-items :center :gap "10px")
       ("a.logo-link" :text-align center :color ,link-blue :text-decoration none :font-size 30px)
       (.logo-image :width 50px)
       ("span.balance" :color ,fg-color)
       ("a.feedback" :font-size 20px :font-weight bold)
       ("a.logo-link:hover" :color ,link-blue)
       (".copyright" :color ,fg-color :text-align center)
       (.toast-container :position "fixed" :bottom "20px" :left "50%" :transform "translateX(-50%)" :z-index "9999" :display "flex" :flex-direction "column" :align-items "center")
       (.toast :background-color "#333" :color "white" :padding "15px" :margin-top "10px" :border-radius "5px" :box-shadow "0px 4px 8px rgba(0, 0, 0, 0.1)" :opacity "0.9" :transition "opacity 0.3s ease" :max-width "80%" :word-wrap "break-word" :text-align "center")
       (".toast.fade-out" :opacity "0")
       (textarea :width 99.5% :margin-top 5px :border "1px solid #1a1a1a" :background-color ,bg-color :color ,fg-color :padding 5px)

       (.loading-container :width "150px" :height "10px" :background-color "#e0e0e0" :overflow "hidden" :position "relative" :border-radius "5px" :border 0 :margin "0 auto")
       (.bar :width "30px" :height "100%" :background-color "#00b800" :position "absolute" :left "-30px" :animation "move 2s cubic-bezier(0.42, 0, 0.58, 1) infinite")
       (".bar:nth-child(2)" :animation-delay "1s")
       ("@keyframes move" ("0%" :left "-30px") ("50%" :left "100%") ("51%" :left "-30px") ("100%" :left "100%"))
       
       ;; (.loading-container :width "150px" :height "10px" :background-color "#e0e0e0" :overflow "hidden" :position "relative" :border-radius "5px" :border 0 :margin "0 auto")
       ;; (.bar :width "30px" :height "100%" :background-color "#00b800" :position "absolute" :left "-30px" :animation "move 2s linear infinite")
       ;; (".bar:nth-child(2)" :animation-delay "1s")
       ;; ("@keyframes move" ("0%" :left "-30px") ("50%" :left "100%") ("51%" :left "-30px") ("100%" :left "100%"))
       
       (".description:focus" :border none :color ,fg-color)
       (.description-title :margin-top 12px)
       ("@media only screen and (min-width: 768px)" (body :font-size 18px :width 80% :margin-left 10%))
       ("@media only screen and (max-width: 768px)"
	(footer :margin-top 50% :text-align left :font-size 15px)
	("a.feedback" :font-size 16px :font-weight bold)
	(a :margin-left 2px :margin-right 2px)
	(".copyright" :color ,fg-color :text-align left))))))

(define-easy-handler (home :uri "/home"
			   :acceptor-names '(ninx::ninx)
			   :host *decklm-host*) ()
  ;; you first check the cookiw against those stored in the database.
  ;; if you match it, then load the page, else delete the cookie.
  (incr-events "home")
  (save-ip-visit (remote-addr* *request*))
  (save-country-visit (remote-addr*))
  (let ((cookie (cookie-in "cookie")))
    (if cookie
	(if (get-email-from-cookie cookie)
	    (let* ((saved-tokens (get-user-tokens nil cookie))
		   (tokens (if saved-tokens saved-tokens 0))
		   (has-access (or (> tokens 0) (has-free-trial-p cookie))) ;;checks if a user has free trial or has tokens
		   (has-free-trial (when cookie (has-free-trial-p cookie)))
		   (has-free-trial-or-tokens (or has-free-trial (> tokens 0)))
		   )
	      
	      (with-html-output-to-string (*standard-output*)
		"<!DOCTYPE html>"
		(:html :lang "en"
		       (:head
			(:title "Create deck | DeckLM")
			(:meta :charset "UTF-8")
			(:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
			(:meta :name "description" :content "The home page for DeckLM")
			(:link :rel "manifest" :href "/decklm/manifest.json")
			(:link :rel "icon" :href "/decklm/static/icons/web/favicon.ico" :sizes "any")
			(:link :rel "apple-touch-icon" :href "/decklm/static/icons/web/apple-touch-icon.png")
			(:style (str (home-css))))
		       (:body :id "parent"
			      (:a :href "/home" :class "logo-link"
				  (:div :class "logo"
					(:img :class "logo-image" :src "/decklm/icon-512.png" :alt "Logo Icon")
					"DeckLM"))
			      (when cookie
				(htm (:span :id "balance-holder" :class (cond
									  (has-free-trial-or-tokens "balance top")
									  ((null has-free-trial-or-tokens) "error-p top")
									  (t  "balance top"))
					    "Your balance: " (:span :id "balance-span" (str (if has-free-trial
												"1 free trial"
												(format nil "~a tokens" tokens))))) :br))
			      (:p :id "no-balance-p" :style (if has-access "display:none" "display:block") :class "error-p"
				  "You don't have any tokens, please buy tokens and try again."
				  (:a :href "https://www.paypal.com/ncp/payment/H6QDGXUTHGCWJ" " Buy tokens"))
			      (:p "To start, select images and/or PDFs from which to make the deck. You can add more files with the '+ Add' button.")
			      (:p :class "description-title" "Optional: Provide a description of what you want in the slide deck.")
			      (:textarea :class "description" :placeholder "Description goes here: include areas you want emphasized and if possible, an outline of things you want included." :id "description"
					 :disabled (if has-access nil t))
			      (:div :id "drop-zone" :class "drop-zone" " Drag and drop files here or click the Add button")
			      (:input :type "file" :id "file-input" :style "display: none;" :multiple t :allow "application/pdf, application/x-javascript, text/javascript, application/x-python, text/x-python, text/plain, text/html, text/css, text/md, text/csv, text/xml, text/rtf" 
				      :disabled (if has-access nil t))
			      (:div :id "files-container")
			      (:div :class "btns"
				    (:button :class "upload-btn" :id "upload-btn"
					     (:span :class "add-symbol" "+")
					     "Add image/PDF")
				    (:button :class "submit-btn" :id "submit-btn" "Create Deck"))
			      (:div :id "loading-container" :class "loading-container" :style "display: none;"
				    (:div :class "bar")
				    (:div :class "bar")
				    (:div :class "bar"))
			      (:div :id "progress-container" :style "display: none;"
				    (:progress :id "upload-progress" :value "0" :max "100"))
			      (:div :id "loading-indicator" :style "display: none;"
				    "Submitting... Please wait.")
			      (:div :id "error-container" :style "display: none;"
				    (:progress :id "error-progress" :value "100" :style "color: #FF6060"))
			      (:div :id "error-indicator" :style "display: none; color: #FF6060"
				    "An error occurred, please try again.")
			      (:div :id "success-indicator" :style "display: none; color: #1e90ff"
				    "The deck has been created, downloaded and saved in downloads.")
			      (:div :id "toast-container" :class "toast-container")
			      (footer *request* cookie))
		       (:script (str (home-js))))))
	    (progn
	      (set-cookie "cookie" :expires (- (get-universal-time) 10000))
	      (hunchentoot:redirect "/")))
	(hunchentoot:redirect "/"))))


(define-easy-handler (files :uri "/files" :host *decklm-host*
			    :acceptor-names '(ninx::ninx)
			    :default-request-type :post) ()
  ;; when we are done, we send the user the file for download and then redirect the user back to the home page.
  ;; if the user has a free trial and the generation is successful, consume it.
  (incr-events "files")
  (save-ip-visit (remote-addr* *request*))
  (save-country-visit (remote-addr*))
  (let ((cookie (cookie-in "cookie")))
    (if cookie
	(let* ((has-free-trial (has-free-trial-p cookie))
	       )
	  (jzon:stringify
	   (if (or (has-tokens-p nil cookie) has-free-trial)
	       (progn
		 (let* ((number-of-files-str (post-parameter "number-of-files"))
			(number-of-files (if number-of-files-str (parse-integer number-of-files-str) 0))
			(files (if (> number-of-files 0)
				   (loop for i from 0 below number-of-files
					 collect (post-parameter (format nil "file_~a" i)))))
	      		(description (post-parameter "description"))
			(slide-data (make-slides cookie description files)))
		   (when has-free-trial (save-to-free-trial cookie))
		   (save-country-deck-creations (remote-addr* *request*))
		   slide-data))
	       (hash-create '(("error" "no tokens"))))))
	(hunchentoot:redirect "/"))))

(define-easy-handler (logout :uri "/logout"
			     :acceptor-names '(ninx::ninx)
			     :host *decklm-host*) ()
  (incr-events "logout")
  (save-ip-visit (remote-addr* *request*))
  (save-country-visit (remote-addr*))
  (let ((cookie (cookie-in "cookie")))
    (when cookie
      (delete-cookie cookie))
    (set-cookie "cookie" :expires (- (get-universal-time) 10000))
    (hunchentoot:redirect "/")))

(define-easy-handler (consent :uri "/consent"		      :acceptor-names '(ninx::ninx)
			      :host *decklm-host*) ()
  (incr-events "consent")
  (save-ip-visit (remote-addr* *request*))
  (save-country-visit (remote-addr*))
  (let ((cookie (cookie-in "cookie")))
    (if cookie
	(progn (save-consent (get-email-from-cookie cookie) t)
	       (hunchentoot:redirect "/home"))
	(hunchentoot:redirect "/"))))

(define-easy-handler (manage-cookie-consent :uri "/revoke-consent"
					    :acceptor-names '(ninx::ninx)
					    :host *decklm-host*) ()
  (incr-events "revoke-consent")
  (save-ip-visit (remote-addr* *request*))
  (save-country-visit (remote-addr*))
  (let ((cookie (cookie-in "cookie")))
    (if cookie
	(progn (revoke-consent (get-email-from-cookie cookie))
	       (hunchentoot:redirect "/home"))
	(hunchentoot:redirect "/"))))

(defun user-decks-css ()
  "the css for the /home endpoint"
  (let ((fg-color "#1a1a1a")
	(red "#FF6060")
	(blue "#CCCCFF")
	(bg-color "#e8e8e8")
	(link-blue "#1e90ff"))
    (cl-css:css
     `((body :text-align center :background-color ,bg-color :color ,fg-color :margin 20px :font-size 16px)
       (h2 :color ,fg-color)
       (p :text-align "justify")
       (button :background-color "#00b800" :border "none" :color ,bg-color :padding "10px 20px" :text-align "center" :text-decoration "none" :display "inline-block" :font-size "16px" :margin "4px 2px" :cursor "pointer" :border-radius "12px" :font-weight bold)
       (.error-p :color ,red)
       (footer :text-align center :margin-top 10%)
       (a :color ,link-blue :text-decoration none :decoration none :margin-left 10px :margin-right 10px)
       ("a:not(.share-link):hover::after" :color ,link-blue)
       ("a:visited" :color ,link-blue :decoration none)
       ("a:hover" :color ,link-blue :decoration underline)
       ("span.balance" :color ,fg-color)
       (.separator :color ,link-blue)
       ("a.feedback" :font-size 20px :font-weight bold)
       ("a.logo-link:hover" :color "#45a049")
       ("a:not(.logo-link, .share-link)::after" :content "\"↪\"" :font-weight "bold" :color "inherit" :vertical-align baseline)
       (.logo :display flex :justify-content center :flex-direction :row :align-items :center :gap "10px")
       ("a.logo-link" :text-align center :color ,link-blue :text-decoration none :font-size 30px)
       (.logo-image :width 50px)
       
       (".copyright" :color ,fg-color :text-align center)
       (.toast-container :position "fixed" :bottom "20px" :left "50%" :transform "translateX(-50%)" :z-index "9999" :display "flex" :flex-direction "column" :align-items "center")
       (.toast :background-color "#333" :color "white" :padding "15px" :margin-top "10px" :border-radius "5px" :box-shadow "0px 4px 8px rgba(0, 0, 0, 0.1)" :opacity "0.9" :transition "opacity 0.3s ease" :max-width "80%" :word-wrap "break-word" :text-align "center")
       (".toast.fade-out" :opacity "0")
       (.buy-tokens-large :font-size 21px :font-weight bold)
       (.doc-div :text-align center)
       ("@media only screen and (min-width: 768px)" (body :font-size 18px :width 80% :margin-left 10%))
       (.wp :width 25px :margin-top 5px :margin-left 10px)
       (.wp-btn :margin-right 5px :margin-left 5px :border none :background-color ,bg-color)
       ("@media only screen and (max-width: 768px)"
	(footer :margin-top 50% :text-align left :font-size 15px)
	("a.feedback" :font-size 16px :font-weight bold)
	(a :margin-left 2px :margin-right 2px)
	(".copyright" :color ,fg-color :text-align left))
       ))))

(defun copy-to-clipboard ()
  "this function will copy to clip board"
  (ps:ps
    (defun copy-to-clipboard (text)
      (ps:chain navigator clipboard (write-text text))
      (show-toast "Link copied to clipboard." 1500))))

(define-easy-handler (user-decks :uri "/user-decks"
				 :acceptor-names '(ninx::ninx)
				 :host *decklm-host*) ()
  ;; this route lists the user's generated decks
  (incr-events "user-decks")
  (save-ip-visit (remote-addr* *request*))
  (save-country-visit (remote-addr*))
  (let ((cookie (cookie-in "cookie")))
    (if cookie
	(if (get-email-from-cookie cookie)
	    (with-html-output-to-string (*standard-output*)
	      "<!DOCTYPE html>"
	      (:html :lang "en"
		     (:head
		      (:title "Your Decks | DeckLM")
		      (:meta :charset "UTF-8")
		      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
		      (:link :rel "manifest" :href "/decklm/manifest.json")
		      (:meta :name "description" :content "The home page for DeckLM")
		      (:link :rel "icon" :href "/decklm/static/icons/web/favicon.ico" :sizes "any")
		      (:link :rel "apple-touch-icon" :href "/decklm/static/icons/web/apple-touch-icon.png")
		      (:style (str (user-decks-css))))
		     (:body :id "parent"
			    (:a :href "/home" :class "logo-link"
				(:div :class "logo"
				      (:img :class "logo-image" :src "/decklm/icon-512.png" :alt "Logo Icon")
				      "DeckLM"))
			    (:h4 "Your decks: Click on one to proceed.")
			    (mapcar (lambda (doc)
				      (let* ((title (cadr doc))
					     (name (str:downcase title))
					     
					     )
					(htm (:p :class "doc-div" (:a 
								   :href (format nil "/deck/~a/~a"
										 (car doc)
										 name)
								   (str title))
						 (str (format nil " - added on ~a" (extract-timestamp
										    (format nil "~a" (universal-to-timestamp
												      (caddr doc)))))))
					     
					     
					     )))
				    (reverse (get-user-documents cookie)))
			    (:div :id "toast-container" :class "toast-container")
			    (footer *request* cookie))
		     (:script (str (home-js)))
		     (:script
		      :async t
		      :src "https://platform.twitter.com/widgets.js"
		      :charset "utf-8")
		     (:script (str (copy-to-clipboard)))
		     ))
	    (progn  (set-cookie "cookie" :expires (- (get-universal-time) 10000))
		    (hunchentoot:redirect "/")))
	(hunchentoot:redirect "/"))))

(defun extract-timestamp (timestamp)
  "Extracts the date and time in YYYY-MM-DD,HH:MM:SS format from a given TIMESTAMP."
  (let ((date-part (subseq timestamp 0 10))  ; Extract YYYY-MM-DD
        (time-part (subseq timestamp 11 19))) ; Extract HH:MM:SS
    (format nil "~A at ~A" date-part time-part))) ; Combine and format the result

(defun feedback-js ()
  "the js for the home page"
  (ps:ps
    (defun is-mobile-browser ()
      "check if the browser is mobile"
      (let ((user-agent (or (ps:chain navigator user-agent) (ps:chain navigator vendor) (ps:chain window opera))))
	(cond
	  ((ps:chain (regex "/android/i") (test user-agent)) t)
	  ((ps:chain (regex "/iPad|iPhone|iPod/") (test user-agent)) t)
	  ((ps:chain (regex "/windows phone/i") (test user-agent)) t)
	  ((ps:chain (regex "/blackberry|bb10|playbook/i") (test user-agent)) t)
	  (t false))))
    (defun show-toast (message duration)
      "show a toast to the user"
      (let ((toast-container (ps:chain document (get-element-by-id "toast-container")))
	    (toast (ps:chain document (create-element "div"))))
	(ps:chain toast class-list (add "toast"))
	(setf (ps:chain toast text-content) message)
	(ps:chain toast-container (append-child toast))
	(set-timeout (lambda () "remove the toast"
		       (ps:chain toast class-list (add "fade-out"))
		       (set-timeout (lambda () "wait fo the fade-out to complete"
				      (ps:chain toast (remove))) 500))
		     duration)))
    ;; Upload files to the "/files" endpoint using plain JS
    (let ((submit-btn (ps:chain document (get-element-by-id "submit-btn")))
	  (progress-container (ps:chain document (get-element-by-id "progress-container")))
	  (progress-bar (ps:chain document (get-element-by-id "upload-progress")))
	  (loading-indicator (ps:chain document (get-element-by-id "loading-indicator")))
	  )

      (defun upload-feedback ()
	"upload the files and track progress of the upload"
	(let ((form-data (new -form-data))
	      (xhr (new -x-m-l-http-request))
	      (feedback-value (ps:chain document (get-element-by-id "feedback") value)) ;; get the feedback
	      )
	  ;; Add feedback to form-data
	  (when feedback-value
	    (ps:chain form-data (append "feedback" feedback-value)))
	  
	  ;; Open the request
	  (ps:chain xhr (open "POST" "/process-feedback" t))
	  
	  ;; Track progress
	  (setf (ps:chain xhr upload onprogress) (lambda (event)
						   (when (ps:chain event length-computable)
						     (setf (ps:chain progress-bar value)
							   (/ (* 100 (ps:chain event loaded)) (ps:chain event total))))))

	  (setf (ps:chain xhr onloadstart) (lambda ()
					     "Show loading indicator when uploading starts"
					     (setf (ps:chain progress-container style display) "block")
					     (setf (ps:chain loading-indicator style display) "block")
					     ))

	  (setf (ps:chain xhr onload) (lambda ()
					"handle upload completion and process response"
					(if (and (eql (ps:chain xhr ready-state) 4) (eql (ps:chain xhr status) 200))
					    (progn
					      (show-toast "Feedback has been sent successfully." 3000)
					      (setf (ps:chain progress-container style display) "none")
					      (setf (ps:chain loading-indicator style display) "none")
					      (setf (ps:chain document (get-element-by-id "feedback") value) ""))
					    (show-toast "An error has occurred while sending feeback, please try again." 3000))))

	  ;; Send the request only if there's a description or files to send
	  (if feedback-value
	      (progn (ps:chain xhr (send form-data))
		     (show-toast "Feedback is being sent to server" 1500))
	      (show-toast "A feedback value is required" 1500))))

      (ps:chain submit-btn (add-event-listener "click" (lambda ()
							 "submit button click listener"
							 (upload-feedback))))

      (let ((textarea (ps:chain document (get-element-by-id "feedback"))))
	(ps:chain textarea (add-event-listener "input" (lambda ()
							 "auto resize the textarea vertically as the user types in"
							 (setf (ps:chain this style height) "auto")
							 (setf (ps:chain this style height) (+ (ps:chain this scroll-height) "px"))))))
      )))

(defun feedback-css ()
  "the css for the /home endpoint"
  (let ((fg-color "#1a1a1a")
	(red "#FF6060")
	(blue "#CCCCFF")
	(bg-color "#e8e8e8")
	(link-blue "#1e90ff"))
    (cl-css:css
     `((body :text-align center :background-color ,bg-color :color ,fg-color :margin 20px :font-size 16px)
       (h2 :color ,fg-color)
       (p :text-align "justify")
       (footer :text-align center :margin-top 10%)
       (a :color ,link-blue :text-decoration none :decoration none :margin-left 10px :margin-right 10px)
       (button :background-color "#00b800" :border "none" :color ,bg-color :padding "10px 20px" :text-align "center" :text-decoration "none" :display "inline-block" :font-size "16px" :margin "4px 2px" :cursor "pointer" :border-radius "12px" :font-weight bold)
       (.submit-btn :float right :background-color ,fg-color)
       ("a:hover::after" :color ,link-blue)
       ("a:visited" :color ,link-blue :decoration none)
       ("a:hover" :color ,link-blue :decoration underline)
       ("a.logo-link" :color ,fg-color :text-decoration none)
       (.error-p :color ,red)
       (.separator :color ,link-blue)
       ("span.balance" :color ,fg-color)
       ("a.feedback" :font-size 20px :font-weight bold)
       (.buy-tokens-large :font-size 21px :font-weight bold)
       ("a.logo-link:hover" :color "#45a049")

       ("a:not(.logo-link)::after" :content "\"↪\"" :font-weight "bold" :color "inherit" :vertical-align baseline)
       (.logo :display flex :justify-content center :flex-direction :row :align-items :center :gap "10px")
       ("a.logo-link" :text-align center :color ,link-blue :text-decoration none :font-size 30px)
       (.logo-image :width 50px)
       
       
       (".copyright" :color ,fg-color :text-align center)
       (.toast-container :position "fixed" :bottom "20px" :left "50%" :transform "translateX(-50%)" :z-index "9999" :display "flex" :flex-direction "column" :align-items "center")
       (.toast :background-color "#333" :color "white" :padding "15px" :margin-top "10px" :border-radius "5px" :box-shadow "0px 4px 8px rgba(0, 0, 0, 0.1)" :opacity "0.9" :transition "opacity 0.3s ease" :max-width "80%" :word-wrap "break-word" :text-align "center")
       (".toast.fade-out" :opacity "0")
       (textarea :width 99.5% :margin-top 5px :border "1px solid #1a1a1a" :background-color ,bg-color :color ,fg-color :padding 5px)
       (".feedback:focus" :border none :color ,fg-color)
       (.feedback-title :margin-top 12px)
       ("@media only screen and (min-width: 768px)" (body :font-size 18px :width 80% :margin-left 10%))
       ("@media only screen and (max-width: 768px)"
	(footer :margin-top 50% :text-align left :font-size 15px)
	("a.feedback" :font-size 16px :font-weight bold)
	(a :margin-left 2px :margin-right 2px)
	(".copyright" :color ,fg-color :text-align left)) ))))

(define-easy-handler (feedback :uri "/feedback"
			       :acceptor-names '(ninx::ninx)
			       :host *decklm-host*) ()
  ;; feedback doesn't require an account
  (incr-events "feedback")
  (save-ip-visit (remote-addr* *request*))
  (save-country-visit (remote-addr*))
  (let ((cookie (cookie-in "cookie")))
    (if cookie
	(unless (get-email-from-cookie cookie)
	  (progn  (set-cookie "cookie" :expires (- (get-universal-time) 10000))
		  (hunchentoot:redirect "/feedback"))))
    (with-html-output-to-string (*standard-output*)
      "<!DOCTYPE html>"
      (:html :lang "en"
	     (:head
	      (:title "Feedback | DeckLM")
	      (:meta :charset "UTF-8")
              (:link :rel "manifest" :href "/decklm/manifest.json")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:meta :name "description" :content "The home page for DeckLM")
	      (:link :rel "icon" :href "/decklm/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/decklm/static/icons/web/apple-touch-icon.png")
	      (:style (str (feedback-css))))
	     (:body :id "parent"
		    (:a :href "/home" :class "logo-link"
			(:div :class "logo"
			      (:img :class "logo-image" :src "/decklm/icon-512.png" :alt "Logo Icon")
			      "DeckLM"))
		    (:h3 "Describe how you would like to see the service improved.")
		    (:p :class "feedback-title" "Optional: Provide a description of what you want in the slide deck.")
		    (:textarea :class "feedback" :placeholder "Suggest improvements here" :id "feedback")
		    (:button :id "submit-btn" "Submit")
		    (:div :id "progress-container" :style "display: none;"
			  (:progress :id "upload-progress" :value "0" :max "100"))
		    (:div :id "loading-indicator" :style "display: none;"
			  "Submitting... Please wait.")
		    (:div :id "toast-container" :class "toast-container")
		    (footer *request* cookie))
	     (:script (str (feedback-js)))
	     ))))

(define-easy-handler (process-feedback :uri "/process-feedback"
				       :acceptor-names '(ninx::ninx)
				       :host *decklm-host*) ()
  ;; save the feedback to the database, optionally with the person who sent it, if logged in
  (incr-events "process-feedback")
  (save-ip-visit (remote-addr* *request*))
  (save-country-visit (remote-addr*))
  (let ((cookie (cookie-in "cookie"))
	(feedback (post-parameter "feedback")))
    (when feedback (save-feedback feedback cookie))
    "saved"))

(defun pdf-bytes-to-ppt-bytes (bytes)
  "convert pdf bytes to ppt bytes"
  (let* ((name (to-string (make-v4)))
	 (pdf-name (format nil "~a.pdf" name))
	 (pdf-path (format nil "~~/common-lisp/decklm/tmp/~a" pdf-name))
	 (pptx-path (format nil "~~/common-lisp/decklm/tmp/~a.pptx" name)))
    (llms::write-octets-to-binary-file bytes pdf-path)    
    (sb-ext:run-program "/usr/bin/soffice" (list "--infilter=impress_pdf_import"
						 "--convert-to" "pptx"
						 "--outdir" (namestring (truename #p"~/common-lisp/decklm/tmp/"))
						 (namestring (truename (pathname pdf-path)))))
    (prog1
	(llms::read-binary-file-to-octets pptx-path)
      (delete-file pdf-path)
      (delete-file pptx-path)
      )))

(defun pptx-bytes-to-pdf-bytes (bytes)
  "convert pptx bytes to pdf bytes"
  (let* ((name (to-string (make-v4)))
	 (pdf-name (format nil "~a.pdf" name))
	 (pdf-path (format nil "~~/common-lisp/decklm/tmp/~a" pdf-name))
	 (pptx-path (format nil "~~/common-lisp/decklm/tmp/~a.pptx" name)))
    (llms::write-octets-to-binary-file bytes pptx-path)    
    (sb-ext:run-program "/usr/bin/soffice" (list "--headless"
						 "--convert-to" "pdf"
						 "--outdir" (namestring (truename #p"~/common-lisp/decklm/tmp/"))
						 (namestring (truename (pathname pptx-path)))))
    (prog1
	(llms::read-binary-file-to-octets pdf-path)
      (delete-file pdf-path)
      (delete-file pptx-path)
      )))

(flet ((match-path (request)
	 (cl-ppcre:scan "^(/download-deck/)" (script-name* request))))
  (define-easy-handler (download-file :uri #'match-path
				      :acceptor-names '(ninx::ninx)
				      :host *decklm-host*) (type)
    (trivia:match (str:split "/" (script-name*))
      ((list "" "download-deck" doc-id doc-name)
       ;; this will get data from the db and send back binary data to the user.
       ;; should we restrict downloads to users who have account's with us? i don't know, for now we won't
       ;; bytes returned are pptx bytes
       (incr-events "download-deck")
       (save-ip-visit (remote-addr* *request*))
       (save-country-visit (remote-addr*))
       (let* ((json-data (get-document-data doc-id))
	      (name (format nil "~a.~a" doc-name type))
	      (bytes (if (equal type "pptx") (make-pptx-bytes json-data) (make-pdf-bytes json-data))))
	 (if (equal type "pptx")
	     (progn
	       (setf (content-type*) "application/vnd.openxmlformats-officedocument.presentationml.presentation")
	       (setf (content-length*) (length bytes))
	       (setf (header-out "Content-disposition")
		     (format nil "attachment; filename=\"~a\"; filename*=\"~a\"" name name))
	       bytes)
	     (progn
	       (setf (content-type*) "application/pdf")
	       (setf (content-length*) (length bytes))
	       (setf (header-out "Content-disposition")
		     (format nil "attachment; filename=\"~a\"; filename*=\"~a\"" name name))
	       bytes)))
       ))))

(define-easy-handler (register-active :uri "/register-active"
				      :acceptor-names '(ninx::ninx)
				      :host *decklm-host* :default-request-type :post) (pathname) 
  ;; this will register all active users
  (incr-active pathname)
  "")

(define-easy-handler (register-duration :uri "/register-duration"
					:acceptor-names '(ninx::ninx)
					:host *decklm-host* :default-request-type :post) (pathname duration)
  (save-duration (if (equal pathname "") "index" pathname) (read-from-string duration))
  "")

(define-easy-handler (share-email :uri "/share-email"
				  :acceptor-names '(ninx::ninx)
				  :host *decklm-host*) ()
  ;; we will design the sharing via email template here.
  (access-code-html 1234))


(flet ((match-path (request)
	 (cl-ppcre:scan "^(/deck/)" (script-name* request))))
  (define-easy-handler (deck-page :uri #'match-path
				  :acceptor-names '(ninx::ninx)
				  :host *decklm-host*) ()
    (trivia:match (str:split "/" (script-name*))
      ((list "" "deck" doc-id doc-name)
       ;; this route lists the user's generated decks
       (incr-events "deck")
       (save-ip-visit (remote-addr* *request*))
       (save-country-visit (remote-addr*))
       (let ((cookie (cookie-in "cookie")))
	 (let* ((doc (get-document-details doc-id))
		(title (cadr doc))
		(name (format nil "~a.pdf" (str:downcase title)))
		(share-link (format nil "/deck/~a/~a"
				    (car doc)
				    (str:replace-all " " "-" name)))
		(whatsapp-link (format nil "whatsapp://send?text=~a%0A%0A~a"
				       (url-encode (format nil "Download the slide deck of \"~a\" at: " title))
				       share-link))
		(email-link (format nil "mailto:?subject=~a slide deck.&body=~a"
				    title
				    (format nil "Download the PDF slide deck of \"~a\" at ~a" title share-link)))
		(telegram-link (format nil "https://t.me/share/url?url=~a&text=%0A%0A~a" share-link
				       (url-encode (format nil "Download the slide deck of ~a from the above link." title)))))
	   (with-html-output-to-string (*standard-output*)
	     "<!DOCTYPE html>"
	     (:html :lang "en"
		    (:head
		     (:title "Download Deck | DeckLM")
		     (:meta :charset "UTF-8")
		     (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
		     (:meta :name "description" :content "The home page for DeckLM")
		     (:link :rel "manifest" :href "/decklm/manifest.json")
		     (:link :rel "icon" :href "/decklm/static/icons/web/favicon.ico" :sizes "any")
		     (:link :rel "apple-touch-icon" :href "/decklm/static/icons/web/apple-touch-icon.png")
		     (:style (str (user-decks-css))))
		    (:body :id "parent"
			   (:a :href "/home" :class "logo-link"
			       (:div :class "logo"
				     (:img :class "logo-image" :src "/decklm/icon-512.png" :alt "Logo Icon")
				     "DeckLM"))
			   (:div :id "container"
				 (:h4 (str (format nil "File: ~a" title)))
				 (str (format nil " Added on ~a" (extract-timestamp
								  (format nil "~a" (universal-to-timestamp
										    (caddr doc))))))
				 :br :br
				 "PDF is designed for better for reading"
				 :br :br
				 "Slides are designed for better for editing."
				 :br :br
				 (:div :id :buttons
				       (let* ((link (format nil "/download-deck/~a/~a"
							    (car doc)
							    (str:replace-all " " "-" doc-name))))
					 (htm
					  (:a :href (format nil "~a.pdf?type=pdf" link) (:button :id "pdf-button" "Download PDF"))
					  (:a :href (format nil "~a.pptx?type=pptx" link) (:button :id "ppt-button" "Download Slides")))))
				 )
			   
			   (when (is-mobile-browser *request*) ;; add a whatsapp share button on mobile
			     (htm (:a :class "share-link" :title "Share to WhatsApp"
				      :href whatsapp-link
				      (:img :src "/decklm/icons/whatsapp.png" :alt "" :class "wp"))))
			   (:a :class "share-link" :title "Share by Email"
			       :href email-link
			       (:img :src "/decklm/icons/mail.png" :class "wp" :alt ""))
			   (:a :class "share-link" :title "Share via Telegram"
			       :href telegram-link
			       (:img :src "/decklm/icons/telegram.png" :class "wp" :alt ""))
			   (:a :href "https://twitter.com/share?ref_src=twsrc%5Etfw"
			       :class "share-link twitter-share-button"
			       :data-text (format nil "Download the slide deck of ~a from this url: " title)
			       :data-url share-link
			       :data-show-count "false"
			       (:img :src "/decklm/icons/x.png" :alt "" :class "wp"))
			   (:a :href "#" :id "copy-link" :class "share-link"
			       :onclick (format nil "copyToClipboard(~s)" share-link)
			       "Copy 🔗")
			   (:hr)
			   (:div :id "toast-container" :class "toast-container")
			   (footer *request* cookie))
		    (:script (str (home-js)))
		    (:script
		     :async t
		     :src "https://platform.twitter.com/widgets.js"
		     :charset "utf-8")
		    (:script (str (copy-to-clipboard)))
		    ))))))))

;;; payments
;;; when we get this, we first verify the transaction on the server, because you know, fraud, and then we redirect the user to home
;; for now the amount per dollar is 1000000/25 = 40000
(defparameter *tokens-per-dollar* 40000)

(defun verify-payment-fun (tx)
  (unless (transaction-saved-p tx)
    (let* ((data (paypal::get-transaction-details tx))
	   (email (getf data :email))
	   (date (getf data :transaction-date))
	   (amount (getf data :amount)))
      (when email
	(save-transaction tx email amount date)
	(when (get-user-id email)
	  (incr-user-tokens (get-user-id email) (* *tokens-per-dollar* (read-from-string amount))))))))

(define-easy-handler (verify-payment :uri "/verify-payment"
				     :acceptor-names '(ninx::ninx)
				     :host *decklm-host*) (cc amt tx st)
  (verify-payment-fun tx)
  (redirect "/"))

;; this defines the success-webhook that will listen for notifications from paypal.
(define-easy-handler (success-webhook :uri "/success-webhook"
				      :acceptor-names '(ninx::ninx)
				      :host *decklm-host*) ()
  (let* ((body-string (flexi-streams:octets-to-string (raw-post-data) :external-format :utf8))
	 (body-hash (jzon:parse body-string))
	 (id (gethash "id" body-hash))
	 (update-time (gethash "update_time" body-hash)))
    (verify-payment-fun id)))

(defun update-payment-info ()
  "this will run every 60 seconds, fetches all the transactions in the last 2 days, saves the data and adds tokens to the respective users.
we may run into a scenario where the email has no user attached, that's the person used a different email at paypal than here. there, we 
store the user-id as null and await a claim or process the claim manually."
  (dolist (transaction-id (get-transactions))
    (verify-payment-fun transaction-id)))

(defun schedule-payments-update ()
  "this does the actual scheduling of the payment information"
  (let ((timer (sb-ext:make-timer #'decklm::update-payment-info :name 'payments-thread :thread t)))
    (sb-ext:schedule-timer timer 60 :repeat-interval 60)
    timer))

;; this is where we record real time analytics for all the important metrics
;; these prices are dependent on the context length, take more to avoid under reporting
(defun get-input-token-price (model)
  "return the price of a single token when given the model name"
  (let ((v (find model '(("gemini-1.5-pro" . 1.25e-6) ;; this is < 128k
			 ("gemini-1.5-pro-128k" . 2.5e-6)
			 ("gemini-1.5-flash" . 7.5e-8)
			 ("gemini-1.5-flash-128k" . 1.5e-7)
			 ("gpt-4o" . 2.5e-6)
			 ("o1-mini" . 3e-12)))))
    (if v v 2.5e-6)))

(defun get-output-token-price (model)
  "return the price of a single output token when given the model name"
  (let ((v (cdr (find model '(("gemini-1.5-pro" . 5e-6) ;; this is < 128k
			      ("gemini-1.5-pro-128k" . 1e-5)
			      ("gemini-1.5-flash" . 3e-7)
			      ("gemini-1.5-flash" . 6e-7)
			      ("gpt-4o" . 1e-5)
			      ("o1-mini" . 12e-6))
		      :test #'equal
		      :key #'car))))
    (if v v 1e-5)))

(defun get-input-cost (&optional (duration 1))
  "return a single cost for all input from all models used in given duration."
  (let* ((all-inputs (get-all-input-tokens :duration duration)))
    (reduce #'+
	    (mapcar (lambda (c)
		      (* (get-input-token-price (car c)) (cadr c))) all-inputs))))

(defun get-trial-input-cost (&optional (duration 1))
  "return a single cost for all trial input from all models used in given duration."
  (let* ((all-inputs (get-all-trial-input-tokens :duration duration)))
    (reduce #'+
	    (mapcar (lambda (c)
		      (* (get-input-token-price (car c)) (cadr c))) all-inputs))))

(defun get-output-cost (&optional (duration 1))
  "return a single cost for all output from all models used in given duration."
  (let* ((all-inputs (get-all-output-tokens :duration duration)))
    (reduce #'+
	    (mapcar (lambda (c)
		      (* (get-output-token-price (car c)) (cadr c))) all-inputs))))

(defun get-trial-output-cost (&optional (duration 1))
  "return a single cost for all trial output from all models used in given duration."
  (let* ((all-inputs (get-all-trial-output-tokens :duration duration)))
    (reduce #'+
	    (mapcar (lambda (c)
		      (* (get-output-token-price (car c)) (cadr c))) all-inputs))))

(defparameter *token-price* 2.5e-5)
(defun get-input-price (&optional (duration 1))
  "return a single price for all input from all models used in given duration."
  (let* ((all-inputs (get-all-input-tokens :duration duration)))
    (reduce #'+
	    (mapcar (lambda (c)
		      (* *token-price* (cadr c))) all-inputs))))

(defun get-trial-input-price (&optional (duration 1))
  "return a single price for all trial input from all models used in given duration."
  (let* ((all-inputs (get-all-trial-input-tokens :duration duration)))
    (reduce #'+
	    (mapcar (lambda (c)
		      (* *token-price* (cadr c))) all-inputs))))

(defun get-output-price (&optional (duration 1))
  "return a single price for all output from all models used in given duration."
  (let* ((all-inputs (get-all-output-tokens :duration duration)))
    (reduce #'+
	    (mapcar (lambda (c)
		      (* *token-price* (cadr c))) all-inputs))))

(defun get-trial-output-price (&optional (duration 1))
  "return a single price for all trial output from all models used in given duration."
  (let* ((all-inputs (get-all-trial-output-tokens :duration duration)))
    (reduce #'+
	    (mapcar (lambda (c)
		      (* *token-price* (cadr c))) all-inputs))))

(defun diff-lists (list1 list2)
  "Compute the difference in counts between two lists of (key count) pairs."
  (let ((result '()))
    (let ((hash2 (make-hash-table :test 'equal)))
      (dolist (pair list2)
        (setf (gethash (first pair) hash2) (second pair)))
      (dolist (pair list1)
        (let* ((key (first pair))
               (count1 (second pair))
               (count2 (gethash key hash2 0))) ;; Default to 0 if key not in list2.
          (push (list key (- count1 count2)) result))))
    (nreverse result)))

(defun compute-costs (duration)
  "get profit for a given duration, output and input all have trial on them"
  (let* ((input (get-input-cost duration))
	 (output (get-output-cost duration)))
    (+ input output)))

(defun compute-sales (duration)
  "Compute sales revenue by calculating the difference between input/output tokens 
   and trial tokens, and then multiplying the result by *token-price*.
   - duration: The duration (in days) to compute the tokens for.
   - Returns the total revenue from both input and output tokens."
  (flet ((compute-total-sales (tokens)
           "Helper function to compute total sales from a list of token-count pairs."
           (reduce #'+ tokens :key (lambda (c) (* *token-price* (cadr c))))))
    (let* ((input (get-all-input-tokens :duration duration))
           (output (get-all-output-tokens :duration duration))
           (trial-input (get-all-trial-input-tokens :duration duration))
           (trial-output (get-all-trial-output-tokens :duration duration))
           (paid-input (diff-lists input trial-input))
           (paid-output (diff-lists output trial-output)))
      (+ (compute-total-sales paid-input) (compute-total-sales paid-output)))))

(defun compute-costs (duration)
  (+ (get-input-cost duration)
     (get-output-cost duration)))

(defun compute-profit (duration)
  (- (compute-sales duration)
     (compute-costs duration)))

(defun compute-profit-growth (from to)
  "Calculate the profit growth percentage between two time periods.
   - from: Duration (in days) representing the earlier time period.
   - to: Duration (in days) representing the later time period.
   Returns the percentage growth from `from` to `to`, or nil if growth cannot be computed."
  (let* ((p1 (compute-profit from))
         (p2 (compute-profit to))
         (diff (- p2 p1)))
    (if (= p1 0)
        (if (= p2 0)
            0			  ; No growth if both profits are zero
            (if (> p2 0)
                100		  ; 100% growth from zero to positive
                -100))		  ; -100% growth from zero to negative
        (* 100 (/ diff p1)))))

(defun compute-growth (old-value new-value)
  "Compute the percentage growth from old-value to new-value.
   Returns the percentage change as a float, or nil if growth cannot be computed.
   - If old-value is zero and new-value is also zero, returns 0 (no growth).
   - If old-value is zero and new-value is positive, returns 100 (100% growth).
   - If old-value is zero and new-value is negative, returns -100 (-100% growth)."
  (cond
    ((= old-value 0)
     (cond ((= new-value 0) 0)	   ; No growth if both are zero
           ((> new-value 0) 100)   ; 100% growth from zero to positive
           ((< new-value 0) -100))) ; -100% growth from zero to negative
    (t
     (* 100 (/ (- new-value old-value) old-value)))))

(defun merge-lists (list1 list2)
  "when given '((\"a\" 1)) and ((\"a\" 2)) return ((\"a\" 1 2))"
  (let ((acc ()))
    (dolist (data list1)
      (let* ((key (car data))
	     (v1 (cadr data))
	     (v2 (cadr (find key list2 :key #'car :test #'equal))))
	(setq list2 (delete (list key v2) list2 :test #'equal))
	(if v2
	    (push (list key v1 v2) acc)
	    (push (list key v1) acc))))
    `(,@acc ,@list2)))

(define-easy-handler (realtime-analytics :uri "/realtime-analytics"
					 :acceptor-names '(ninx::ninx)
					 :host *decklm-host*) ()
  (let* ((daily-cost (compute-costs 1))
	 (daily-sales (compute-sales 1))
	 (daily-profit (compute-profit 1))

	 (weekly-cost (compute-costs 7))
	 (weekly-sales (compute-sales 7))
	 (weekly-profit (compute-profit 7))

	 (monthly-cost (compute-costs 28))
	 (monthly-sales (compute-sales 28))
	 (monthly-profit (compute-profit 28)))
    (with-html-output-to-string (*standard-output*)
      (htm
       (:html
	(:head
	 (:title "Realtime Analytics")
	 (:style (str (cl-css:css
		       `((.ana-div :width 50vh)
			 (.left-div :float left)
			 (.right-div :float right))))))
	(:body
	 (:div :class "ana-div left-div"
	       (:h4 "Tokenomics Daily")
	       (:table
		   (:tr (:th "type")
		    (:th "Amount"))
		 (:tr (:td "Cost") (:td (str (format nil "~,7f" daily-cost))))
		 (:tr (:td "Sales") (:td (str (format nil "~,7f" daily-sales))))
		 (:tr (:td "Profit") (:td (str (format nil "~,7f" daily-profit))))
		 (:tr (:td "% Growth") (:td (str (format nil "~,7f" (compute-profit-growth 2 1))))))

	       (:h4 "Tokenomics Weekly")
	       (:table
		   (:tr (:th "type")
		    (:th "Amount"))
		 (:tr (:td "Cost") (:td (str (format nil "~,7f" weekly-cost))))
		 (:tr (:td "Sales") (:td (str (format nil "~,7f" weekly-sales))))
		 (:tr (:td "Profit") (:td (str (format nil "~,7f" weekly-profit))))
 		 (:tr (:td "% Growth") (:td (str (format nil "~,7f" (compute-profit-growth 14 7))))))

	       (:h4 "Tokenomics Monthly")
	       (:table
		   (:tr (:th "type")
		    (:th "Amount"))
		 (:tr (:td "Cost") (:td (str (format nil "~,7f" monthly-cost))))
		 (:tr (:td "Sales") (:td (str (format nil "~,7f" monthly-sales))))
		 (:tr (:td "Profit") (:td (str (format nil "~,7f" monthly-profit))))
 		 (:tr (:td "% Growth") (:td (str (format nil "~,7f" (compute-profit-growth 56 28))))))

	       (:h4 "Daily Active")
	       (:table
		   (:tr (:th "Page") (:th "Number") (:th "Growth"))
		 (:tr (:td "Index") (:td (str (get-active "index"))) (:td (str (format nil "~,7f"
										       (compute-growth
											(get-active "index" :duration 2)
											(get-active "index" :duration 1))))))
		 (:tr (:td "Accounts") (:td (str (get-active "accounts"))) (:td (str (format nil "~,7f"
											     (compute-growth
											      (get-active "accounts" :duration 2)
											      (get-active "accounts" :duration 1))))))
		 (:tr (:td "Deck") (:td (str (get-active "deck"))) (:td (str (format nil "~,7f"
										     (compute-growth
										      (get-active "deck" :duration 2)
										      (get-active "deck" :duration 1))))))
		 (:tr (:td "Home") (:td (str (get-active "home"))) (:td (str (format nil "~,7f"
										     (compute-growth
										      (get-active "home" :duration 2)
										      (get-active "home" :duration 1)))))))

	       (:h4 "Daily Events")
	       (:table
		   (:tr (:th "Page") (:th "Number") (:th "Growth"))
		 (:tr (:td "Index") (:td (str (get-events "index")))(:td (str (format nil "~,7f"
										      (compute-growth
										       (get-events "index" :duration 2)
										       (get-events "index" :duration 1))))))
		 (:tr (:td "Accounts") (:td (str (get-events "accounts")))(:td (str (format nil "~,7f"
											    (compute-growth
											     (get-events "accounts" :duration 2)
											     (get-events "accounts" :duration 1))))))
		 (:tr (:td "Deck") (:td (str (get-events "deck"))) (:td (str (format nil "~,7f"
										     (compute-growth
										      (get-events "deck" :duration 2)
										      (get-events "deck" :duration 1))))))
		 (:tr (:td "Home") (:td (str (get-events "home"))) (:td (str (format nil "~,7f"
										     (compute-growth
										      (get-events "home" :duration 2)
										      (get-events "home" :duration 1))))))))))
       (:div :class "ana-div right-div"
	     (:h4 "Parsing Error - Daily")
	     (:table
		 (:tr (:th "Model") (:th "Number") (:th "% change"))
	       (let* ((model-data-1 (get-all-parsing-errors :duration 2))
		      (model-data-2 (get-all-parsing-errors :duration 1))
		      (merged-data (merge-lists model-data-2 model-data-1)))
		 (dolist (data merged-data)
		   (htm
		    (:tr (:td (str (car data)))
			 (:td (str (cadr data)))
			 (:td (str (format nil "~,7f" (compute-growth (let ((old (caddr data)))
									(if old old 0))
								      (cadr data))))))))))

	     (:h4 "Parsing Error - Weekly")
	     (:table
		 (:tr (:th "Model") (:th "Number") (:th "% change"))
	       (let* ((model-data-1 (get-all-parsing-errors :duration 14))
		      (model-data-2 (get-all-parsing-errors :duration 7))
		      (merged-data (merge-lists model-data-2 model-data-1)))
		 (dolist (data merged-data)
		   (htm
		    (:tr (:td (str (car data)))
			 (:td (str (cadr data)))
			 (:td (str (format nil "~,7f" (compute-growth (let ((old (caddr data)))
									(if old old 0))
								      (cadr data))))))))))

	     (:h4 "Parsing Error - Monthly")
	     (:table
		 (:tr (:th "Model") (:th "Number") (:th "% change"))
	       (let* ((model-data-1 (get-all-parsing-errors :duration 56))
		      (model-data-2 (get-all-parsing-errors :duration 28))
		      (merged-data (merge-lists model-data-2 model-data-1)))
		 (dolist (data merged-data)
		   (htm
		    (:tr (:td (str (car data)))
			 (:td (str (cadr data)))
			 (:td (str (format nil "~,7f" (compute-growth (let ((old (caddr data)))
									(if old old 0))
								      (cadr data))))))))))

	     
	     (:h4 "Unique visitors")
	     (:table
		 (:tr (:th "Type")  (:th "Number") (:th "% change"))
	       (:tr (:td "Daily") (:td (str (get-unique-visitors-by-ip 1))) (:td (str (format nil "~,7f"
											      (compute-growth
											       (get-unique-visitors-by-ip 2)
											       (get-unique-visitors-by-ip 1))))))
	       (:tr (:td "Weekly") (:td (str (get-unique-visitors-by-ip 1))) (:td (str (format nil "~,7f"
											       (compute-growth
												(get-unique-visitors-by-ip 14)
												(get-unique-visitors-by-ip 7))))))
	       (:tr (:td "Monthly") (:td (str (get-unique-visitors-by-ip 1))) (:td (str (format nil "~,7f"
												(compute-growth
												 (get-unique-visitors-by-ip 28)
												 (get-unique-visitors-by-ip 56)))))))

	     (:h4 "Top Countries - Daily Visits")
	     (:table
		 (:tr (:th "Country") (:th "Count"))
	       (dolist (data (get-top-country-visitors))
		 (htm (:tr (:td (str (car data))) (:td (str (format nil "~a" (cadr data))))))))

	     (:h4 "Top Countries - Weekly Visits")
	     (:table
		 (:tr (:th "Country") (:th "Count"))
	       (dolist (data (get-top-country-visitors :duration 7))
		 (htm (:tr (:td (str (car data))) (:td (str (format nil "~a" (cadr data))))))))

	     (:h4 "Top Countries - Monthly Visits")
	     (:table
		 (:tr (:th "Country") (:th "Count"))
	       (dolist (data (get-top-country-visitors :duration 28))
		 (htm (:tr (:td (str (car data))) (:td (str (format nil "~a" (cadr data))))))))

	     (:h4 "Top Countries - Daily Decks")
	     (:table
		 (:tr (:th "Country") (:th "Count"))
	       (dolist (data (get-country-deck-creations))
		 (htm (:tr (:td (str (car data))) (:td (str (format nil "~a" (cadr data))))))))

	     (:h4 "Top Countries - Weekly Decks")
	     (:table
		 (:tr (:th "Country") (:th "Count"))
	       (dolist (data (get-country-deck-creations :duration 7))
		 (htm (:tr (:td (str (car data))) (:td (str (format nil "~a" (cadr data))))))))

	     (:h4 "Top Countries - Monthly Decks")
	     (:table
		 (:tr (:th "Country") (:th "Count"))
	       (dolist (data (get-country-deck-creations :duration 28))
		 (htm (:tr (:td (str (car data))) (:td (str (format nil "~a" (cadr data))))))))
	     
	     )))))


(defun index-css ()
  (cl-css:css
   `((body :margin 0 :padding 0 :background-color "white" 
           :font-family "Arial, sans-serif" :line-height 1.5)
     (.main :margin "20px auto" :background-color "#e8e8e8" 
            :padding "20px" :border-radius "15px" 
            :box-shadow "0 4px 8px rgba(0, 0, 0, 0.2)"
            :max-width "1200px")
     (.header :display :flex :justify-content :space-between 
              :align-items :center :margin-bottom "20px")
     (.outer-logo :display :flex :flex-direction :column 
                  :align-items :flex-start :gap "10px")
     (.logo :display :flex :flex-direction :row :align-items :center :gap "10px")
     (a :font-weight bold)
     ("a:visited" :color blue)
     (.logo-image :width "50px" :border-radius "5px")
     (.logo-link :font-size "2em" :font-weight :bold :text-decoration :none 
                 :color "#1e90ff")
     ("a.logo-link:visited" :color "#1e90ff")
     (.logo-description :font-style :italic :font-size "0.9em" 
                        :background-color "yellow" :padding "10px" 
                        :border-radius "5px" :transform "rotate(-5deg)" 
                        :transform-origin "top left")
     (.nav-links :display :flex :gap "15px" :align-items :center)
     (".nav-links a" :text-decoration :none :font-weight :bold)
     (.home :display :inline-block :padding "10px 20px" 
            :background-color "#28a745" :color "white" 
            :text-decoration :none :font-weight :bold 
            :border-radius "20px" :text-align :center :border "none")
     ("a.home" :color "#e8e8e8")
     (".home:hover" :background-color "#1e7e34")
     (.try-div :display :flex :justify-content :center :align-items :center 
               :padding "10px 20px" :background-color "#28a745" 
               :color "white" :text-decoration :none 
               :font-weight :bold :border-radius "20px" 
               :margin "10px auto" :border "none" :width "150px")
     (".try-div a" :text-decoration none :color "#e8e8e8")
     (".try-div:hover" :background-color "#1e7e34")

     (.video-container :position :relative :display :flex 
                       :justify-content :center :align-items :center 
                       :margin "20px auto" :width "100%" 
                       :max-width "1000px")
     (.video-demo :width "100%" :height "auto" :border-radius "10px")
     (.video-overlay :position :absolute :top 0 :left 0 
                     :width "100%" :height "100%" 
                     :background-color "rgba(255, 255, 255, 0.7)" 
                     :display :flex :justify-content :center 
                     :align-items :center :border-radius "10px" 
                     :pointer-events :none)
     (.overlay-button :padding "15px 30px" :font-size "1.2em" 
                      :background-color "#28a745" :color "#e8e8e8" 
                      :border "none" :border-radius "25px" 
                      :text-align :center :text-decoration :none 
                      :font-weight :bold :cursor :pointer 
                      :pointer-events :all :transition "background-color 0.3s")
     (".overlay-button:hover" :background-color "#1e7e34")

     (.big-text :font-size "2em" :margin-bottom "20px")
     (.center-item :text-align center)
     (.no-margin :margin 0 :font-weight bold)
     (.sample-pages :display :flex :gap "10px" :justify-content :center 
                    :margin-top "20px")
     (.pdf-sample :position :relative :width "calc(50% - 10px)" 
                  :max-width "calc(50% - 10px)" :flex-basis "calc(50% - 10px)")
     (".pdf-sample-text" :position :absolute :top "5px" :left "5px" 
                         :color "#e8e8e8" :font-weight :bold 
                         :background-color "rgba(0, 0, 0, 0.7)" 
                         :padding "5px" :border-radius "3px")
     (".sample-pages img" :width "100%" :height "auto" :display :block 
                          :border-radius "5px")
     (.footer :margin-top "20px" :font-size "0.9em")
     (.rest :font-size "20px")
     (.small-div :width 60% :margin-left 20%)
     (.fab
      :position :fixed :bottom "20px" :right "20px" :z-index 1000
      :background-color "#1e90ff" :color "#e8e8e8" :border "none"
      :border-radius "30px" :padding "10px 20px" :font-size "1em"
      :box-shadow "0px 4px 6px rgba(0, 0, 0, 0.1)"
      :cursor :pointer :transition "background-color 0.3s, transform 0.2s"
      :display :inline-block :text-align :center :font-weight :bold)
     (".fab:hover"
      :background-color "#1e7e34"
      :transform "scale(1.05)")
     (".fab a:visited" :color "#e8e8e8")
     ("@media (max-width: 768px)"
      (.main :margin "0px auto" :max-width 100%)
      (.small-div :width 95% :margin-left 0)
      (.big-text :font-size 1.5em)
      (.header :flex-direction column)
      (.logo-description :font-size 0.8em )
      (.sample-pages :flex-direction column :gap "15px" :align-items center)
      (.pdf-sample :width "100%" :max-width "100%" :margin "0 auto")
      (.pdf-sample-text :font-size 0.6em)
      (.overlay-button :font-size 0.9em)
      (.fab
       :font-size "0.9em" :padding "8px 16px" :bottom "15px" :right "15px")
      ))))

(define-easy-handler (index :uri "/"
			    :acceptor-names '(ninx::ninx)
			    :host *decklm-host*) ()
  (format *terminal-io* "~%~a~%" *request*)
  (incr-events "index")
  (save-ip-visit (remote-addr* *request*))
  (save-country-visit (remote-addr*))
  (let* ((cookie (cookie-in "cookie"))
	 (email (when cookie (get-email-from-cookie cookie))))
    (if (and email cookie)
	(hunchentoot:redirect "/home")
	(with-html-output-to-string (*standard-output*)
	  "<!DOCTYPE html>"
	  (htm (:html :lang "en"
		      (:head
		       (:title "DeckLM — Online slide deck creator")
		       (:meta :charset "UTF-8")
		       (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
		       (:link :rel "manifest" :href "/decklm/manifest.json")
		       (:meta :name "description" :content "Landing page for DeckLM, the online slide deck creator")
		       (:link :rel "icon" :href "/decklm/static/icons/web/favicon.ico" :sizes "any")
		       (:link :rel "apple-touch-icon" :href "/decklm/static/icons/web/apple-touch-icon.png")
		       (:style (str (index-css)))
		       (:script "<!-- Google tag (gtag.js) -->
<script async src=\"https://www.googletagmanager.com/gtag/js?id=AW-16666958238\">
</script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'AW-16666958238');
</script>")
		       )
		      (:body
		       (:div :class "main"
			     (:div :class "header"
				   (:div :class "outer-logo"
					 (:a :href "/home" :class "logo-link"
					     (:div :class "logo" (:img :src "/decklm/icon-512.png" :class "logo-image")
						   "DeckLM"))
					 (:div :class "logo-description" "Simple and easy to use"))
				   (:div :class "nav-links"
					 (:a :href "/pricing" "Pricing")
					 (:a :class "home" :href "/accounts" "Signup or Signin")))
			     (:div :class "rest"
				   (:h1 :class "big-text center-item" "Insanely simple slide deck generation!")
				   (:p :class "center-item" "DeckLM uses your resources—images, PDFs, plain text, and CSV files—to create a slide deck with detailed, relevant information that you can customize, download the finished deck as a PDF or PowerPoint presentation—all in under 5 minutes.")
				   (:div :class "try-div center-item" (:a :href "/accounts" "Try it for free."))
				   (:div :class "video-container"
					 (:video :class "video-demo" :id "demo-video" :controls t
						 :src (if (is-mobile-browser *request*) "/decklm/demo-mobile.mp4" "/decklm/demo.mp4")
						 :alt "Demonstration video for the DeckLM.")
					 (:div :class "video-overlay"
					       (:a :class "overlay-button" :href "javascript:void(0);" 
						   :data-target "#demo-video" "▶ See how DeckLM works")))

				   (:script "
document.addEventListener('DOMContentLoaded', function () {
  const overlayButton = document.querySelector('.overlay-button');
  overlayButton.addEventListener('click', function () {
    const video = document.querySelector(overlayButton.getAttribute('data-target'));
    if (video) {
      video.play();
      overlayButton.parentElement.style.display = 'none';
    }
  });
});
")
				   
				   
				   (:div :class "sample-pages"
					 (:div :class "pdf-sample" (:span :class "pdf-sample-text" "PDF sample page")
					       (:img :src "/decklm/static/images/sample-pdf.png" :alt "Sample PDF page generated."))
					 (:div :class "pdf-sample" (:span :class "pdf-sample-text" "PowerPoint sample page")
					       (:img :src "/decklm/static/images/sample-pptx.png" :alt "Sample PPTX page generated.")))
				   (:div :class "small-div center-item"
					 (:h3 :class "big-text no-margin center-item" "With clear billing: only pay for what you use.")
					 (:p :class "center-item" "You are only charged when you generate a slide deck and are charged per word in your resources and the deck created. No hidden charges. See " (:a :href "/pricing" "Pricing."))
					 (:h3 :class "big-text no-margin center-item" "We want to hear from you.")
					 (:p :class "center-item" "We take all questions about pricing, how to use and all other inquiries. "
					     (:a :href "mailto:lam@ninx.xyz" "Email our founder")
					     ", he wants to hear from you.")))
			     (:button :class "fab" (:a :href "/accounts" "Try it now"))
			     (:div :class "footer center-item"
				   "DeckLM is built and backed by " (:a :href "https://ninx.xyz/about" "Ninx Technology Limited.")
				   " Enjoy the rest of your day!")))
		      (:script (str (beacon-js)))
		      (:script (str (duration-js)))
		      ))))))
