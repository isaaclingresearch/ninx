(in-package :pageone)

(defvar *pageone-url* (uiop:getenv "PAGEONE_HOST"))
(defvar *pageone-host* (let ((host (uiop:getenv "PAGEONE_HOST")))
			 (if (equal "443" ninx:*ninx-https-port*)
			     host
			     (format nil "~a:~a" host ninx:*ninx-https-port*))))

(defparameter *hourly-scrap-p* t)

(defparameter *actors* (make-actor-system))

(def-suite pageone)
;; nation media papers

(defun get-yyyy-mm-dd (&optional yyyy mm dd)
  (if (and yyyy mm dd)
      (format nil "~a-~a-~a" yyyy
	      	(if (< mm 10) (format nil "0~a" mm) mm)
		(if (< dd 10) (format nil "0~a" dd) dd))
      (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
	  (decode-universal-time (get-universal-time))
	(format nil "~a-~a-~a" year
		;; we add a zero to make yyyy-mm-dd for when the values are single digit.
		(if (< month 10) (format nil "0~a" month) month)
		(if (< date 10) (format nil "0~a" date) date)))))

(defun get-daily-monitor (&optional (yyyy-mm-dd (get-yyyy-mm-dd)))
  "fetch the image for the day's daily monitor"
  (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
      (drakma:http-request (format nil "https://d25r30nj823dl8.cloudfront.net/epaper/daily-monitor-ug/~a/thumbnails/1.jpg"
				   yyyy-mm-dd)
			   :method :get
			   :user-agent :firefox
			   :additional-headers '(("referer" . "https://epaper.nation.africa/ug")))
    (declare (ignore status-text request-uri flexi-response response-bool))
    (if (equal response-code 200)
	(progn (save-image "Daily Monitor" response "image/jpg" yyyy-mm-dd)
	       :success)
	(format t "Error: ~a. Daily Monitor failed. ~%" response-code))))

(defun scrap-monitor (&key (days 0) (all nil))
  "gets the days monitor, or if days is given, the monitor for given days ago.
   if all is true, will scrap all days from days to 0"
  (when (>= days 0)
    (get-daily-monitor (car (str:split "T" (format nil "~a" (chronicity:parse (format nil "~a days ago" days))))))
    (when all
      (scrap-monitor :days (1- days) :all t))))

(defun get-ennyanda (&optional (yyyy-mm-dd (get-yyyy-mm-dd)))
  "fetch the image for the day's daily monitor"
  (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
      (drakma:http-request (format nil "https://d25r30nj823dl8.cloudfront.net/epaper/ennyanda-ug/~a/thumbnails/1.jpg"
				   yyyy-mm-dd)
			   :method :get
			   :user-agent :firefox
			   :additional-headers '(("referer" . "https://epaper.nation.africa/ug")))
    (declare (ignore status-text request-uri flexi-response response-bool))
    (if (equal response-code 200)
	(progn (save-image "Ennyanda" response "image/jpg" yyyy-mm-dd)
	       :success)
	(format t "Error: ~a. Ennyanda failed. ~%" response-code))))

(defun scrap-ennyanda (&key (days 0) (all nil))
  "gets the days monitor, or if days is given, the monitor for given days ago.
   if all is true, will scrap all days from days to 0"
  (when (>= days 0)
    (get-ennyanda (car (str:split "T" (format nil "~a" (chronicity:parse (format nil "~a days ago" days))))))
    (when all
      (scrap-ennyanda :days (1- days) :all t))))

(defun get-seeds-of-gold (&optional (yyyy-mm-dd (get-yyyy-mm-dd)))
  "fetch the image for the day's daily monitor"
  (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
      (drakma:http-request (format nil "https://d25r30nj823dl8.cloudfront.net/epaper/seeds-of-gold-ug/~a/thumbnails/1.jpg"
				   yyyy-mm-dd)
			   :method :get
			   :user-agent :firefox
			   :additional-headers '(("referer" . "https://epaper.nation.africa/ug")))
    (declare (ignore status-text request-uri flexi-response response-bool))
    (if (equal response-code 200)
	(progn (save-image "Seeds of Gold" response "image.jpg" yyyy-mm-dd)
	       :success)
	(format t "Error: ~a. Seeds of Gold.~%" response-code))))

(defun scrap-seeds-of-gold (&key (days 0) (all nil))
  "gets the days monitor, or if days is given, the monitor for given days ago.
   if all is true, will scrap all days from days to 0"
  (when (>= days 0)
    (get-seeds-of-gold (car (str:split "T" (format nil "~a" (chronicity:parse (format nil "~a days ago" days))))))
    (when all
      (scrap-seeds-of-gold :days (1- days) :all t))))

(defun get-the-east-african (&optional (yyyy-mm-dd (get-yyyy-mm-dd)))
  "fetch the image for the day's daily monitor"
  (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
      (drakma:http-request (format nil "https://d25r30nj823dl8.cloudfront.net/epaper/the-east-african-ug/~a/thumbnails/1.jpg"
				   yyyy-mm-dd)
			   :method :get
			   :user-agent :firefox
			   :additional-headers '(("referer" . "https://epaper.nation.africa/ug")))
    (declare (ignore status-text request-uri flexi-response response-bool))
    (if (equal response-code 200)
	(progn (save-image "The East African" response "image/jpg" yyyy-mm-dd)
	       :success)
	(format t "Error: ~a. The East African failed.~%" response-code))))

(defun scrap-the-east-african (&key (days 0) (all nil))
  "gets the days monitor, or if days is given, the monitor for given days ago.
   if all is true, will scrap all days from days to 0"
  (when (>= days 0)
    (get-the-east-african (car (str:split "T" (format nil "~a" (chronicity:parse (format nil "~a days ago" days))))))
    (when all
      (scrap-the-east-african :days (1- days) :all t))))


;; VISION GROUP.

(defun get-vision-group (&optional (months 0) (d1 1) (d2 1))
  "new vision website is a bit odd. the pattern is /2024/11/Untitled-1-1.gif
   d1 and d2 represent the -1-1 in the last part of the script. 
   i have observed that the d2 can go upto 30 and d1 from 1 to 3, rest is 404.
   so we can only write a single function without separation. 
   run it from 1 to 3 for d1 and for each from 1 to 30 for d2 for a given month, then we can go down, since we start at the current month.
   we can run given months back. months can be from 0"
  (trivia:match (str:split "-" (format nil "~a" (chronicity:parse (format nil "~a months ago" months))))
    ((list year month date)
     (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
	 (drakma:http-request (format nil "https://epapers.visiongroup.co.ug/wp-content/uploads/~a/~a/Untitled-~a-~a.gif"
				      year month d1 d2)
			      :method :get
			      :user-agent :firefox
			      :additional-headers '(("referer" . "https://epaper.visiongroup.co.ug")))
       (declare (ignore status-text request-uri flexi-response response-bool))
       (if (equal response-code 200)
	   (progn (save-image "Vision Group paper" response "image/git" (get-yyyy-mm-dd year month date))
		  :success)
	  (format t "Error: ~a. Vision Group failed.~%" response-code))))))

(defun scrap-new-vision (&key (months 0) (d1 1) (d2 1))
  "Recursively scrape data by incrementing d2 to 30, then incrementing d1 to 3,
   and finally decrementing months when all combinations are processed."
  (when (and (>= months 0) (<= d1 3) (<= d2 30))
    ;; Call the get-vision-group function with current values
    (get-vision-group months d1 d2)
    ;; Recursive conditions
    (cond
      ;; If d2 reaches 30 and d1 is less than 3, reset d2 to 1 and increment d1
      ((and (= d2 30) (< d1 3))
       (scrap-new-vision :months months :d1 (1+ d1) :d2 1))
      ;; If d2 reaches 30 and d1 reaches 3, decrement months and restart with d1 = 1 and d2 = 1
      ((and (= d2 30) (= d1 3))
       (scrap-new-vision :months (1- months) :d1 1 :d2 1))
      ;; Otherwise, increment d2
      (t (scrap-new-vision :months months :d1 d1 :d2 (1+ d2))))))


;; OBSERVER

(defun month-abbr (month)
  (trivia:match month
    (1 "Jan")
    (2 "Feb")
    (3 "Mar")
    (4 "Arp")
    (5 "May")
    (6 "Jun")
    (7 "Jul")
    (8 "Aug")
    (9 "Sep")
    (10 "Oct")
    (11 "Nov")
    (12 "Dec")))

(defun get-observer (&optional (weeks 0))
  "Generate the filename for The Observer based on the current week and optional weeks offset.
   - The Observer is published weekly on Wednesday and runs up to the next Tuesday.
   - Use 'weeks' to go forward or backward in time."
  (let* ((current-timestamp (now))
         ;; Parse 'this Wednesday' and adjust for the week offset
         (this-wednesday (chronicity:parse "this wednesday"))
         (wednesday (if (timestamp> current-timestamp this-wednesday)
                        (chronicity:parse "last wednesday")
                        this-wednesday))
         ;; Adjust for optional week offset
         (offset-wednesday (timestamp+ wednesday (* weeks 7 86400) :sec)) ; Offset in seconds
         (next-tuesday (timestamp+ offset-wednesday (* 6 86400) :sec)) ; Add 6 days for Tuesday
         ;; Extract components for formatting
         (month (timestamp-month offset-wednesday))
         (wednesday-date (timestamp-day offset-wednesday))
         (tuesday-date (timestamp-day next-tuesday))
         (year (timestamp-year offset-wednesday))
	 ;; Format and return the filename
	 (url (format nil "https://observer.ug/images2/print/~a/The-Observer-~a-~a-~a-~a.jpg"
		      year (month-abbr month) wednesday-date tuesday-date year)))
    (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
	(drakma:http-request url
			     :method :get
			     ;; :user-agent :firefox ;; this causes an error for now, don't know, will return to it later.
			     :additional-headers '(("referer" . "https://observer.ug"))
			     )
      (declare (ignore status-text request-uri flexi-response response-bool))
      (if (equal response-code 200)
	  (progn (save-image "The Observer" response "image/jpg" (get-yyyy-mm-dd (timestamp-year offset-wednesday)
										 (timestamp-month offset-wednesday)
										 (timestamp-day offset-wednesday)))
		 :success)
	  (format t "Error: ~a. Observer failed.~%" response-code)))))

(defun scrap-observer (&optional (weeks 0))
  "go back the given number of weeks"
  (when (>= weeks 0)
    (get-observer (- weeks))
    (scrap-observer (1- weeks))))

(defun initial-scrap ()
  "this scraps 1000 days worth of images"
 (scrap-observer 162)
 (scrap-new-vision :months 36)
 (scrap-monitor :days 1000 :all t)
 (scrap-ennyanda :days 1000 :all t)
 (scrap-seeds-of-gold :days 1000 :all t)
 (scrap-the-east-african :days 1000 :all t)
)

(defun daily-scrap ()
  "this scraps only a single day; we run 24 times a day"
  (format *terminal-io* "~%scrapping~%")
  (scrap-observer)
  (scrap-new-vision)
  (scrap-monitor)
  (scrap-ennyanda)
  (scrap-seeds-of-gold)
  (scrap-the-east-african)
  (sleep 3600)
  (daily-scrap))

;;; db access functions.
(ensure-directories-exist (format nil "~a/common-lisp/ninx/apps/pageone/db/" (uiop:getenv "HOME")))
(defparameter *db* (connect  (format nil  "~a/common-lisp/ninx/apps/pageone/db/pageone.db" (uiop:getenv "HOME"))))

(test start-tests
  (is (not (null (setf *db* (progn
			      (disconnect *db*)
			      (connect (format nil  "~a/common-lisp/ninx/apps/pageone/db/pageone-test.db" (uiop:getenv "HOME")))))))))

(defun initialise-db ()
  "create the tables"
  (create-tables))

(test initialise-db (is (null (initialise-db))))


(defun create-tables ()
  "create the tables for analytics and images"
  (execute-non-query *db* "create table if not exists images (id text primary key, data blob, mimetype text, digest blob, date integer, paper_name text, unique(paper_name, date, digest))")
  (execute-non-query *db* "create table if not exists image_requests (id integer primary key autoincrement, date text unique, count integer)")
  (execute-non-query *db* "create table if not exists image_downloads (id integer primary key autoincrement, date text unique, count integer)")
  (execute-non-query *db* "create table if not exists user_ids (id text primary key, creation_date default current_timestamp)"))
(test create-tables (is (null (create-tables))))

(defun delete-tables ()
  "delete all tables"
  (dolist (table '("images" "image_requests" "user_ids" "image_downloads"))
    (execute-non-query *db* (format nil "drop table if exists ~a" table))))
(test delete-tables (is (null (delete-tables))))

(defun reset-tables ()
  (handler-case (delete-tables) (error (err) (declare (ignore err))))
  (create-tables))
(test reset-tables (is (null (reset-tables))))

;; user ids
(defun create-user-id ()
  "function creates, saves and returns a user id"
  (let ((uuid (to-string (make-v4))))
    (execute-non-query *db* "insert into user_ids (id) values (?)" uuid)))
(test create-user-id (is (null (create-user-id))))

(defun confirm-user-id (id)
  "when a user sends in a request, it is accompanied with this: so we confirm it or reject it.
   this is a basic level of security. i currently have no way of testing this."
  (execute-to-single *db* "select id from user_ids where id=?" id))

;; DATA FUNCTIONS
(defun save-image (paper-name data mimetype date &optional (id (to-string (make-v4))))
  "the optional id is for tests"
  (let ((digest (ironclad:digest-sequence 'ironclad:md5 data)))
    (execute-non-query *db* "insert or ignore into images (id, paper_name, date, data, digest, mimetype) values (?, ?,?,?,?,?)" id paper-name date data digest mimetype)))
(test save-image (is (null (save-image "test-paper" (read-binary-file-to-octets "~/common-lisp/ninx/apps/pageone/test/test.png") "image/png" (get-yyyy-mm-dd) "test-paper"))))
;; this test tests what happens when there's collision of uniques.
(test save-image-fails (is  (null (save-image "test-paper" (read-binary-file-to-octets "~/common-lisp/ninx/apps/pageone/test/test.png") "image/png" (get-yyyy-mm-dd "test-image")))))

(defun get-images (&optional (count 1))
  "each page has 10 images; we use count to offset so for 2 we start at 10 etc "
  (execute-to-list *db* "select id, paper_name, date from images order by date desc limit ? offset ?" (* count 10) (* 10 (1- count))))
(test get-images (is (not (null (get-images)))))

(defun get-image-data (image-id)
  (execute-to-list *db* "select mimetype, data from images where id = ?" image-id))
(test get-image-data (is (equalp `(("image/png" ,(read-binary-file-to-octets "~/common-lisp/ninx/apps/pageone/test/test.png")))
				 (get-image-data "test-paper"))))

;; analytics

(defun incr-image-requests ()
  "this is called up every time a request for image meta data is sent. we incr 10 between we send 10 such images. 
   we expect to send 10 images from the requests."
  (let ((date (get-yyyy-mm-dd)))
    (execute-non-query *db* "insert into image_requests (date, count) values (?, ?) on conflict (date) do update set count = count + 1 where date=?" date 1 date)))
(test incr-image-requests (is (null (incr-image-requests))))

(defun get-image-requests (&key (duration 1))
  "get the top count number of countries witht the highest unique visitors in duration"
  (execute-single *db* (format nil (if (= 1 duration)
				       "select sum(count) from image_requests where date > date('~a', '-~a day') group by date"
				       "select sum(count) from image_requests where date > date('~a', '-~a days') group by date")
			       (get-yyyy-mm-dd) duration)))
(test get-image-requests (is (eql 1 (get-image-requests))))

(defun incr-image-downloads ()
  "this is called up every time a download for image meta data is sent. we incr 1"
  (let ((date (get-yyyy-mm-dd)))
    (execute-non-query *db* "insert into image_downloads (date, count) values (?, ?) on conflict (date) do update set count = count + 1 where date=?" date 1 date)))
(test incr-image-downloads (is (null (incr-image-downloads))))

(defun get-image-downloads (&key (duration 1))
  "get the top count number of countries witht the highest unique visitors in duration"
  (execute-single *db* (format nil (if (= 1 duration)
				       "select sum(count) from image_downloads where date > date('~a', '-~a day') group by date"
				       "select sum(count) from image_downloads where date > date('~a', '-~a days') group by date")
			       (get-yyyy-mm-dd) duration)))
(test get-image-downloads (is (eql 1 (get-image-downloads))))


;; last test returns to pageone db
(test return-to-pageone-db (null (setq *db* (progn
					      (disconnect *db*)
					      (connect  (format nil  "~a/common-lisp/ninx/apps/pageone/db/pageone.db" (uiop:getenv "HOME")))))))


;;; server
;;; define the routes.

(defun home-css ()
  (cl-css:css
   `((body :line-height 1.4 :font-size 16px :padding "0 10px" :margin "50px auto" :max-width 650px :text-align left :text-wrap pretty)
     ("a:visited" :color blue)
     )))

(define-easy-handler (pageone-index :uri (define-matching-functions "^/$" *pageone-host*)
				    :host *pageone-host*
				    :acceptor-names '(ninx::ninx)) ()
  (with-html-output-to-string (*standard-output*)
    "<!DOCTYPE html>"
    (htm (:html :lang "en"
		(:head
		 (:title "PageOne")
		 (:meta :charset "UTF-8")
		 (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
		 (:meta :name "description" :content "The accounts page for DeckLM")
		 (:link :rel "icon" :href "/ninx/static/icons/web/favicon.ico" :sizes "any")
		 (:link :rel "apple-touch-icon" :href  "/pageone.ninx/static/icons/web/apple-touch-icon.png")
		 (:link :rel "manifest" :href "/manifest.json")
	         (:style (cl-who:str (home-css)))
		 (:link :href "https://fonts.googleapis.com/css?family=Roboto&display=swap" :rel "stylesheet"))
		(:body
		 (:h1 (:a :href "/" "PageOne."))
		 (:p "PageOne is a mobile application that brings you daily Ugandan newspaper front pages.")
		 (:h2 "Team")
		 (:p (:a :target "_blank" :href "https://ninx.xyz" "Ninx Technology Limited: Mobile applications division."))
		 (:h2 "Useful Links")
		 (:a :target "_blank" :href "https://pageone.ninx.xyz/privacy.txt" "Privacy Policy.")
		 (:b (:p "Mail us at " (:a :href "mailto:info@ninx.xyz" "info@ninx.xyz")))
		 (:h2 "Our Other Products")
		 (:p (:a :target "_blank" :href "https://decklm.com" "DeckLM") " - Generate slides from your learning resources in minutes.")
		 (:p (:a :target "_blank" :href "https://spotpdf.com" "SpotPDF") " - Convert between all image and document formats.")
		 :hr
		 (:b "Ninx Technology Limited,")
		 :br
		 (:b "Lugoba North, Kazo Lugoba, Nansana Division.")
		 :br
		 (:b "P.O.Box 112999, Wakiso,")
		 :br
		 (:b "Wakiso, Uganda."))))))

(define-easy-handler (privicy.txt
		      :uri (define-matching-functions "^/privacy.txt$" *pageone-host*)
		      :host *pageone-host*) ()
  (setf (content-type*) "text/plain")
  (setf (header-out "content-disposition") "inline; filename=privacy.txt")
  (ninx:read-binary-file-to-octets #p"~/common-lisp/ninx/priv/pageone.ninx/privacy.txt"))


(define-easy-handler (manifest.json
		      :uri (define-matching-functions "^/manifest.json$" *pageone-host*)
		      :host *pageone-host*) ()
  (setf (content-type*) "text/plain")
  (setf (header-out "content-disposition") "inline; filename=manifest.json")
  (ninx:read-binary-file-to-octets #p"~/common-lisp/ninx/priv/pageone.ninx/manifest.json"))

(define-easy-handler (favicon
		      :uri (define-matching-functions "/favicon.ico" *pageone-host*)
		      :host *pageone-host*) ()
  (setf (content-type*) "image/vnd.microsoft.icon")
  (setf (header-out "content-disposition") "inline; filename=favicon.ico")
  (ninx:read-binary-file-to-octets #p"~/common-lisp/ninx/priv/pageone.ninx/static/icons/web/favicon.ico"))

;; this returns image data, 10 images are read onto a page, starting with page=1
(define-easy-handler (get-images-route
		      :uri (define-matching-functions "^/get-images$" *pageone-host*)
		      :host *pageone-host*)
    (page)
  (incr-image-requests)
  (setf (header-out "access-control-allow-origin") "*")
  (let ((papers (get-images (cond ((stringp page) (parse-integer page)) (t page)))))
    (setf (content-type*) "application/json")
    ;; when we have < 10 papers, we have reached the end, don't send a timestamp, other send the timestamp of the 10th.
    (jzon:stringify
     (if papers
	 (hash-create (list (list "data" (loop for paper in papers
					       collect
					       (hash-create (list (list "name" (second paper))
								  (list "id" (first paper))
								  (list "date" (third paper))))))))
	 (hash-create (list (list "data" nil)))))))

(define-easy-handler (get-image-route
		      :uri (define-matching-functions "^/get-image$" *pageone-host*)
		      :host *pageone-host*)
    (id)
  (incr-image-downloads)
  (setf (header-out "access-control-allow-origin") "*")
  (let ((image-data (get-image-data id)))
    (setf (content-type*) (caar image-data))
    (cadar image-data)))

(define-easy-handler (realtime-analytics :uri (define-matching-functions "^/realtime-analytics$" *pageone-host*)
					 :acceptor-names '(ninx::ninx)
					 :host *pageone-host*) ()
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
	     (:table
		 (:tr (:th "Requests in Time") (:th "Number") (:th "Growth"))
	       (:tr (:td "Day") (:td (str (get-image-requests :duration 1))) (:td (str (format nil "~,7f"
										     (compute-growth
										      (get-image-requests :duration 2)
										      (get-image-requests :duration 1))))))
	       (:tr (:td "Week") (:td (str (get-image-requests :duration 7))) (:td (str (format nil "~,7f"
												(compute-growth
												 (get-image-requests :duration 14)
												 (get-image-requests :duration 7))))))
	       (:tr (:td "Month") (:td (str (get-image-requests :duration 28))) (:td (str (format nil "~,7f"
												  (compute-growth
												   (get-image-requests :duration 56)
												   (get-image-requests :duration 28))))))
	       (:table
		   (:tr (:th "Downloads in Time") (:th "Number") (:th "Growth"))
		 (:tr (:td "Day") (:td (str (get-image-downloads :duration 1))) (:td (str (format nil "~,7f"
											(compute-growth
											 (get-image-downloads :duration 2)
											 (get-image-downloads :duration 1))))))
		 (:tr (:td "Week") (:td (str (get-image-downloads :duration 7))) (:td (str (format nil "~,7f"
											 (compute-growth
											  (get-image-downloads :duration 14)
											  (get-image-downloads :duration 7))))))
		 (:tr (:td "Month") (:td (str (get-image-downloads :duration 28))) (:td (str (format nil "~,7f"
											   (compute-growth
											    (get-image-downloads :duration 56)
											    (get-image-requests :duration 28))))))))))))))



;;;;;; AVB ROUTES.

(defparameter *avb-host* "127.0.0.1:8443")

(define-easy-handler (pageone-index-avb :uri (define-matching-functions "^/$" *avb-host*)
					:host *avb-host*
					:acceptor-names '(ninx::ninx)) ()
  (format *terminal-io* "~%called~%~%")
  (with-html-output-to-string (*standard-output*)
    "<!DOCTYPE html>"
    (htm (:html :lang "en"
		(:head
		 (:title "PageOne")
		 (:meta :charset "UTF-8")
		 (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
		 (:meta :name "description" :content "The accounts page for DeckLM")
		 (:link :rel "icon" :href "/ninx/static/icons/web/favicon.ico" :sizes "any")
		 (:link :rel "apple-touch-icon" :href  "/pageone.ninx/static/icons/web/apple-touch-icon.png")
		 (:link :rel "manifest" :href "/manifest.json")
	         (:style (cl-who:str (home-css)))
		 (:link :href "https://fonts.googleapis.com/css?family=Roboto&display=swap" :rel "stylesheet"))
		(:body
		 (:h1 (:a :href "/" "PageOne."))
		 (:p "PageOne is a mobile application that brings you daily Ugandan newspaper front pages.")
		 (:h2 "Team")
		 (:p (:a :target "_blank" :href "https://ninx.xyz" "Ninx Technology Limited: Mobile applications division."))
		 (:h2 "Useful Links")
		 (:a :target "_blank" :href "https://pageone.ninx.xyz/privacy.txt" "Privacy Policy.")
		 (:b (:p "Mail us at " (:a :href "mailto:info@ninx.xyz" "info@ninx.xyz")))
		 (:h2 "Our Other Products")
		 (:p (:a :target "_blank" :href "https://decklm.com" "DeckLM") " - Generate slides from your learning resources in minutes.")
		 (:p (:a :target "_blank" :href "https://spotpdf.com" "SpotPDF") " - Convert between all image and document formats.")
		 :hr
		 (:b "Ninx Technology Limited,")
		 :br
		 (:b "Lugoba North, Kazo Lugoba, Nansana Division.")
		 :br
		 (:b "P.O.Box 112999, Wakiso,")
		 :br
		 (:b "Wakiso, Uganda."))))))

(define-easy-handler (privicy.txt-avb
		      :uri (define-matching-functions "^/privacy.txt$" *avb-host*)
		      :host *avb-host*) ()
  (setf (content-type*) "text/plain")
  (setf (header-out "content-disposition") "inline; filename=privacy.txt")
  (ninx:read-binary-file-to-octets #p"~/common-lisp/ninx/priv/pageone.ninx/privacy.txt"))


(define-easy-handler (manifest.json-avb
		      :uri (define-matching-functions "^/manifest.json$" *avb-host*)
		      :host *avb-host*) ()
  (setf (content-type*) "text/plain")
  (setf (header-out "content-disposition") "inline; filename=manifest.json")
  (ninx:read-binary-file-to-octets #p"~/common-lisp/ninx/priv/pageone.ninx/manifest.json"))

(define-easy-handler (favicon-avb
		      :uri (define-matching-functions "/favicon.ico" *avb-host*)
		      :host *avb-host*) ()
  (setf (content-type*) "image/vnd.microsoft.icon")
  (setf (header-out "content-disposition") "inline; filename=favicon.ico")
  (ninx:read-binary-file-to-octets #p"~/common-lisp/ninx/priv/pageone.ninx/static/icons/web/favicon.ico"))

;; this returns image data, 10 images are read onto a page, starting with page=1
(define-easy-handler (get-images-route-avb
		      :uri (define-matching-functions "^/get-images$" *avb-host*)
		      :host *avb-host*)
    (page)
  (incr-image-requests)
  (setf (header-out "access-control-allow-origin") "*")
  (let ((papers (get-images (cond ((stringp page) (parse-integer page)) (t page)))))
    (setf (content-type*) "application/json")
    ;; when we have < 10 papers, we have reached the end, don't send a timestamp, other send the timestamp of the 10th.
    (jzon:stringify
     (if papers
	 (hash-create (list (list "data" (loop for paper in papers
					       collect
					       (hash-create (list (list "name" (second paper))
								  (list "id" (first paper))
								  (list "date" (third paper))))))))
	 (hash-create (list (list "data" nil)))))))

(define-easy-handler (get-image-route-avb
		      :uri (define-matching-functions "^/get-image$" *avb-host*)
		      :host *avb-host*)
    (id)
  (incr-image-downloads)
  (setf (header-out "access-control-allow-origin") "*")
  (let ((image-data (get-image-data id)))
    (setf (content-type*) (caar image-data))
    (cadar image-data)))

(define-easy-handler (realtime-analytics-avb :uri (define-matching-functions "^/realtime-analytics$" *avb-host*)
					 :acceptor-names '(ninx::ninx)
					 :host *avb-host*) ()
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
	     (:table
		 (:tr (:th "Requests in Time") (:th "Number") (:th "Growth"))
	       (:tr (:td "Day") (:td (str (get-image-requests :duration 1))) (:td (str (format nil "~,7f"
										     (compute-growth
										      (get-image-requests :duration 2)
										      (get-image-requests :duration 1))))))
	       (:tr (:td "Week") (:td (str (get-image-requests :duration 7))) (:td (str (format nil "~,7f"
												(compute-growth
												 (get-image-requests :duration 14)
												 (get-image-requests :duration 7))))))
	       (:tr (:td "Month") (:td (str (get-image-requests :duration 28))) (:td (str (format nil "~,7f"
												  (compute-growth
												   (get-image-requests :duration 56)
												   (get-image-requests :duration 28))))))
	       (:table
		   (:tr (:th "Downloads in Time") (:th "Number") (:th "Growth"))
		 (:tr (:td "Day") (:td (str (get-image-downloads :duration 1))) (:td (str (format nil "~,7f"
											(compute-growth
											 (get-image-downloads :duration 2)
											 (get-image-downloads :duration 1))))))
		 (:tr (:td "Week") (:td (str (get-image-downloads :duration 7))) (:td (str (format nil "~,7f"
											 (compute-growth
											  (get-image-downloads :duration 14)
											  (get-image-downloads :duration 7))))))
		 (:tr (:td "Month") (:td (str (get-image-downloads :duration 28))) (:td (str (format nil "~,7f"
											   (compute-growth
											    (get-image-downloads :duration 56)
											    (get-image-requests :duration 28))))))))))))))


;; start the scrap job

(with-context (*actors*)
  (when *hourly-scrap-p*
    (task-start #'daily-scrap)))
