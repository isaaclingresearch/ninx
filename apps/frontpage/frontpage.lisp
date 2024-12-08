(in-package :frontpage)

;; nation media papers

(defun get-yyyy-mm-dd (&optional yyyy mm dd)
  (if (and yyyy mm dd)
      (format nil "~a-~a-~a" yyyy mm dd)
      (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
	  (decode-universal-time (get-universal-time))
	(format nil "~a-~a-~a" year month date))))

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
	(format t "~%~a: success~%" yyyy-mm-dd)
	(format t "~a" (list :error response-code response-headers response)))))

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
	(format t "~%~a: success~%" yyyy-mm-dd)
	(format t "~a" (list :error response-code response-headers response)))))

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
	(format t "~%~a: success~%" yyyy-mm-dd)
	(format t "~a" (list :error response-code response-headers response)))))

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
	(format t "~%~a: success~%" yyyy-mm-dd)
	(format t "~a" (list :error response-code response-headers response)))))

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
    ((list year month _)
     (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
	 (drakma:http-request (format nil "https://epapers.visiongroup.co.ug/wp-content/uploads/~a/~a/Untitled-~a-~a.gif"
				      year month d1 d2)
			      :method :get
			      :user-agent :firefox
			      :additional-headers '(("referer" . "https://epaper.visiongroup.co.ug")))
       (declare (ignore status-text request-uri flexi-response response-bool))
       (if (equal response-code 200)
	   (format t "~%~a: success~%" (list year month d1 d2))
	   (format t "~a" (list :error response-code response-headers response)))))))

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

;; to be implemented later.
(defun get-observer (&optional (weeks 0))
  "observer is published weekly, on a wednesday and runs upto tuesday. 
   format is The-Observer-Month-Date-of-Wed-Date-of-Next-Tue-Year.jpg
   we don't know how the transition between months occurs. but i guess that's something we will learn upon months end.
   to implement this; we need to know where are in the week. if >= wednesday we use this wednesday and next tuesday. if not we use the
   wednesday of last week and the tuesday of last week. 
   for the previous weeks, we use the wednesday in a week and the tuesday in a week after."
  (let* ((current-timestamp (now))
	 (this-wednesday (chronicity:parse "this wednesday"))
	 (wednesday (if (timestamp> current-timestamp this-wednesday)
			(chronicity:parse "last wednesday")
			this-wednesday))
	 (this-tuesday (chronicity:parse "this tuesday"))
	 (tuesday (if timestamp> current-timestamp this-tuesd)))))

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
	  (format t "~%~a: success~%" url)
	  (format t "~a" (list :error response-code response-headers response))))))

(defun scrap-observer (&optional (weeks 0))
  "go back the given number of weeks"
  (when (>= weeks 0)
    (get-observer (- weeks))
    (scrap-observer (1- weeks))))
