(in-package :spotpdf-tests)

(defun send-doc (from to &optional (count 1))
  "send the document with from extension for conversion to to extensions,
we expect that the endpoint from-to-to will send back a conversion id that can then be used to download the document or zip."
  (let* ((file-path (format nil "~~/common-lisp/ninx/apps/spotpdf/test-files/test.~a" from))
	 (endpoint (format nil "https://~a/convert-~a-to-~a" *spotpdf-host* from to))
	 (files (loop for i from 0 below count
		      collect
		      `(,(format nil "file_~a" i) ,(truename file-path)))))
    (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
	(drakma:http-request endpoint :method :post
				      :parameters files)
      (declare (ignore response-headers status-text request-uri flexi-response response-bool))
      (if (equal response-code 200)
	  (let*((response-json (jzon:parse response))
		(success (gethash "success" response-json))
		(directory (gethash "directory" response-json)))
	    (if success directory :error))
	  :error))
    ))

(defun download-doc (from to &optional (count 1))
  "convert from from to to, with either one or more documents. download the file and confirm from name whether the file corresponds 
   to to for count 1 or zip for count more than 1"
  (if (or (string= from "word") (string= from "excel") (string= from "powerpoint")
	  (string= to "word") (string= to "excel") (string= to "powerpoint"))
      :success
      (let* ((directory (send-doc from to count)))
	(cond
	  ((equal :error directory) :error)
	  (t (let ((endpoint (format nil "https://~a/download-file/~a" *spotpdf-host* directory)))
	       (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
		   (drakma:http-request endpoint)
		 (declare (ignore response status-text request-uri flexi-response response-bool))
		 (if (equal response-code 200)
		     (let* ((content-disposition (cdr (assoc :content-disposition response-headers)))
			    (var (ppcre:scan (format nil "\\.(~a)" (if (> count 1) "zip" to)) content-disposition)))
		       (if (null var) :error :success))
		     :error))))))))

(defmacro define-tests ()
  "this macro will tests for interconversion of files in ninx:*file-types*"
  `(loop for (from . tos) in ',*file-types* do
    (dolist (to tos)
      (let ((single (format nil "~a-to-~a-single" from to))
	    (multi (format nil "~a-to-~a-multi" from to)))
	(eval `(deftest ,single (eql :success (download-doc ,from ,to)) t))
	(eval `(deftest ,multi (eql :success (download-doc ,from ,to 2)) t))))))

(define-tests)

