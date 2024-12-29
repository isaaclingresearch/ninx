(in-package :cenna)

(defvar *cenna-url* (uiop:getenv "CENNA_HOST"))
(defvar *cenna-host* (let ((host (uiop:getenv "CENNA_HOST")))
			 (if (equal "443" ninx:*ninx-https-port*)
			     host
			     (format nil "~a:~a" host ninx:*ninx-https-port*))))
