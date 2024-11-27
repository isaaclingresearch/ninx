(in-package :goodpdf)

(defvar *goodpdf-host* (let ((port (uiop:getenv "NINX_HTTPS_PORT"))
			       (host (uiop:getenv "GOODPDF_HOST")))
			   (if (equal "443" port)
			       host
			       (format nil "~a:~a" host port))))
