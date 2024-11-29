(in-package :spotpdf)

(defvar *spotpdf-host* (let ((port (uiop:getenv "NINX_HTTPS_PORT"))
			       (host (uiop:getenv "SPOTPDF_HOST")))
			   (if (equal "443" port)
			       host
			       (format nil "~a:~a" host port))))
