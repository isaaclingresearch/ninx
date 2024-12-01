(in-package :spotpdf)

(defvar *spotpdf-host* (let ((port (uiop:getenv "NINX_HTTPS_PORT"))
			       (host (uiop:getenv "SPOTPDF_HOST")))
			   (if (equal "443" port)
			       host
			       (format nil "~a:~a" host port))))

(defparameter *cleanup-timer* nil)

(defun start-spotpdf (&key (schedule-cleanup t))
  "setup the required functions to run the application."
  (setq *cleanup-timer* (schedule-cleanup)))

(defun stop-spotpdf ()
  "stop the timer for cleanup"
  (sb-ext:unschedule-timer *cleanup-timer*))

(defun restart-spotpdf (&key (schedule-cleanup t))
  (stop-spotpdf :schedule-cleanup schedule-cleanup)
  (start-spotpdf))
