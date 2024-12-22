(defpackage :server
  (:use :cl :ninx hunchentoot :hunchensocket :ninx-app :ninx-blog :cenna);; :pageone ;; :decklm :spotpdf
  (:documentation "The container for all the applications")
  (:local-nicknames (:jzon :com.inuoe.jzon) (:ppcre :cl-ppcre))
  (:export :start-server :restart-server :stop-server))

(in-package :server)

(defun start-server (&key (log-to-file t) (schedule-payments t) (schedule-cleanup t))
  "Start the server, when the server is started, we schedule a timer that will fetch paypal transaction data for the last 2 days every minute.
2 days because we don't know the timezone we will hit and so to avoid any problems, we set a two day gap. we use keys such that we just add 
application specific options"
  (hunchentoot:start ninx:*ninx-http-acceptor*)
  (hunchentoot:start ninx:*ninx-wss-acceptor*)
  (if log-to-file
      (progn ;; set logging to files
	(setf (acceptor-message-log-destination ninx:*ninx-wss-acceptor*) (truename "~/common-lisp/ninx/logs/message.log"))
	(setf (acceptor-access-log-destination ninx:*ninx-wss-acceptor*) (truename "~/common-lisp/ninx/logs/access.log")))
      (progn ;; set logging to files
	(setf (acceptor-message-log-destination ninx:*ninx-wss-acceptor*) *terminal-io*)
	(setf (acceptor-access-log-destination ninx:*ninx-wss-acceptor*) *terminal-io*)))
  ;; start application specific components
;;  (start-decklm :schedule-payments schedule-payments)
 ;; (spotpdf:start-spotpdf :schedule-cleanup schedule-cleanup)
  )

(defun stop-server ()
  "Stop the server"
  (when (started-p ninx:*ninx-http-acceptor*)
    (stop ninx:*ninx-http-acceptor*))
  (when (started-p ninx:*ninx-wss-acceptor*)
    (stop ninx:*ninx-wss-acceptor*))
  ;; stop application specific code
;;  (stop-decklm)
;;  (spotpdf:stop-spotpdf)
  )

(defun restart-server (&key (log-to-file t) (schedule-payments t) (schedule-cleanup t))
  (stop-server)
  (start-server :log-to-file log-to-file :schedule-payments schedule-payments :schedule-cleanup schedule-cleanup))

;; websocket methods to handle communication via websocket
(defmethod hunchensocket:client-connected ((endpoint ws-endpoint) ws-user))

;; the messages will be handled by the methods in respective applications.
(defmethod hunchensocket:text-message-received ((endpoint ws-endpoint) ws-user message-json)
  (trivia:match (ninx::name endpoint)
		(_ t)
;;    ("/ws/decklm" (decklm::ws-decklm endpoint ws-user message-json))
		))

(defun start-test-server ()
  (start-server :log-to-file nil :schedule-payments nil :schedule-cleanup nil))

(defun restart-test-server ()
  (stop-server)
  (start-test-server))
