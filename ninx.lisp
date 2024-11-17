(defpackage :ninx
  (:use :cl :hunchentoot :cl-who :cl-css :easy-routes)
  (:shadow easy-routes:redirect)
  (:export :start-server :restart-server))

(in-package :ninx)

;;; HTTP(S) 
(setq *show-lisp-errors-p* t) ;; set this to show error files in /priv/errors

;; define server config
;;;; these are set in $HOME/.bashrc to be accessible in the sbcl repl 
(defvar *ninx-http-port* (parse-integer (uiop:getenv "NINX_HTTP_PORT")))
(defvar *ninx-https-port* (parse-integer (uiop:getenv "NINX_HTTPS_PORT")))
(defvar *ninx-ssl-cert* (uiop:getenv "NINX_SSL_CERT"))
(defvar *ninx-ssl-key* (uiop:getenv "NINX_SSL_KEY"))
(defvar *ninx-url* (uiop:getenv "NINX_HOST"))


;; redirect all traffic to https
(defclass http-to-https-acceptor (hunchentoot:acceptor) ())
(defmethod hunchentoot:acceptor-dispatch-request ((acceptor http-to-https-acceptor) request)
  (hunchentoot:redirect (hunchentoot:request-uri request)
                        :protocol :https :port *ninx-https-port*))

(defvar *ninx-ssl-acceptor* (make-instance 'routes-ssl-acceptor :port *ninx-https-port*
								     :ssl-certificate-file *ninx-ssl-cert*
								     :ssl-privatekey-file *ninx-ssl-key*
								     :document-root (truename "~/common-lisp/ninx/priv/")
								 ;    :error-template-directory (truename "~/common-lisp/ninx/priv/errors/")
								     ))

(defvar *ninx-http-acceptor* (make-instance 'http-to-https-acceptor :port *ninx-http-port*))

;; don't allow persistent connections
;; this is because the server was not responding to requests, with a 503, and the error logs were showing too many threads.
;; still investigation, but maybe the connections were sending a keep alive header.
(setf (acceptor-persistent-connections-p *ninx-http-acceptor*) nil)
(setf (acceptor-persistent-connections-p *ninx-ssl-acceptor*) nil)

;; after reviewing the taskmaster section of the docs, either of two things happened, because i was having one active connections
;; 1). the connections persisted, I don't why that is, but i have stopped persistent connections.
;; 2). The taskmaster ran out of threads, or the max accept was exceeded by the active requests.
;; 3). this is the solution, stop persistent connections above, then increase the threads to 1000, and max accept to 1500.

(let ((http-taskmaster (slot-value *ninx-http-acceptor* 'taskmaster))
      (https-taskmaster (slot-value *ninx-ssl-acceptor* 'taskmaster)))
  (setf (slot-value http-taskmaster 'hunchentoot::max-thread-count) 10000)
  (setf (slot-value http-taskmaster 'hunchentoot::max-accept-count) 15000)
  (setf (slot-value https-taskmaster 'hunchentoot::max-thread-count) 10000)
  (setf (slot-value https-taskmaster 'hunchentoot::max-accept-count) 15000))

(defun start-server (&optional (log-to-file t))
  "Start the server, when the server is started, we schedule a timer that will fetch paypal transaction data for the last 2 days every minute.
2 days because we don't know the timezone we will hit and so to avoid any problems, we set a two day gap."
  (hunchentoot:start *ninx-http-acceptor*)
  (hunchentoot:start *ninx-ssl-acceptor*)  (if log-to-file
      (progn ;; set logging to files
	(setf (acceptor-message-log-destination *ninx-ssl-acceptor*) (truename "~/common-lisp/ninx/logs/message.log"))
	(setf (acceptor-access-log-destination *ninx-ssl-acceptor*) (truename "~/common-lisp/ninx/logs/access.log")))
      (progn ;; set logging to files
	(setf (acceptor-message-log-destination *ninx-ssl-acceptor*) *terminal-io*)
	(setf (acceptor-access-log-destination *ninx-ssl-acceptor*) *terminal-io*))))

(defun stop-server ()
  "Stop the server"
  (when (started-p *ninx-http-acceptor*)
    (stop *ninx-http-acceptor*))
  (when (started-p *ninx-ssl-acceptor*)
    (stop *ninx-ssl-acceptor*)))

(defun restart-server (&optional (log-to-file t))
  (stop-server)
  (start-server log-to-file))


(defun home-css ()
  (cl-css:css
   `((body :margin 40px :font-family "Roboto, san-serif")
     (a :text-decoration none :color blue)
     ("a:visited" :text-decoration none :color blue))))

(defroute index ("/") ()
  (with-html-output-to-string (*standard-output*)
    "<!DOCTYPE html>"
    (:html :lang "en"
	   (:head
            (:title "Ninx Technology Limited")
            (:meta :charset "UTF-8")
            (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
            (:meta :name "description" :content "The accounts page for DeckLM")
            (:link :rel "icon" :href "/static/icons/web/favicon.ico" :sizes "any")
            (:link :rel "apple-touch-icon" :href "/static/icons/web/apple-touch-icon.png")
            (:style (str (home-css)))
	    (:link :href "https://fonts.googleapis.com/css?family=Roboto&display=swap" :rel "stylesheet")
	    )
	   (:body
	    (:h1 (:a :href "/" "Ninx Technology Limited."))
	    (:p "Ninx Technology Limited (Ninxtech) is studying how advances in technology in health, education and agriculture can be applied and spread to all humans. We are engineers, doctors and teachers building software, researching and applying ideas from marketing and spread of technologies to all our products.")
	    (:h2 "Team")
	    (:p "Lubwama Abdul Muswawir!")

	    (:p "Contact us at " (:a :href "mailto:info@ninx.xyz" "Info@ninx.xyz"))
	    (:h2 "Products")
	    (:p (:a :href "https://decklm.com" "Decklm") " - Generate slides from your learning resources in minutes.")
	    :hr
	    "Ninx Technology Limited,"
	    :br
	    "Nabweru Road, Lugoba,"
	    :br
	    "Wakiso, Uganda."))))
