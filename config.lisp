(in-package :ninx)

;; easy-routes is incompatible with hunchentoot's easy-handler for using hosts, so we won't be using it.

;;; HTTP(S) 
(setq *show-lisp-errors-p* t) ;; set this to show error files in /priv/errors

;; define server config
;;;; these are set in $HOME/.bashrc to be accessible in the sbcl repl 
(defvar *ninx-http-port* (parse-integer (uiop:getenv "NINX_HTTP_PORT")))
(defvar *ninx-https-port* (parse-integer (uiop:getenv "NINX_HTTPS_PORT")))
(defvar *ninx-ssl-cert* (uiop:getenv "NINX_SSL_CERT"))
(defvar *ninx-ssl-key* (uiop:getenv "NINX_SSL_KEY"))


;; WEBSOCKET SERVER AND FUNCTIONS
(defclass ws-endpoint (hunchensocket:websocket-resource)
  ((name :initarg :name :initform (error "Name this server") :reader :name :accessor name))
  (:default-initargs :client-class 'ws-user))

(defclass ws-user (hunchensocket:websocket-client)
  ((name :initarg :user-agent :reader name :initform (error "Name this user!") :accessor name)))

;; all websocket endpoints will be defined here.
;; add a new instance for the endpoint you need.
;; add a type for every endpoint to use a generic function
(deftype decklm-ws-type () '(member |/ws/decklm|))

;; this generic will be specialised in all applications that wish to use websockets
(defgeneric handle-ws-message (app endpoint ws-user message-json))

(defvar *ws-endpoints* (list (make-instance 'ws-endpoint :name "/ws/decklm")))

(defun find-ws-endpoint (request)
  (find (hunchentoot:script-name request) *ws-endpoints* :test #'string= :key #'name))

(pushnew 'find-ws-endpoint hunchensocket:*websocket-dispatch-table*)


;; we need to use easy-routes over websockets, so we will create children of both
(defclass ws-acceptor (acceptor-websocket easy-acceptor)
  ()
  (:documentation "a subclass of acceptor and hunchensocket"))

(defclass ws-ssl-acceptor (websocket-ssl-acceptor easy-ssl-acceptor)
  ()
  (:documentation "routes and websockets over ssl"))


;; redirect all traffic to https
(defclass http-to-https-acceptor (hunchentoot:acceptor) ())
(defmethod hunchentoot:acceptor-dispatch-request ((acceptor http-to-https-acceptor) request)
  (hunchentoot:redirect (hunchentoot:request-uri request)
                        :protocol :https :port *ninx-https-port*))

(defvar *ninx-wss-acceptor* (make-instance 'ws-ssl-acceptor :port *ninx-https-port*
							    :name 'decklm
							    :ssl-certificate-file *ninx-ssl-cert*
							    :ssl-privatekey-file *ninx-ssl-key*
							    :document-root (truename "~/common-lisp/ninx/priv/")
							    :error-template-directory (truename "~/common-lisp/ninx/priv/errors/")))

(defvar *ninx-http-acceptor* (make-instance 'http-to-https-acceptor :port *ninx-http-port*))


;; don't allow persistent connections
;; this is because the server was not responding to requests, with a 503, and the error logs were showing too many threads.
;; still investigation, but maybe the connections were sending a keep alive header.
(setf (acceptor-persistent-connections-p *ninx-http-acceptor*) nil)
(setf (acceptor-persistent-connections-p *ninx-wss-acceptor*) nil)

;; after reviewing the taskmaster section of the docs, either of two things happened, because i was having one active connections
;; 1). the connections persisted, I don't why that is, but i have stopped persistent connections.
;; 2). The taskmaster ran out of threads, or the max accept was exceeded by the active requests.
;; 3). this is the solution, stop persistent connections above, then increase the threads to 1000, and max accept to 1500.

(let ((http-taskmaster (slot-value *ninx-http-acceptor* 'taskmaster))
      (https-taskmaster (slot-value *ninx-wss-acceptor* 'taskmaster)))
  (setf (slot-value http-taskmaster 'hunchentoot::max-thread-count) 10000)
  (setf (slot-value http-taskmaster 'hunchentoot::max-accept-count) 15000)
  (setf (slot-value https-taskmaster 'hunchentoot::max-thread-count) 10000)
  (setf (slot-value https-taskmaster 'hunchentoot::max-accept-count) 15000))
