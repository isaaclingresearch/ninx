;; all application code that uses the server will be defined in this package.
(defpackage :ninx
  (:use :cl :hunchentoot :hunchensocket :cl-who :cl-css :cl-base64 :frugal-uuid :trivia :com.inuoe.jzon :parenscript :paren-async :paren6 :local-time :cl-hash-util :postmodern :zstd :cl-ppcre :cl-smtp :sb-rt :py4cl2 :sb-alien :cl-maxminddb :sb-rt)
  (:shadow hunchentoot:reply parenscript:@ parenscript:stringify parenscript:% cl-who:fmt str:match parenscript:chain)
  (:documentation "The container for all the applications")
  (:local-nicknames (:jzon :com.inuoe.jzon) (:ppcre :cl-ppcre))
  (:export  :handle-ws-message
	    *ninx-http-port* *ninx-https-port* *ninx-ssl-key* *ninx-ssl-cert* *ninx-wss-acceptor* *ninx-http-acceptor*
	    ;; ws types
	    decklm-ws-type 
	    ws-endpoint
   :match-path :define-matching-functions
   :get-current-year :get-dd-mm-yyyy
   :read-binary-file-to-octets
   :write-octets-to-binary-file
   :acceptor-server-name
   :is-mobile-browser
   :compute-growth))
