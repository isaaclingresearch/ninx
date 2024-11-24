(defpackage :decklm.sexp
  (:use :cl)
  (:nicknames :sexp)
  (:documentation "Processes strings and sexps")
  (:export :parse :stringify :ensure-plist :plistp :remove-encapsulation))

(defpackage :decklm.nlp
  (:use :cl :str :trivia)
  (:nicknames :nlp)
  (:shadow str:match)
  (:documentation "This package for processing text")
  (:export :remove-punctuation :remove-json-encapsulation :remove-lisp-encapsulation :make-strict-json :tokenize :count-terms :compute-tf))

(defpackage :decklm.cpdf
   (:use :cl :cl-typesetting :cl-pdf :trivia)
   (:documentation "This package handles creation of pdfs")
   (:nicknames :cpdf)
   (:shadow pdf:write-document pdf:initialize! pdf:image pdf:with-document)
   (:export :generate-pdf :test-table :test-simple-table))

(defpackage :decklm.paypal
   (:use :cl :drakma :com.inuoe.jzon :cl-base64 :local-time :cl-hash-util)
   (:nicknames :paypal)
   (:documentation "This package for making rest api calls to paypal")
   (:local-nicknames (:jzon :com.inuoe.jzon))
   (:export :get-transactions))

(defpackage :decklm.apis
  (:use :cl :drakma :com.inuoe.jzon :cl-hash-util :trivial-mimes)
  (:nicknames :llms)
  (:documentation "This package for making rest api calls to llm endpoints")
  (:local-nicknames (:jzon :com.inuoe.jzon))
  (:export :make-gemini-multiturns
   :query-gemini
	   :query-together
   :query-groq
	   :query-ollama
   :query-azure-ai
   *index-prompt*
   *system-prompt*
   *sexp-index-prompt*
   *sexp-title-prompt*
   *json-chat-prompt*
   *default-together-model*
   *qwen-system-prompt*))

(defpackage :decklm
  (:use :cl :ninx :hunchentoot :hunchensocket :cl-who :cl-css :cl-base64 :frugal-uuid :trivia :com.inuoe.jzon :parenscript :paren-async :paren6 :local-time :cl-hash-util :postmodern :zstd :paypal :cl-ppcre :cl-smtp :sb-rt :py4cl2 :nlp :sb-alien :cl-maxminddb)
  (:shadow hunchentoot:reply parenscript:@ parenscript:stringify parenscript:% cl-who:fmt str:match parenscript:chain)
  (:documentation "The main package of the makeitslides application.")
  (:local-nicknames (:jzon :com.inuoe.jzon) (:ppcre :cl-ppcre))
  (:export :start-decklm :stop-decklm :handle-ws-message))
