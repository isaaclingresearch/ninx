(defpackage :cenna.llms
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

(defpackage :cenna
  (:use :cl :ninx :postmodern :fiveam :local-time :hunchentoot :hunchensocket :cl-hash-util :com.inuoe.jzon :trivia :cl-ppcre :frugal-uuid :flexi-streams :cenna.llms)
  (:import-from :drakma :http-request)
  (:shadow hunchentoot:reply str:match str:split cl)
  (:local-nicknames (:jzon :com.inuoe.jzon) (:ppcre :cl-ppcre)))
