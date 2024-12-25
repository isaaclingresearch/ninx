(defsystem "fdb"
  :author "Ninx technology limited"
  :description "CFFI client for Foundationdb"
  :version "0.1.0"
  :depends-on (:cffi)
;; :cl-pdf :cl-pdf-parser :cl-typesetting
  :components ((:file "package")
	       (:file "c_api")
	       (:file "base"))
  :build-operation "program-op" ;; leave as is
  :build-pathname "ninx"
  :entry-point "fdb:start-server")
