(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defsystem "ninx"
  :author "Ninx technology limited"
  :description "This is a system for the company site, blog and all young applications that are not yet mature enough to have their own servers."
  :version "0.1.0"
  :depends-on (:postmodern :str :com.inuoe.jzon :hunchentoot :hunchensocket :drakma :cl-who :cl-css :cl-base64 :frugal-uuid :trivia :flexi-streams :parenscript :paren-async :paren6 :local-time :cl-hash-util :trivial-mimes :zstd :cl-ppcre :cl-smtp :sb-rt :py4cl2 :cl-maxminddb :sqlite :cl-html-parse :cl-pass :cl-mime-from-string :zippy :chronicity :sento :ironclad :fiveam :sento :cffi)
;; :cl-pdf :cl-pdf-parser :cl-typesetting
  :components ((:file "package")
	       (:file "config")
	       ;; (:module "apps/decklm/src"
	       ;; 	:components ((:file "package")
	       ;; 		     (:file "decklm.pdf")
	       ;; 		     (:file "decklm.apis")
	       ;; 		     (:file "decklm.paypal")
	       ;; 		     (:file "decklm.sexp")
	       ;; 		     (:file "decklm.nlp")
	       ;; 		     (:file "decklm.app")
	       ;; 		     (:file "decklm.prompts")
	       ;; 		     (:file "decklm.postgres")
	       ;; 		     (:file "decklm")
	       ;; 		     ))
	       (:module "apps/pageone/src"
		:components ((:file "package")
			     (:file "pageone")))
	       ;; (:module "apps/spotpdf"
	       ;; 	:components ((:file "package")
	       ;; 		     (:file "spotpdf")
	       ;; 		     (:file "spotpdf.app")
	       ;; 		     (:file "spotpdf.tests")))
	       (:module "apps/blog"
		:components ((:file "ninx")
			     (:file "blog")))
	       (:module "apps/ninx"
		:components ((:file "package")
			     (:file "ninx")))
	       (:module "apps/cenna/src"
			:components ((:file "package")
				     (:file "postgres.cenna")
				     (:file "cenna")))
	       (:file "server"))
  :build-operation "program-op" ;; leave as is
  :build-pathname "ninx"
  :entry-point "server:start-server")
