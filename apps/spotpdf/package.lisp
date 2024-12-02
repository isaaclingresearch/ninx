(defpackage :spotpdf
  (:use :cl :ninx :hunchentoot :hunchensocket :cl-who :cl-css :cl-base64 :frugal-uuid :trivia :com.inuoe.jzon :parenscript :paren-async :paren6 :local-time :cl-hash-util :zstd :cl-ppcre :cl-smtp :sb-rt :py4cl2 :sb-alien :cl-mime-from-string :sb-ext :zip)
  (:shadow hunchentoot:reply parenscript:@ parenscript:stringify parenscript:% cl-who:fmt str:match parenscript:chain sb-ext:create)
  (:documentation "The main package for ilovepdf clone.")
  (:local-nicknames (:jzon :com.inuoe.jzon) (:ppcre :cl-ppcre))
  (:export :start-spotpdf :stop-spotpdf :handle-ws-message *file-types* *image-types* *spotpdf-host*))

(defpackage :spotpdf-tests
  (:use :cl :ninx :spotpdf :com.inuoe.jzon :cl-ppcre :sb-rt)
  (:documentation "Tests for all the conversion types")
  (:local-nicknames (:jzon :com.inuoe.jzon) (:ppcre :cl-ppcre)))
