(defpackage :goodpdf
  (:use :cl :ninx :hunchentoot :hunchensocket :cl-who :cl-css :cl-base64 :frugal-uuid :trivia :com.inuoe.jzon :parenscript :paren-async :paren6 :local-time :cl-hash-util :zstd :cl-ppcre :cl-smtp :sb-rt :py4cl2 :sb-alien :cl-mime-from-string :sb-ext :zip)
  (:shadow hunchentoot:reply parenscript:@ parenscript:stringify parenscript:% cl-who:fmt str:match parenscript:chain sb-ext:create)
  (:documentation "The main package for ilovepdf clone.")
  (:local-nicknames (:jzon :com.inuoe.jzon) (:ppcre :cl-ppcre))
  (:export :start-goodpdf :stop-goodpdf :handle-ws-message))
