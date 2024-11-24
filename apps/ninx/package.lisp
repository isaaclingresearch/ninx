;; all application code that uses the server will be defined in this package.
(defpackage :ninx-app
  (:use :cl :hunchentoot :hunchensocket :cl-who :cl-css )
  (:documentation "The company website"))
