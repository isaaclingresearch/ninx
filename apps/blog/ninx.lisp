(defpackage :ninx-blog
  (:use :cl :ninx :hunchentoot :cl-who :sqlite :cl-pass :cl-html-parse :cl-css :cl-base64 :str :cl-ppcre :trivia)
  (:shadow cl-who:fmt str:match str:split)
  (:documentation "The main package of the website blog")
  (:export :start-server :restart-server :create-tables))

(in-package :ninx-blog)

(defvar *ninx-blog-host* (let ((port (uiop:getenv "NINX_HTTPS_PORT"))
			       (host (uiop:getenv "NINX_BLOG_HOST")))
			   (if (equal "443" port)
			       host
			       (format nil "~a:~a" host port))))

(defun get-current-year ()
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time (get-universal-time))
    year))

(defmacro site-template (title &key body description active)
  ;; this is the template for all html on the blog.
  ;; sends headers, bootstrap, icons, the header, footer and general layout of the pages.
  ;; the modifications are added the body argument of the macro
  ;; the title argument sends the title of the page
  ;; the active argument helps us to highlight the active link
  `(with-html-output-to-string	(*standard-output*)
     (htm (:html :lang "en"
		 (:head 
		  (:meta :charset "utf-8")
		  (:meta :name "description" :content (if ,description
							  ,description
							  "Ninx Blog"))
		  (:meta :http-equiv "x-ua-compatible" :content "ie=edge")
		  (:meta :name "viewport" :content "width=device-width, initial-scale=1")
		  (:link :rel "icon" :href "/blog-priv/favicons/favicon.ico" :sizes "any")
		  (:link :rel "apple-touch-icon" :href "/blog-priv/favicons/apple-touch-icon.png")
		  (:link :rel "manifest" :href "/blog-priv/favicons/manifest.json")

		  ;; microsoft clarity
		  (:script :type "text/javascript"
			   "(function(c,l,a,r,i,t,y){
				    c[a]=c[a]||function(){(c[a].q=c[a].q||[]).push(arguments)};
				    t=l.createElement(r);t.async=1;t.src=\"https://www.clarity.ms/tag/\"+i;
				    y=l.getElementsByTagName(r)[0];y.parentNode.insertBefore(t,y);
				    })(window, document, \"clarity\", \"script\", \"hh3t2vvd5n\");")
		  ;; google analytics
		  (:script :async t :src "https://www.googletagmanager.com/gtag/js?id=G-G5ZPQXQQ8Z")
		  (:script
		   "window.dataLayer = window.dataLayer || [];
		   function gtag(){dataLayer.push(arguments);}
		   gtag('js', new Date());
		   gtag('config', 'G-G5ZPQXQQ8Z');") 
		  (:title ,title)
		  (:link :rel "stylesheet" :href "/blog-priv/css/ninx.css")
		  (:style "body {line-height: 1.4; font-size: 16px; padding: 0 10px; margin: 50px auto; max-width: 650px; text-align: left; text-wrap: pretty;} a:visited {color: blue;}")
		  )
		 (:body (:div :class "main-container"
			      (:div (:h2 "Ninx Technology Limited")
				    (:b "P.O.Box 112999, Wakiso, Uganda.") (:br)
				    (:b "Mail us at: " (:a :href "mailto:info@ninx.xyz" "info@ninx.xyz") ".")
				    (:div :id "main" ,body))))))))

(defun check-admin-details (username password)
  "cheacks the provided password against saved password for username.
   returns boolean."
  (cl-pass:check-password password (caar (execute-to-list *db* "select password from admin where username = ?" username))))
