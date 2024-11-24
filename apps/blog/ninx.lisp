(defpackage :ninx-blog
  (:use :cl :hunchentoot :cl-who :sqlite :cl-pass :cl-html-parse :cl-css :cl-base64 :str :cl-ppcre :trivia)
  (:shadow cl-who:fmt str:match str:split)
  (:documentation "The main package of the website blog")
  (:export :start-server :restart-server :create-tables))

(in-package :ninx-blog)
(defvar *ninx-blog-host* (format nil "~a:~a" (uiop:getenv "NINX_BLOG_HOST") (uiop:getenv "NINX_HTTPS_PORT")))

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
		  (:link :rel "stylesheet" :href "/css/ninx.css")
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


(define-easy-handler (admin-login :uri "/admin/login" :host *ninx-blog-host*) ()
  ;; displays a form to login the admin
  (site-template "Ninx | Admin Login"
		 :body (with-html-output (*standard-output*)
			 (htm (:div :style (inline-css '(:text-align center))
				    (:form :method "post" :action "/admin/process-login"
					   (:h4 :class "" "Please sign in")
					   (:input :style (inline-css '(:width 100% :margin-bottom 2% :height 6%)) :type "text" :id "username" :placeholder "Username" :name "username")
					   (:input :style (inline-css '(:width 100% :height 6%)) :type "password" :id "password" :placeholder "Password" :name "password")
					   (:button :style (inline-css '(:width 50% :margin-top 1% :background-color "#ffd700" :height 6%)) "Login")
					   ))))))

(define-easy-handler (process-admin-login :uri "/admin/process-login"
					  :default-request-type :post
					  :host *ninx-blog-host*)
    (username password)
  ;; processes admin login, redirects if details are corrected, otherwise returns an error
  (if (cookie-in "na-cookie")
      (hunchentoot:redirect "/admin") 
      (site-template "Ninx | Admin Login"
		     :body (with-html-output (*standard-output*)
			     (if (check-admin-details username password)
				 (progn
				   (set-cookie* (set-cookie "na-cookie" :path "/" :value (string-to-base64-string username) :expires (+ (get-universal-time) (* 365 24 60 60))))
				   (hunchentoot:redirect "/admin"))
				 (htm (:div :class "container"
					    (:form :method "post" :action "/admin/process-login"
						   (:h1 :class "" "Please sign in")
						   (:h2 :class "" "Incorrect username/password")
						   (:div :class ""
							 (:label :class "form-label" :for "username" "Username")
							 (:input :class "form-control" :type "text" :id "username" :placeholder "Username" :name "username" :value (str username)))
						   (:div :class "mb-3"
							 (:label :class "form-label" :for "password" "Password")
							 (:input :class "form-control" :type "password" :id "password" :placeholder "Password" :name "password"))
						   (:button :class "" "Login")
						   )))
				 )))))


(define-easy-handler (admin-logout :uri "/admin/logout" :host *ninx-blog-host*) ()
  ;; logout admin, delete the cookie.
  (site-template "Ninx | Admin logout"
		 :body (progn (set-cookie* (set-cookie "na-cookie" :path "/" :value (cookie-in "na-cookie") :expires (- (get-universal-time) (* 2 365 24 60 60))))
			      (hunchentoot:redirect "/admin/login"))))

(define-easy-handler (admin-dashboard :uri "/admin" :host *ninx-blog-host*) ()
  ;; display the options for admin, create new post, edit drafts or logout.
  (if (null (cookie-in "na-cookie"))
      (hunchentoot:redirect "/admin/login")
      (site-template "Ninx | Admin dashboard"
		     :body (with-html-output (*standard-output*)
			     (htm (:div
				   (:br)
				   (let ((button-style '(:background-color \#ffd700 :margin "0 1% 0 1%" :height 7%))
					 (link-style '(:text-decoration none :color black)))
				     (htm
				      (:div 
				       (:button :style (inline-css `(,@button-style)) (:a :style (inline-css `(,@link-style)) :href (format nil "/blog/~a/create-blog-post" (base64-string-to-string (cookie-in "na-cookie"))) "Create new blog post"))
				       (:button :style (inline-css `(,@button-style :background-color blue)) (:a :style (inline-css `(,@link-style)) :href (format nil "/blog/~a/drafts" (base64-string-to-string (cookie-in "na-cookie"))) "Edit drafts"))
				       (:button :style (inline-css `(,@button-style :background-color red)) (:a :style (inline-css `(,@link-style)) :href "/admin/logout" "Logout"))
				       )))))
			     ))))
