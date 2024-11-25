;; this file contains the code for the blog of the company and my personal blog.
;; i found it interesting to separate the code of the blog from the main site.

(in-package :ninx-blog)

;; defines the sqlite3 database used by the application.
(defparameter *db* (connect  (truename "~/common-lisp/ninx/apps/blog/db/ninx.db")))


;; DB ACCESS FUNCTIONS

(defun create-tables ()
  "Create the ninx db tables
   creates two tables: blog for storing blog posts and admin for storing admin credentials"
  (execute-non-query *db* "create table blog (id INTEGER PRIMARY KEY, title TEXT, url TEXT UNIQUE, draft BOOLEAN, author TEXT, timestamp DEFAULT CURRENT_TIMESTAMP, essay TEXT)")
  (execute-non-query *db* "create table admin (id INTEGER PRIMARY KEY, username TEXT, password TEXT, timestamp DEFAULT CURRENT_TIMESTAMP, full_name TEXT)"))

(defun get-blog-post (blog-url)
  "Get a blog post using it's url"
  (execute-to-list *db* "select title, draft, author, essay, timestamp from blog where url = ?" blog-url))

(defun get-all-blog-posts ()
  "Get all blog posts from the blog table"
  (execute-to-list *db* "select title, url, draft, author, essay, timestamp from blog where draft = 'false'"))

(defun get-all-blog-posts-by-author (author)
  "Get all blog posts from the blog table for a particular author"
  (execute-to-list *db* "select title, url, draft, author, essay, timestamp from blog where draft = 'false' and author = ?" author))

(defun get-all-draft-blog-posts ()
  "Get all blog posts from the blog table"
  (execute-to-list *db* "select title, url, draft, author, essay, timestamp from blog where draft = 'true'"))

(defun get-all-draft-blog-posts-by-author (author)
  "Get all blog posts from the blog table for a particular author"
  (execute-to-list *db* "select title, url, draft, author, essay, timestamp from blog where draft = 'true' and author = ?" author))

(defun add-admin (username password full-name)
  "add an admin to the admin table, password is encrypted with cl-pass:hash"
  (execute-non-query *db* "insert into admin (username, password, full_name) values (?, ?, ?)" username (cl-pass:hash password) full-name))

(defun update-admin-password (username password)
  "change the password of an admin"
  (execute-non-query *db* "update admin set password = ? where username = ?" (cl-pass:hash password) username))

(defun get-all-admins ()
  "get all the credentials of admins in the admin table."
  (execute-to-list *db* "select * from admin"))

(defun get-admin-full-name (username)
  (execute-single *db* "select full_name from admin where username = ?" username))

(defun make-description-meta (blog-url)
  "create a snippet of the actual page to be displayed as a meta description"
  (let ((post-list (get-blog-post blog-url)))
    (if post-list
	(let ((post (car post-list)))
	  (if (> 157 (length (cadddr post)))
	      (cadddr post)
	      (concatenate 'string (subseq (cadddr post) 0 157) "..."))))))

(define-easy-handler (drafts-page :uri (lambda (request)
					 (trivia:match
					     (str:split "/" (script-name* request))
					   ((list "" "blog" _ "drafts") t)
					   (_ nil)))
				  :host *ninx-blog-host*) ()
  (if (null (cookie-in "na-cookie"))
      (redirect "/admin/login"))
  (trivia:match
      (str:split "/" (script-name*))
    ((list "" "blog" user "drafts")
     ;; displays a list of draft essays; a draft is an essay that is not yet completed.
     (with-html-output-to-string (*standard-output*)
       (htm (:html :lang "en"
		   (:head (:meta :charset "utf-8")
			  (:meta :http-equiv "x-ua-compatible" :content "ie=edge")
			  (:meta :name "viewport" :content "width=device-width, initial-scale=1")
			  (:title "Ninx | Essays")
			  (:style "body {line-height: 1.4; font-size: 16px; padding: 0 10px; margin: 50px auto; max-width: 650px; text-align: left; text-wrap: pretty;}")
			  (:link :rel "icon" :href "/blog-priv/favicons/favicon.ico" :sizes "any")
			  (:link :rel "apple-touch-icon" :href "/blog-priv/favicons/apple-touch-icon.png")
			  (:link :rel "manifest" :href "/blog-priv/favicons/manifest.json")
			  )
		   (let ((posts (get-all-draft-blog-posts-by-author (get-admin-full-name user))))
		     (if posts
			 (loop for post in posts
			       collect (htm (:article :class "" :style "margin-bottom: 4rem; white-space: text-wrap"
						      (:h2 :class "" (str (car post)))
						      (:p :style "margin-bottom: 1.25rem; color: #727272;"
							  "Added on " (str (nth 5 post))
							  " Written by "
							  (:a :href "#" (str (cadddr post))))
						      (if (> 500 (length (nth 4 post)))
							  (htm  (:p :class "blog-post" (str (nth 4 post))))
							  (htm  (:p :class "blog-post"
								    (str (subseq (nth 4 post) 0 500))
								    "... "
								    (:a :style (inline-css '(:color blue)):href (str (format nil "/blog/~a/~a" user (cadr post))) "Continue"))))
						      (if (and (not (null (cookie-in "na-cookie"))) (equal user (base64-string-to-string (cookie-in "na-cookie")))) 
							  (htm (:div
								(:div
								 (:a :style (inline-css '(:color blue)) :href (str (format nil "/blog/~a/update-blog-post/~a" user (cadr post))) "Edit Post"))
								(:div
								 (:a :style (inline-css '(:color blue)) :href (str (format nil "/blog/~a/save-blog-as-draft/~a" user (cadr post))) "Save as Draft"))
								)))
						      )))
			 (htm (:p "There are no draft blog posts"))))))))))

(define-easy-handler (view-draft
		      :uri (lambda (request)
					 (trivia:match
					     (str:split "/" (script-name* request))
					   ((list "" "blog" _ "view-draft" _) t)
					   (_ nil)))
		      :host *ninx-blog-host*) ()
  (trivia:match
      (str:split "/" (script-name*))
    ((list "" "blog" user "view-draft" blog-url)
     ;; displays a draft for reviewing.
     ;; same as above but works for only drafts.
     (site-template
      "Ninx | View Draft"
      :active :blog
      :body (let ((post-list (get-blog-post blog-url)))
	      (if post-list
		  (let ((post (car post-list)))
		    (with-html-output (*standard-output*)
		      (htm (:article :class "" :style "margin-bottom: 4rem; white-space: text-wrap"
				     (:h2 :class "" (str (car post)))
				     (:p :style "margin-bottom: 1.25rem; color: #727272;"
					 "Added on " (str (nth 4 post))
					 " Written by "
					 (:a :href "#" (str (caddr post))))
				     (htm  (:p :class "text-justify" :style "white-space: pre-wrap;" (str (cadddr post))))			 
				     ))))
		  (with-html-output (*standard-output*)
		    (htm (:p "There are no blog posts")))))
      :description (make-description-meta blog-url)))))


(define-easy-handler (create-blog-post
		      :host *ninx-blog-host*
		      :uri (lambda (request)
					 (trivia:match
					     (str:split "/" (script-name* request))
					   ((list "" "blog" _ "create-blog-post") t)
					   (_ nil)))) ()
  (if (null (cookie-in "na-cookie"))
      (redirect "/admin/login")
      (trivia:match
	  (str:split "/" (script-name*))
	((list "" "blog" user "create-blog-post")
	 ;; display a form to create new blog post.
	 (site-template "Ninx | Blog"
			:body (with-html-output (*standard-output*)
				(htm (:div :style (inline-css '(:width 80% :margin-left 10%))
					   (let ((label-css '(:width 100% :margin "0 0 1% 0" :height 6% :padding 5px)))
					     (htm
					      (:form :method "post"
						     (:input :style (inline-css `(,@label-css)) :id "essay-title" :type "text" :required t :name "title" :placeholder "Title")
						     (:input :style (inline-css `(,@label-css)) :id "url" :type "text" :required t :name "url" :placeholder "Url")
						     (:input :style (inline-css `(,@label-css)) :id "author" :type "text" :required t :name "author" :placeholder (get-admin-full-name (base64-string-to-string (cookie-in "na-cookie"))))
						     (:textarea :style (inline-css '(:width 100% :margin "0 0 1% 0" :padding 5px :height 20%)) :id "essay" :required t :name "essay" :placeholder "Essay here.")
						     (:button :style (inline-css '(:width 40% :height 6% :float left :background-color green)) :formaction (format nil "/blog/~a/save-blog-post" user) "Create Essay")
						     (:button :style (inline-css '(:width 40% :height 6% :float right :background-color blue)) :formaction (format nil "/blog/~a/save-blog-post?draft=true" user) "Save as draft")
						     )))))
				))))))

(define-easy-handler (update-blog-post
		      :host *ninx-blog-host*
		      :uri (lambda (request)
					 (trivia:match
					     (str:split "/" (script-name* request))
					   ((list "" "blog" _ "update-blog-post" _) t)
					   (_ nil)))) ()
  (if (null (cookie-in "na-cookie"))
      (redirect "/admin/login")
      (trivia:match
	  (str:split "/" (script-name*))
	((list "" "blog" user "update-blog-post" blog-url)
	 ;; display a form having already saved form details.
	 (site-template "Ninx | Blog"
			:body (with-html-output (*standard-output*)
				(let ((blog-post (car (get-blog-post blog-url))))
				  (if (null blog-post)
				      (htm (:p "Blog post not found"))
				      (htm (:div :style (inline-css '(:width 80% :margin-left 10%))
						 (let ((label-css '(:width 100% :margin "0 0 1% 0" :height 6% :padding 5px)))
						   (htm
						    (:form :method "post"
							   (:input :style (inline-css `(,@label-css)) :id "essay-title" :type "text" :required t :name "title" :placeholder "Title"
								   :value (str (car blog-post)))
							   (:input :style (inline-css `(,@label-css)) :id "url" :type "text" :required t :name "url" :placeholder "Url"
								   :value (str blog-url))
							   (:input :style (inline-css `(,@label-css)) :id "author" :type "text" :required t :name "author" :placeholder "Author"
								   :value (str (caddr blog-post)))
							   (:textarea :style (inline-css '(:width 100% :margin "0 0 1% 0" :padding 5px :height 20%)) :id "essay" :required t :name "essay" :placeholder "Essay here."
								      (str  (cadddr blog-post)))
							   (:button :style (inline-css '(:width 30% :height 6% :float left :background-color green)) :formaction (format nil "/blog/~a/edit-blog-post/~a" user blog-url) "Edit Essay")
							   (:button :style (inline-css '(:width 30% :height 6% :margin-left 5% :background-color blue)) :formaction (format nil "/blog/~a/edit-blog-post/~a?draft=true" user blog-url) "Save as draft")
							   (:button :style (inline-css '(:width 30% :height 6% :float right :background-color red :text-align center none :color black)) (:a :style (inline-css '(:text-decoration none :color black)) :href (str (format nil "/blog/~a/delete-blog-post/~a" user blog-url)) "Delete post"))
							   ))))
					   ))
				  )))))))

(defun save-blog-post-to-db (url title draft author essay)
  "Save the blog essay to the database"
  (execute-non-query *db* "insert into blog (title, url, draft, author, essay) values (?, ?, ?, ?, ?)" title url draft author essay))

(defun edit-blog-post-to-db (blog-url url title draft author essay)
  "Save the edited blog essay to the database. The url must remain constant for now"
  (execute-non-query *db* "update blog SET url = ?, title = ?, draft = ?, author = ?, essay = ? where url = ?" url title draft author essay blog-url))

(defun save-blog-as-draft-to-db (blog-url)
  (execute-non-query *db* "update blog set draft = 'true' where url = ?" blog-url))

(defun delete-blog-post-from-db (blog-url)
  "delete a blog post from a database"
  (execute-non-query *db* "delete from blog where url = ?" blog-url))

(define-easy-handler (save-blog-post
		      :default-request-type :post
		      :host *ninx-blog-host*
		      :uri (lambda (request)
					 (trivia:match
					     (str:split "/" (script-name* request))
					   ((list "" "blog" _ "save-blog-post") t)
					   (_ nil))))
    (draft title url author essay)
  (if (null (cookie-in "na-cookie"))
      (redirect "/admin/login")
      (trivia:match
	  (str:split "/" (script-name*))
	((list "" "blog" user "save-blog-post")	 
	 ;; endpoint for saving the blog post.
	 (save-blog-post-to-db url title (if draft "true" "false") author essay))
	(site-template "Ninx | Save blog"
		       :body (with-html-output (*standard-output*)
			       (htm (:p "The blog entry has been saved.")))))))

(define-easy-handler (edit-blog-post
		      :default-request-type :post
		      :host *ninx-blog-host*
		      :uri (lambda (request)
					 (trivia:match
					     (str:split "/" (script-name* request))
					   ((list "" "blog" _ "edit-blog-post" _) t)
					   (_ nil))))
    (draft title url author essay)
  (if (null (cookie-in "na-cookie"))
      (redirect "/admin/login")
      (trivia:match
	  (str:split "/" (script-name*))
	((list "" "blog" user "edit-blog-post" blog-url)
	 ;; endpoint for saving edited blog post
	 (edit-blog-post-to-db blog-url url title (if draft "true" "false") author essay)
	 (site-template "Ninx | Save blog"
			:body (with-html-output (*standard-output*)
				(htm (:p (str (if draft "The blog entry has been saved as a draft." "The blog entry has been saved."))))))))))

(define-easy-handler (save-blog-as-draft
		      :host *ninx-blog-host*
		      :uri (lambda (request)
					 (trivia:match
					     (str:split "/" (script-name* request))
					   ((list "" "blog" _ "save-blog-post-as-draft" _) t)
					   (_ nil)))) ()
  (if (null (cookie-in "na-cookie"))
      (redirect "/admin/login")
      (trivia:match
	  (str:split "/" (script-name*))
	((list "" "blog" user "save-blog-post-as-draft" blog-url)
	 ;; endpoint for saving blog as a draft
	 (save-blog-as-draft-to-db blog-url)
	 (site-template "Ninx | Blog"
			:body (with-html-output (*standard-output*)
				(htm (:p "The blog has been saved as a draft."))))))))

(define-easy-handler (delete-blog-post
		      :host *ninx-blog-host*
		      :uri (lambda (request)
					 (trivia:match
					     (str:split "/" (script-name* request))
					   ((list "" "blog" _ "delete-blog-post" _) t)
					   (_ nil)))) ()
  (if (null (cookie-in "na-cookie"))
      (redirect "/admin/login")
      (trivia:match
	  (str:split "/" (script-name*))
	((list "" "blog" user "delete-blog-post" blog-url)
	 ;; endpoint for deleting the blog
	 (delete-blog-post-from-db blog-url)
	 (site-template "Ninx | Blog"
			:body (with-html-output (*standard-output*)
				(htm (:p "The blog has been deleted from database."))))))))

(define-easy-handler (blog-index-page
		      :uri (lambda (request)
					 (trivia:match
					     (str:split "/" (script-name* request))
					   ((list "" "blog" _) t)
					   (_ nil)))
		      :host *ninx-blog-host*) ()
  (trivia:match
      (str:split "/" (script-name*))
    ((list "" "blog" user)
     (with-html-output-to-string (*standard-output*)
       (htm (:html :lang "en"
		   (:head (:meta :charset "utf-8")
			  (:meta :http-equiv "x-ua-compatible" :content "ie=edge")
			  (:meta :name "viewport" :content "width=device-width, initial-scale=1")
			  (:title "Ninx | Essays")
			  (:style "body {line-height: 1.4; font-size: 16px; padding: 0 10px; margin: 50px auto; max-width: 650px; text-align: left; text-wrap: pretty;}")
			  (:link :rel "icon" :href "/blog-priv/favicons/favicon.ico" :sizes "any")
			  (:link :rel "apple-touch-icon" :href "/blog-priv/favicons/apple-touch-icon.png")
			  (:link :rel "manifest" :href "/blog-priv/favicons/manifest.json")
			  )
		   (let ((posts (get-all-blog-posts-by-author (get-admin-full-name user))))
		     (if posts
			 (htm (:div
			       (loop for post in (reverse posts) ;; reverse to start with the newest essay
				     collect
				     (htm
				      (:article
				       :class "" :style "margin-bottom: 4rem; white-space: pre-wrap;"
				       (:h2 :class "" (str (car post)))
				       (:p :style "margin-bottom: 1.25rem; color: #727272;"
					   "Added on " (str (nth 5 post))
					   " Written by "
					   (:a :style (inline-css '(:color blue)) :href "#" (str (cadddr post))))
				       (if (> 500 (length (nth 4 post)))
					   (htm  (:p :class "blog-post" (str (nth 4 post))))
					   (htm  (:p :class "blog-post"
						     (str (subseq (nth 4 post) 0 500))
						     "... "
						     (:a :style (inline-css '(:color blue)) :href (str (format nil "/blog/~a/~a" user (cadr post))) "Continue"))))
				       (if (and (not (null (cookie-in "na-cookie"))) (equal user (base64-string-to-string (cookie-in "na-cookie"))))
					   (htm (:div :class ""
						      (:div :class ""
							    (:a :style (inline-css '(:color blue)) :href (str (format nil "/blog/~a/update-blog-post/~a" user (cadr post))) "Edit Post"))
						      (:div :class ""
							    (:a :style (inline-css '(:color blue)) :href (str (format nil "/blog/~a/save-blog-as-draft/~a" user (cadr post))) "Save as Draft"))
						      
						      )))
				       )))))
			 (htm (:p "There are no blog posts"))))))))))

(define-easy-handler (blog-page :uri (lambda (request)
					 (trivia:match
					     (str:split "/" (script-name* request))
					   ((list "" "blog" _ _) t)
					   (_ nil)))
				:host *ninx-blog-host*) ()
  (trivia:match
      (str:split "/" (script-name*))
    ((list "" "blog" user blog-url) 
     ;; displays the actual blog essay for reading in full format.
     (with-html-output-to-string (*standard-output*)
       (htm (:html :lang "en"
		   (:head (:meta :charset "utf-8")
			  (:meta :http-equiv "x-ua-compatible" :content "ie=edge")
			  (:meta :name "viewport" :content "width=device-width, initial-scale=1")
			  (:title "Ninx | Essays")
			  (:style "body {line-height: 1.4; font-size: 16px; padding: 0 10px; margin: 50px auto; max-width: 650px; text-align: left; text-wrap: pretty;}")
			  (:link :rel "icon" :href "/blog-priv/favicons/favicon.ico" :sizes "any")
			  (:link :rel "apple-touch-icon" :href "/blog-priv/favicons/apple-touch-icon.png")
			  (:link :rel "manifest" :href "/blog-priv/favicons/manifest.json"))
		   
		   (let ((post-list (get-blog-post blog-url)))
		     (if post-list
			 (let ((post (car post-list)))
			   (htm (:article :class "blog-post-div"
					  (:h2 :class "" (str (car post)))
					  (:p :style "margin-bottom: 1.25rem; color: #727272;"
					      "Added on " (str (nth 4 post))
					      " Written by "
					      (:a :href "#" (str (caddr post)))
					      (:br)
					      )
					  (htm  (:p :style (inline-css '(:white-space pre-wrap :text-align justify)) (str (cadddr post))))			 
					  )))
			 (htm (:p "There are no blog posts"))))))))))
