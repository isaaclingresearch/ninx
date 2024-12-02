(in-package :spotpdf)

(defun home-js ()
  "the js for the home page"
  (ps:ps
    
    (defun scroll-to-bottom ()
      (ps:chain window (scroll-to (ps:create
				   top (ps:chain document body scroll-height)
				   behavior "smooth"))))
    
    (defun is-mobile-browser ()
      "check if the browser is mobile"
      (let ((user-agent (or (ps:chain navigator user-agent) (ps:chain navigator vendor) (ps:chain window opera))))
	(cond
	  ((ps:chain (regex "/android/i") (test user-agent)) t)
	  ((ps:chain (regex "/iPad|iPhone|iPod/") (test user-agent)) t)
	  ((ps:chain (regex "/windows phone/i") (test user-agent)) t)
	  ((ps:chain (regex "/blackberry|bb10|playbook/i") (test user-agent)) t)
	  (t false))))
    
    ;; Only show the drop zone if it's not a mobile browser.
    (ps:chain document (add-event-listener "DOMContentLoaded" (lambda ()
								(let ((drop-zone (ps:chain document (get-element-by-id "drop-zone")))
								      (style (if (is-mobile-browser) "none" "block")))
								  (setf (ps:chain drop-zone style display) style)))))

    (defun generate-random-id ()
      "generate a random id from date"
      (+ "id" (ps:chain -date (now)) "-" (ps:chain -math (random) (to-string 36) (substr 2 9))))

    (setf files-array (array)) ;; this will hold the files
    
    (defun handle-files (files)
      "This function handles addition of files when a user submits or drops them."
      (let ((files-container (ps:chain document (get-element-by-id "files-container"))))
	(ps:chain -array (from files)
		  (for-each
		   (lambda (file)
                     ;; Add the file to the files array
                     (ps:chain files-array (push file))
                     (let ((file-frame (ps:chain document (create-element "div")))
			   (close-button (ps:chain document (create-element "button")))
			   (frame-id (generate-random-id))
			   (btn-id (generate-random-id)))
                       ;; Handle file frame
                       (ps:chain file-frame class-list (add "file-frame"))
                       (ps:chain file-frame (set-attribute "id" frame-id))
                       (ps:chain file-frame (set-attribute "draggable" "true")) ;; Allow frames to be draggable

                       ;; Handle close button
                       (ps:chain close-button class-list (add "close-btn"))
                       (ps:chain close-button (set-attribute "id" btn-id))
                       (setf (ps:chain close-button inner-h-t-m-l) "&times;")
                       (ps:chain close-button
				 (add-event-listener "click"
                                                     (lambda ()
                                                       ;; Remove the file from files-array
                                                       (let ((index (ps:chain files-array (index-of file))))
							 (if (> index -1)
                                                             (ps:chain files-array (splice index 1))))
                                                       ;; Remove the file frame
                                                       (ps:chain file-frame (remove))))))

                     ;; Handle file type
                     (cond
                       ;; Image files
                       ((ps:chain file type (starts-with "image/"))
			(let ((img (ps:chain document (create-element "img")))
                              (reader (new -file-reader)))
                          (setf (ps:chain reader onload)
				(lambda (e) (setf (ps:chain img src) (ps:chain e target result))))
                          (ps:chain reader (read-as-data-u-r-l file))
                          (ps:chain file-frame (append-child img))
			  (ps:chain file-frame (append-child close-button))))

                       ;; PDF files
                       ((eql (ps:chain file type) "application/pdf")
			(let ((iframe (ps:chain document (create-element "iframe")))
                              (file-name (ps:chain document (create-element "span"))))
                          (setf (ps:chain iframe src) (ps:chain -u-r-l (create-object-u-r-l file)))
                          (ps:chain file-frame (append-child iframe))
                          ;; File name
                          (ps:chain file-name class-list (add "file-name"))
                          (setf (ps:chain file-name text-content) (ps:chain file name (to-lower-case)))
                          (ps:chain file-frame (append-child file-name))
			  (ps:chain file-frame (append-child close-button))))

                       ;; Other files
                       (t
			(let ((file-row (ps:chain document (create-element "div")))
                              (file-name (ps:chain document (create-element "span"))))
                          ;; Create a row container
                          (ps:chain file-row class-list (add "file-row"))

                          ;; Set up file name
                          (ps:chain file-name class-list (add "file-name"))
                          (setf (ps:chain file-name text-content) (ps:chain file name (to-lower-case)))

                          ;; Append file name and close button to the row
                          (ps:chain file-row (append-child file-name))
                          (ps:chain file-row (append-child close-button))

                          ;; Append the row to the file frame
                          (ps:chain file-frame (append-child file-row)))))

                     ;; Append the file frame to the container
                     (ps:chain files-container (append-child file-frame))
                     (scroll-to-bottom))))))


    ;; Enable drag-and-drop file rearranging within files-container
    (setf files-container (ps:chain document (get-element-by-id "files-container")))
    (setf dragged-item nil)

    (ps:chain files-container (add-event-listener "dragstart"
						  (lambda (event)
						    (setf dragged-item (ps:chain event target))
						    ;; Add visual feedback when dragging starts
						    (set-timeout (lambda () (setf (ps:chain dragged-item style opacity) "0.5")) 0))))

    (ps:chain files-container (add-event-listener "dragend"
						  (lambda ()
						    ;; Reset opacity and clear dragged item after dropping
						    (set-timeout (lambda ()
								   (setf (ps:chain dragged-item style opacity) "1")
								   (setf dragged-item nil)) 0))))

    (ps:chain files-container (add-event-listener "dragover"
						  (lambda (event)
						    ;; Allow dropping by preventing the default behavior
						    (ps:chain event (prevent-default)))))

    (ps:chain files-container (add-event-listener "drop"
						  (lambda (event)
						    (ps:chain event (prevent-default)) ;; Prevent default drop behavior
						    (let ((target (ps:chain event target)))
						      (when (and dragged-item (ps:chain target class-list (contains "file-frame")))
							(let ((rect (ps:chain target get-bounding-client-rect))
							      (offset (ps:chain event client-y)))
							  ;; Determine whether to insert before or after the target based on the Y position
							  (if (< offset (+ (ps:chain rect top) (/ (ps:chain rect height) 2)))
							      (ps:chain files-container (insert-before dragged-item target))
							      (let ((next-sibling (ps:chain target next-sibling)))
								(if next-sibling
								    (ps:chain files-container (insert-before dragged-item next-sibling))
								    (ps:chain files-container (append-child dragged-item)))))))))))

    ;; Add files via "Add a file" button click
    (ps:chain document (get-element-by-id "upload-btn")
	      (add-event-listener "click"
				  (lambda ()
				    (let ((file-input (ps:chain document (get-element-by-id "file-input"))))
				      (ps:chain file-input (click))
				      (setf (ps:chain file-input onchange)
					    (lambda ()
					      (handle-files (ps:chain file-input files))
					      (setf (ps:chain file-input value) "")))))))

    ;; Enable file drag-and-drop in the drop zone
    (setf drop-zone (ps:chain document (get-element-by-id "drop-zone")))

    (ps:chain drop-zone (add-event-listener "dragenter" (lambda (event)
							  (ps:chain event (prevent-default))
							  (ps:chain drop-zone class-list (add "dragging")))))

    (ps:chain drop-zone (add-event-listener "dragover" (lambda (event)
							 (ps:chain event (prevent-default)))))

    (ps:chain drop-zone (add-event-listener "dragleave" (lambda (event)
							  (ps:chain event (prevent-default))
							  (ps:chain drop-zone class-list (remove "dragging")))))

    (ps:chain drop-zone (add-event-listener "drop"
					    (lambda (event)
					      (ps:chain event (prevent-default))
					      (ps:chain drop-zone class-list (remove "dragging"))
		 			      (handle-files (ps:chain event data-transfer files)))))
    
    ;; Upload files to the "/files" endpoint using plain JS
    (let ((submit-btn (ps:chain document (get-element-by-id "submit-btn")))
	  (progress-container (ps:chain document (get-element-by-id "progress-container")))
	  (progress-bar (ps:chain document (get-element-by-id "upload-progress")))
	  (loading-indicator (ps:chain document (get-element-by-id "loading-indicator")))
	  (error-container (ps:chain document (get-element-by-id "error-container")))
	  (error-indicator (ps:chain document (get-element-by-id "error-indicator")))
	  (loading-container (ps:chain document (get-element-by-id "loading-container")))
	  )

      (defun upload-files ()
	"upload the files and track progress of the upload"
	(ps:chain console (log files-array))
	(let ((form-data (new -form-data))
	      (xhr (new -x-m-l-http-request))
	      )
	  ;; Add files to FormData
	  (when (> (ps:chain files-array length) 0)
	    (ps:chain form-data (set "number-of-files" (ps:chain files-array length)))
	    (loop for i from 0 below (ps:chain files-array length) do
	      (ps:chain form-data (append (+ "file_" i) (aref files-array i)))))
	  
	  ;; Open the request
	  (let* ((action (ps:chain window location pathname)))
	    (ps:chain xhr (open "POST" (+ "/convert-" (ps:chain action (substr 1))) t)))
	  ;; Track progress
	  (setf (ps:chain xhr upload onprogress)
		(lambda (event)
		  (when (ps:chain event length-computable)
		    (setf (ps:chain progress-bar value)
			  (/ (* 100 (ps:chain event loaded)) (ps:chain event total)))
		    (when (eql (ps:chain event loaded) (ps:chain event total))
		      (setf (ps:chain progress-container style display) "none")
		      (setf (ps:chain loading-indicator style display) "block")
		      (setf (ps:chain loading-container style display) "block")
		      (setf (ps:chain loading-indicator inner-h-t-m-l) "Converting, please wait...")))))

	  (setf (ps:chain xhr onloadstart) (lambda ()
					     "Show loading indicator when uploading starts"
					     (setf (ps:chain loading-container style display) "none")
					     (setf (ps:chain progress-container style display) "block")
					     (setf (ps:chain loading-indicator style display) "block")
					     (setf (ps:chain error-container style display) "none")
					     (setf (ps:chain error-indicator style display) "none")
					     ))

	  (setf (ps:chain xhr onload)
		(lambda ()
		  "handle upload completion and process response"
		  (if (and (eql (ps:chain xhr ready-state) 4) (eql (ps:chain xhr status) 200))
		      (let* ((response-json (ps:chain xhr response-text))
			     (response (ps:chain -j-s-o-n (parse response-json))))
			(ps:chain console (log response))
			(setf (ps:chain progress-container style display) "none")
			(setf (ps:chain loading-indicator style display) "none")
			(if (eql (ps:chain response success) t)
			    (setf (ps:chain window location href) 
				  (+ (ps:chain window location pathname) "/" (ps:chain response directory)))
			    (progn
			      (ps:chain console (log response))
			      (let* ((error-code (ps:chain response error-code))
				     (error-text (cond
						   ((eql error-code 429) "Server currently busy, please try again later.")
						   ((eql error-code 500) "An error occured, please try again.")
						   ((eql error-code 503) "Server currently experiencing some technical problems. Try again later.")
						   ((eql error-code 504) "The conversion is taking very long, won't finish in time. Decrease number and/or size of the documents and try again.")
						   (t "An error occurred while processing the request. Please try again.")))))
			      (setf (ps:chain progress-container style display) "none")
			      (setf (ps:chain loading-indicator style display) "none")
			      (setf (ps:chain loading-container style display) "none")
			      (setf (ps:chain error-indicator inner-h-t-m-l) error-text)
			      (setf (ps:chain error-container style display) "block")
			      (setf (ps:chain error-indicator style display) "block")
			      )))
		      (progn 
			(setf (ps:chain loading-container style display) "none")
			(setf (ps:chain progress-container style display) "none")
			(setf (ps:chain loading-indicator style display) "none")
			(setf (ps:chain error-container style display) "block")
			(setf (ps:chain error-indicator style display) "block")
			))))

	  ;; Send the request only if there's a description or files to send
	  (when (> (length files-array) 0)
	    (ps:chain xhr (send form-data)))))

      (ps:chain submit-btn (add-event-listener "click" (lambda ()
							 "submit button click listener"
							 (upload-files))))
      ;; hide drop-zone on mobile devices
      (when (is-mobile-browser)
	(setf (ps:chain document (get-element-by-id "drop-zone") style display) "none")))))

(defun home-css ()
  "the css for the /home endpoint"
  (let ((fg-color "#1a1a1a")
	(red "#FF6060")
;	(blue "#CCCCFF")
	(bg-color "#e8e8e8")
	(link-blue "#1e90ff"))
    (cl-css:css
     `((body :text-align center :background-color ,bg-color :color ,fg-color :margin 20px :font-size 16px)
       (button :background-color "#00b800" :border "none" :color "#e8e8e8" :padding "10px 20px" :text-align "center" :text-decoration "none" :display "inline-block" :font-size "16px" :margin "4px 2px" :cursor "pointer" :border-radius "12px" :font-weight bold)
       (.submit-btn :float right :background-color "#00b800")
       (.upload-btn :float left)
       (.add-symbol :margin-right 8px :font-size :16px :font-weight bold)
       ("button:hover" :background-color "#45a049")

       (.file-frame :display "inline-block" :margin "10px" :padding "10px" :position "relative" :text-align "center" :max-width "150px")
       (.file-row :display "flex" :align-items "center" :justify-content "space-between" :margin-bottom "10px" :padding "10px" :border "1px solid #ccc" :border-radius "4px" :background-color "#f9f9f9" :max-width "150px")
       (.file-name :font-size "16px" :color ,fg-color :margin-top "5px" :word-wrap "normal" :max-width "150px" :overflow "hidden" :text-overflow "ellipsis" :flex-grow "1")
       (.close-btn :position "absolute" :top "5px" :right "5px" :background-color "red" :color "white" :border "none" :cursor "pointer" :border-radius "0%" :width "24px" :height "24px" :display "flex" :align-items "center" :justify-content "center" :font-size "14px" :line-height "1")
       (".close-btn:hover" :background-color "#d32f2f")
       ("img, iframe" :max-width "150px" :max-height "150px")
       (.drop-zone :width "100%" :height "200px" :border "2px dashed #1a1a1a" :display "flex" :align-items "center" :justify-content "center" :text-align "center" :margin "20px 0" :cursor "pointer")
       (.drop-zone.dragging :border-color "#666")

       (h2 :color ,fg-color)
       (p :text-align "justify")
       (.error-p :color ,red)
       (.top :font-weight bold)
       (a :color ,link-blue :text-decoration none :decoration none :margin-left 10px :margin-right 10px)
       (a.dropdown-item :color ,link-blue :text-decoration none :decoration none :margin-left 10px :margin-right 10px)
       ("a:hover::after" :color ,link-blue)
       ("a:visited" :color ,link-blue :decoration none)
       ("a:hover" :color ,link-blue :decoration underline)
       (.logo :display flex :justify-content center :flex-direction :row :align-items :center :gap "10px")
       ("a.logo-link" :text-align center :color ,link-blue :text-decoration none :font-size 30px)
       (.logo-image :width 50px)
       ("a.logo-link:hover" :color ,link-blue)
       (.loading-container :width "150px" :height "10px" :background-color "#e0e0e0" :overflow "hidden" :position "relative" :border-radius "5px" :border 0 :margin "0 auto")
       (.bar :width "30px" :height "100%" :background-color "#00b800" :position "absolute" :left "-30px" :animation "move 2s cubic-bezier(0.42, 0, 0.58, 1) infinite")
       (".bar:nth-child(2)" :animation-delay "1s")
       ("@keyframes move" ("0%" :left "-30px") ("50%" :left "100%") ("51%" :left "-30px") ("100%" :left "100%"))
       (".description:focus" :border none :color ,fg-color)
       (.description-title :margin-top 12px)
       (.ad :width 75% :height 5% :margin-top 3%)
       (body :font-size 18px :width 80% :margin-left 10%)
       ("a.download-btn" :color "#e8e8e8" :text-decoration none)
       ("a:visited.download-btn" :color "#e8e8e8" :text-decoration none)
       (.submenu-item :background-color "#e8e8e8")
       (a.active :color black)
       (footer :position sticky :margin-top 10% :width 100% :bottom 0 :height 60px :line-height 60px :background-color "#f5f5f5")
       (.index-footer :margin-top 25%)
       ("@media only screen and (max-width: 768px)"
	(footer :position sticky :width 100vw :bottom 0 :margin-left -0.5% :margin-top 120% :height 90px :line-height 20px :background-color "#f5f5f5")
	(a :margin-left 2px :margin-right 2px)
	(.file-name :font-size 20px)
	(.close-btn :font-size 20px)
	(.ad :width 98%)
	(.instruct :text-align justify)
	(button :font-size 22px)
	(.upload-btn :float none)
	(a.dropdown-item :font-size 22px)
	(.nav-link :font-size 22px)
	(body :font-size 20px :margin 2%)
	(.index-footer :position absolute)
	(.btns :display flex :flex-direction column))))))

(defparameter *file-types* '(("azw3" . ("docx" "epub" "fb2" "lit" "mobi" "pdf" "pdb" "rtf" "txt"))
			     ("doc" . ("pdf"))
			     ("docx" . ("azw3" "epub" "fb2" "lit" "mobi" "pdf" "pdb" "rtf" "txt"))
			     ("epub" . ("azw3" "docx" "fb2" "lit" "mobi" "pdf" "pdb" "rtf" "txt"))
			     ("excel" . ("pdf"))
			     ("fb2" . ("azw3" "docx" "epub" "lit" "mobi" "pdf" "pdb" "rtf" "txt"))
			     ("lit" . ("azw3" "docx" "epub" "fb2" "mobi" "pdf" "pdb" "rtf" "txt"))
			     ("mobi" . ("azw3" "docx" "epub" "fb2" "lit" "pdf" "pdb" "rtf" "txt"))
			     ("pdb" . ("azw3" "docx" "epub" "fb2" "lit" "mobi" "pdf" "rtf" "txt"))
			     ("pdf" . ("azw3" "docx" "epub" "fb2" "jpeg" "jpg" "lit" "mobi" "pdb" "png" "powerpoint" "ppt" "pptx" "rtf" "tiff" "txt" "word"))
			     ("powerpoint" . ("pdf"))
			     ("ppt" . ("pdf"))
			     ("pptx" . ("pdf"))
			     ("rtf" . ("azw3" "docx" "epub" "fb2" "lit" "mobi" "pdf" "pdb" "txt"))
			     ("txt" . ("azw3" "docx" "epub" "fb2" "lit" "mobi" "pdf" "pdb" "rtf"))
			     ("word" . ("pdf"))
			     ("xls" . ("pdf"))
			     ("xlsx" . ("pdf"))))

(defparameter *image-types* '(("image" . ("pdf"))
			      ("jpeg" . ("pdf" "png" "tiff" "webp"))
			      ("jpg" . ("pdf" "png" "tiff" "webp"))
			      ("png" . ("jpeg" "jpg" "pdf" "tiff" "webp"))
			      ("tiff" . ("jpeg" "jpg" "png" "webp"))
			      ("webp" . ("jpeg" "jpg" "pdf" "png" "tiff"))))

(defun make-sitemap (&key (path #p"~/common-lisp/ninx/priv/spotpdf/sitemap.xml") (alist *file-types*))
  "Save a sitemap to the given PATH from the provided ALIST in XML format."
  (with-open-file (stream (truename path) :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
    (format stream "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">~%")
    (dolist (entry alist)
      (let ((from (car entry))
            (tos (cdr entry)))
        (dolist (to tos)
          (format stream "  <url><loc>~a-to-~a</loc></url>~%" from to))))
    (format stream "</urlset>")))

(defun header ()
  (with-html-output (*standard-output*)
    (htm
     (:nav :class "navbar navbar-expand-lg"
	   (:div :class "container-fluid"
		 (:a :class "navbar-brand" :href "/"
		     (:img :src "/spotpdf/spotpdf-logo.png" :width "68" :height "60" :class "d-inline-block align-top" :alt ""))
		 (:ul :class "navbar-nav me-auto mb-2 mb-lg-0"
		      (:li :class "nav-item dropdown"
			   (:a :class "nav-link dropdown-toggle" :href "#" :id "convert-menu" :role "button" :data-bs-toggle "dropdown"
			       :aria-expanded "false" "Convert Documents")
			   (:div :class "dropdown-menu convert-menu" :aria-labelledby "convert-menu"
				 (loop for (from . to-list) in *file-types*
				       do
					  (let ((from-capital (str:upcase from)))
					    (htm
					     (:div :class "dropdown dropend"
						   (:a :class "dropdown-item dropdown-toggle m-0 p-0 ps-1" :href "#" :id (format nil "~a-menu" from) :role "button" :data-bs-toggle "dropdown" :aria-expanded "false" (cl-who:fmt "~a to" from-capital))
						   (:div :class "dropdown-menu" :aria-labelledby (format nil "~a-menu" from)
							 (loop for to in (remove-duplicates (sort to-list #'string<) :test #'string=)
							       do
								  (let* ((from-to (format nil "/~a-to-~a" from to))
									 (from-to-class (format nil "m-0 p-0 ps-1 dropdown-item m-0 submenu-item ~a" (if (string= from-to (script-name*)) "" "active-link"))))
								    (htm (:a :href from-to :class from-to-class (cl-who:fmt "~a to ~a" from-capital (str:upcase to)))))))))))))
		      (:li :class "nav-item dropdown"
			   (:a :class "nav-link dropdown-toggle" :href "#" :id "convert-menu" :role "button" :data-bs-toggle "dropdown"
			       :aria-expanded "false" "Convert Images")
			   (:div :class "dropdown-menu convert-menu" :aria-labelledby "convert-menu"
				 (loop for (from . to-list) in *image-types*
				       do
					  (let ((from-capital (str:upcase from)))
					    (htm
					     (:div :class "dropdown dropend"
						   (:a :class "dropdown-item dropdown-toggle m-0 p-0 ps-1" :href "#" :id (format nil "~a-menu" from) :role "button" :data-bs-toggle "dropdown" :aria-expanded "false" (cl-who:fmt "~a to" from-capital))
						   (:div :class "dropdown-menu" :aria-labelledby (format nil "~a-menu" from)
							 (loop for to in (remove-duplicates (sort to-list #'string<) :test #'string=)
							       do
								  (let* ((from-to (format nil "/~a-to-~a" from to))
									 (from-to-class (format nil "m-0 p-0 ps-1 dropdown-item m-0 submenu-item ~a" (if (string= from-to (script-name*)) "" "active-link"))))
								    (htm (:a :href from-to :class from-to-class (cl-who:fmt "~a to ~a" from-capital (str:upcase to)))))))))))))))))))

(defun footer ()
  (with-html-output (*standard-output*)
    (htm (:footer :class (if (ppcre:scan "(/|/(.)/(.))" (script-name*))
			      "footer index-footer" "footer")
		  (:div :class "container"
			(:b "SpotPDF - A one stop conversion spot.")
		       	(:b (cl-who:fmt "  © Ninx Technology Limited ~a." (ninx:get-current-year))))))))

(define-easy-handler (home
		      :uri (define-matching-functions "^/$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (with-html-output-to-string (*standard-output*)
    (:html :lang "en"
	   (:head
	    (:title "SpotPDF. FAST. FREE. ONLINE.")
	    (:meta :name "description" :content "Convert documents from one format to another. FAST. FREE. ONLINE.")
	    (:meta :name "keywords" :content "spotpdf spot pdf convert pdf, word, excel, powerpoint jpg png .")
	    (:meta :charset "UTF-8")
	    (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	    (:link :rel "manifest" :href "/spotpdf/manifest.json")
	    (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	    (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")
	    (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	    (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	    (:style (str (home-css))))
	   (:body
	    (:main :role "main" :class "container"
		   (header)
		   :br
		   (:h5 :class "instruct" "Click on 'Convert Documents' to convert, if you have documents.")
		   :br
		   (:h5 :class "instruct" "Click on 'Convert Images' to convert, if you have images."))
	    (:div :class "ad")
	    (footer))
	   (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))

(define-easy-handler (pdf-to-ppt
		      :uri (define-matching-functions "^/pdf-to-(ppt|pptx|powerpoint)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((to (cadr (str:split "-to-" (script-name*))))
	 (to-capital (str:upcase to)))
    (with-html-output-to-string (*standard-output*)
      (:html :lang "en"
	     (:head
	      (:title (cl-who:fmt "Convert PDF to ~a. FAST. FREE. ONLINE." to-capital))
	      (:meta :name "description" :content (format nil "Convert PDFs to accurate, editable ~a slideshows/presentations. FAST. FREE. ONLINE." to-capital))
	      (:meta :name "keywords" :content (format nil "spotpdf spot pdf convert pdf to ~a, online, most accurate, free, fast" to))
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:link :rel "manifest" :href "/spotpdf/manifest.json")
	      (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")    
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert PDF to ~a" to-capital))
		    (:p (cl-who:fmt "Convert your PDFs to ~a" to-capital))
		    (:div :id "drop-zone" :class "drop-zone" " Drag and drop files here or click the Choose PDF button")
		    (:input :type "file" :id "file-input" :style "display: none;" :accept ".pdf" :multiple t)
		    (:div :id "files-container")
		    (:div :class "btns"
			  (:button :class "upload-btn" :id "upload-btn"
				   (:span :class "add-symbol" "+")
				   "Choose PDF")
			  (:button :class "submit-btn" :id "submit-btn" (cl-who:fmt "Convert to ~a" to-capital)))
		    (:div :id "loading-container" :class "loading-container" :style "display: none;"
			  (:div :class "bar")
			  (:div :class "bar")
			  (:div :class "bar"))
		    (:div :id "progress-container" :style "display: none;"
			  (:progress :id "upload-progress" :value "0" :max "100"))
		    (:div :id "loading-indicator" :style "display: none;"
			  "Submitting... Please wait.")
		    (:div :id "error-container" :style "display: none;"
			  (:progress :id "error-progress" :value "100" :style "color: #FF6060"))
		    (:div :id "error-indicator" :style "display: none; color: #FF6060"
			  "An error occurred, please try again.")
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))

(define-easy-handler (convert-pdf-to-pptx-route
		      :uri (define-matching-functions "^/convert-pdf-to-(ppt|pptx|powerpoint)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let*  ((files (post-parameters*))
	  (uuid (to-string (make-v4)))
	  (to (ppcre:regex-replace-all "(/convert-pdf-to-)" (script-name*) ""))
	  (format (if (string= to "ppt") "ppt" "pptx")))
    (convert-pdf-to-format format uuid files)
    (jzon:stringify (hash-create `(("directory" ,uuid)
				   ("success" t))))))

(define-easy-handler (process-pdf-to-pptx
		      :uri (define-matching-functions "^/pdf-to-(ppt|pptx|powerpoint)/([^/]+)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((dir-list (str:split "/" (script-name*)))
	 (dir (caddr dir-list))
	 (to (cadr (str:split "-to-" (cadr dir-list))))
	 (to-capital (str:upcase to)))
    (with-html-output-to-string (*standard-output*)
      (:html :lang "en"
	     (:head
	      (:title (cl-who:fmt "Convert PDF to ~a. ONLINE. FREE." to-capital))
	      (:meta :name "description" :content (format nil "Convert PDFs to accurate, editable ~a slideshows/presentations. FAST. FREE. ONLINE" to-capital))
	      (:meta :name "keywords" :content (format nil "spotpdf spot pdf convert pdf to ~a, online, most accurate, free, fast" to))
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:link :rel "manifest" :href "/spotpdf/manifest.json")
	      (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")    
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      
	      (:style (str (home-css))))
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert PDF to ~a" to-capital))
		    (:p (cl-who:fmt "Your files have been converted to ~a." to-capital))
		    (:a :class "download-btn" :target "_blank" :href (format nil "/download-file/~a" dir) (:button "Download now."))
		    
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))

(define-easy-handler (download-pdf-file
		      :uri (define-matching-functions "^/download-file/([^/]+)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((dir (third (str:split "/" (script-name*)))))
    (trivia:match (get-downloadable-data dir)
      ((list type file-name data)
       (setf (content-type*) type)
       (setf (header-out "content-disposition") (format nil "attachment; filename=~s" file-name))
       (setf (content-length*) (primitive-object-size data))
       data)
      (else (format *terminal-io* "~a" else)))))

;; PDF TO DOCS

(define-easy-handler (pdf-to-word
		      :uri (define-matching-functions "^/pdf-to-(word|doc)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((to (cadr (str:split "-to-" (script-name*))))
	 (to-capital (str:upcase to)))
    (with-html-output-to-string (*standard-output*)
      (:html :lang "en"
	     (:head
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:link :rel "manifest" :href "/spotpdf/manifest.json")
	      (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")

	      (:title (cl-who:fmt "Convert PDF to ~a. FAST. FREE. ONLINE." to-capital))
	      (:meta :name "description" :content (format nil "Convert PDF to accurate, editable ~a documents. FAST. FREE. ONLINE" to-capital))
	      (:meta :name "keywords" :content (format nil "spotpdf spot pdf convert pdf to ~a, online, most accurate, free, fast" to))
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert PDF to ~a" to-capital))
		    (:p (cl-who:fmt "Convert your PDFs to ~a." to-capital))

		    (:div :id "drop-zone" :class "drop-zone" " Drag and drop files here or click the Choose PDF button")
		    (:input :type "file" :id "file-input" :style "display: none;" :accept ".pdf" :multiple t)
		    (:div :id "files-container")
		    (:div :class "btns"
			  (:button :class "upload-btn" :id "upload-btn"
				   (:span :class "add-symbol" "+")
				   "Choose PDF")
			  (:button :class "submit-btn" :id "submit-btn" (cl-who:fmt "Convert to ~a" to-capital)))
		    (:div :id "loading-container" :class "loading-container" :style "display: none;"
			  (:div :class "bar")
			  (:div :class "bar")
			  (:div :class "bar"))
		    (:div :id "progress-container" :style "display: none;"
			  (:progress :id "upload-progress" :value "0" :max "100"))
		    (:div :id "loading-indicator" :style "display: none;"
			  "Submitting... Please wait.")
		    (:div :id "error-container" :style "display: none;"
			  (:progress :id "error-progress" :value "100" :style "color: #FF6060"))
		    (:div :id "error-indicator" :style "display: none; color: #FF6060"
			  "An error occurred, please try again.")
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))

(define-easy-handler (convert-pdf-to-word-route
		      :uri (define-matching-functions "^/convert-pdf-to-(word|doc)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((files (post-parameters*))
	 (uuid (to-string (make-v4)))
	 (to (ppcre:regex-replace-all "(/convert-pdf-to-)" (script-name*) ""))
	 (format (if (string= to "word") "docx" "doc")))
    
    (convert-pdf-to-format format uuid files)
    (jzon:stringify (hash-create `(("directory" ,uuid)
				   ("success" t))))))

(define-easy-handler (process-pdf-to-word
		      :uri (define-matching-functions "^/pdf-to-(word|doc)/([^/]+)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((dir-list (str:split "/" (script-name*)))
	 (dir (caddr dir-list))
	 (to (cadr (str:split "-to-" (cadr dir-list))))
	 (to-capital (str:upcase to)))
    (with-html-output-to-string (*standard-output*)
      (:html :lang "en"
	     (:head
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:link :rel "manifest" :href "/spotpdf/manifest.json")
	      (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")

	      (:title (cl-who:fmt "Convert PDF to ~a. FAST. FREE. ONLINE." to-capital))
	      (:meta :name "description" :content (format nil "Convert PDF to accurate, editable ~a documents. FAST. FREE. ONLINE" to-capital))
	      (:meta :name "keywords" :content (format nil "spotpdf spot pdf convert pdf to ~a, online, most accurate, free, fast" to))
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert PDF to ~a" to-capital))
		    (:p (cl-who:fmt "Your files have been converted to ~a." to-capital))
		    (:a :class "download-btn" :target "_blank" :href (format nil "/download-file/~a" dir) (:button "Download now."))
		    
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))


;;; DOCX TO PDF

(define-easy-handler (word-to-pdf
		      :uri (define-matching-functions "^/(doc|word)-to-pdf$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((to (str:replace-all "/" "" (car (str:split "-to-" (script-name*)))))
	 (to-capital (str:upcase to)))
    (with-html-output-to-string (*standard-output*)
      (:html :lang "en"
	     (:head
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:link :rel "manifest" :href "/spotpdf/manifest.json")
	      (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")

	      (:title (cl-who:fmt "Convert ~a to PDF. FAST. FREE. ONLINE." to-capital))
	      (:meta :name "description" :content (format nil "Convert ~a documents to PDFs. Convert ~a to PDFs in seconds. FAST. FREE. ONLINE" to-capital to-capital))
	      (:meta :name "keywords" :content (format nil "spotpdf spot pdf convert ~a to pdf,  online, free." to))
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to PDF" to-capital))
		    (:p (cl-who:fmt "Convert your ~a documents to PDFs." to-capital))

		    (:div :id "drop-zone" :class "drop-zone" (cl-who:fmt "Drag and drop files here or click the Choose ~a button" to-capital))
		    (:input :type "file" :id "file-input" :style "display: none;" :accept ".doc, .docx" :multiple t)
		    (:div :id "files-container")
		    (:div :class "btns"
			  (:button :class "upload-btn" :id "upload-btn"
				   (:span :class "add-symbol" "+")
				   (cl-who:fmt "Choose ~a" to-capital))
			  (:button :class "submit-btn" :id "submit-btn" "Convert to PDF"))
		    (:div :id "loading-container" :class "loading-container" :style "display: none;"
			  (:div :class "bar")
			  (:div :class "bar")
			  (:div :class "bar"))
		    (:div :id "progress-container" :style "display: none;"
			  (:progress :id "upload-progress" :value "0" :max "100"))
		    (:div :id "loading-indicator" :style "display: none;"
			  "Submitting... Please wait.")
		    (:div :id "error-container" :style "display: none;"
			  (:progress :id "error-progress" :value "100" :style "color: #FF6060"))
		    (:div :id "error-indicator" :style "display: none; color: #FF6060"
			  "An error occurred, please try again.")
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))

(define-easy-handler (convert-word-to-pdf-route
		      :uri (define-matching-functions "^/convert-(doc|word)-to-pdf$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let ((files (post-parameters*))
	(uuid (to-string (make-v4))))
    (convert-format-to-format "pdf" uuid files)
    (jzon:stringify (hash-create `(("directory" ,uuid)
				   ("success" t))))))

(define-easy-handler (process-word-to-pdf
		      :uri (define-matching-functions "^/(doc|word)-to-pdf/([^/]+)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((dir-list (str:split "/" (script-name*)))
	 (dir (caddr dir-list))
	 (from (car (str:split "-to-" (cadr dir-list))))
	 (from-capital (str:upcase from)))
    (with-html-output-to-string (*standard-output*)
      (:html :lang "en"
	     (:head
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:link :rel "manifest" :href "/spotpdf/manifest.json")
	      (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")

	      (:title (cl-who:fmt "Convert ~a to PDF. FAST. FREE. ONLINE." from-capital))
	      (:meta :name "description" :content (format nil "Convert ~a documents to PDFs. Convert ~a to PDFs in seconds. FAST. FREE. ONLINE" from-capital from-capital))
	      (:meta :name "keywords" :content (format nil "spotpdf spot pdf convert ~a to pdf,  online, free." from))
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))     
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to PDF" from-capital))
		    (:p "Your files have been converted to PDF.")
		    (:a :class "download-btn" :target "_blank" :href (format nil "/download-file/~a" dir) (:button "Download now."))
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))

;;; PPTX TO PDF

(define-easy-handler (pptx-to-pdf
		      :uri (define-matching-functions "^/(ppt|pptx|powerpoint)-to-pdf$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((from (str:replace-all "/" "" (car (str:split "-to-" (script-name*)))))
	 (from-capital (str:upcase from)))
    (with-html-output-to-string (*standard-output*)
      (:html :lang "en"
	     (:head
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:link :rel "manifest" :href "/spotpdf/manifest.json")
	      (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")

	      (:title (cl-who:fmt "Convert ~a to PDF. FAST. FREE. ONLINE." from-capital))
	      (:meta :name "description" :content (format nil "Convert ~a slides to PDFs. Convert ~a to PDFs in seconds. FAST. FREE. ONLINE" from-capital from-capital))
	      (:meta :name "keywords" :content (format nil "spotpdf spot pdf convert ~a to pdf, online, free." from-capital))
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to PDF" from-capital))
		    (:p (cl-who:fmt "Convert your ~a documents to PDFs." from-capital))

		    (:div :id "drop-zone" :class "drop-zone" (cl-who:fmt "Drag and drop files here or click the Choose ~a button" from-capital))
		    (:input :type "file" :id "file-input" :style "display: none;" :accept ".ppt, .pptx" :multiple t)
		    (:div :id "files-container")
		    (:div :class "btns"
			  (:button :class "upload-btn" :id "upload-btn"
				   (:span :class "add-symbol" "+")
				   (cl-who:fmt "Choose ~a" from-capital))
			  (:button :class "submit-btn" :id "submit-btn" "Convert to PDF"))
		    (:div :id "loading-container" :class "loading-container" :style "display: none;"
			  (:div :class "bar")
			  (:div :class "bar")
			  (:div :class "bar"))
		    (:div :id "progress-container" :style "display: none;"
			  (:progress :id "upload-progress" :value "0" :max "100"))
		    (:div :id "loading-indicator" :style "display: none;"
			  "Submitting... Please wait.")
		    (:div :id "error-container" :style "display: none;"
			  (:progress :id "error-progress" :value "100" :style "color: #FF6060"))
		    (:div :id "error-indicator" :style "display: none; color: #FF6060"
			  "An error occurred, please try again.")
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))

(define-easy-handler (convert-pptx-to-pdf-route
		      :uri (define-matching-functions "^/convert-(ppt|pptx|powerpoint)-to-pdf$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let ((files (post-parameters*))
	(uuid (to-string (make-v4))))
    (convert-format-to-format "pdf" uuid files)
    (jzon:stringify (hash-create `(("directory" ,uuid)
				   ("success" t))))))

(define-easy-handler (process-pptx-to-pdf
		      :uri (define-matching-functions "^/(ppt|pptx|powerpoint)-to-pdf/([^/]+)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((dir-list (str:split "/" (script-name*)))
	 (dir (caddr dir-list))
	 (from (car (str:split "-to-" (cadr dir-list))))
	 (from-capital (str:upcase from)))
    (with-html-output-to-string (*standard-output*)
      (:html :lang "en"
	     (:head
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:link :rel "manifest" :href "/spotpdf/manifest.json")
	      (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")

	      (:title (cl-who:fmt "Convert ~a to PDF. FAST. FREE. ONLINE." from-capital))
	      (:meta :name "description" :content (format nil "Convert ~a slides to PDFs. Convert ~a to PDFs in seconds. FAST. FREE. ONLINE" from-capital from-capital))
	      (:meta :name "keywords" :content (format nil "spotpdf spot pdf convert ~a to pdf, online, free." from-capital))
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))     
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to PDF" from-capital))
		    (:p "Your files have been converted to PDF.")
		    (:a :class "download-btn" :target "_blank" :href (format nil "/download-file/~a" dir) (:button "Download now."))
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))

;;; Excel TO PDF

(define-easy-handler (excel-to-pdf
		      :uri (define-matching-functions "^/(xlsx|xls|excel)-to-pdf$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((from (str:replace-all "/" "" (car (str:split "-to-" (script-name*)))))
	 (from-capital (str:upcase from)))
    (with-html-output-to-string (*standard-output*)
      (:html :lang "en"
	     (:head
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:link :rel "manifest" :href "/spotpdf/manifest.json")
	      (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")

	      (:title (cl-who:fmt "Convert ~a sheets to PDF Documents. FAST. FREE. ONLINE." from-capital))
	      (:meta :name "description" :content (format nil "Convert ~a sheets to PDF Documents in seconds. FAST. FREE. ONLINE" from-capital))
	      (:meta :name "keywords" :content (format nil "spotpdf spot pdf convert ~a to word, online, free." from))
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to PDF" from-capital))
		    (:p (cl-who:fmt "Convert your ~a sheets to PDF Documents" from-capital))

		    (:div :id "drop-zone" :class "drop-zone" (cl-who:fmt "Drag and drop files here or click the Choose ~a button" from-capital))
		    (:input :type "file" :id "file-input" :style "display: none;" :accept ".xlsx, .xls" :multiple t)
		    (:div :id "files-container")
		    (:div :class "btns"
			  (:button :class "upload-btn" :id "upload-btn"
				   (:span :class "add-symbol" "+")
				   (cl-who:fmt "Choose ~a" from-capital))
			  (:button :class "submit-btn" :id "submit-btn" "Convert to PDF"))
		    (:div :id "loading-container" :class "loading-container" :style "display: none;"
			  (:div :class "bar")
			  (:div :class "bar")
			  (:div :class "bar"))
		    (:div :id "progress-container" :style "display: none;"
			  (:progress :id "upload-progress" :value "0" :max "100"))
		    (:div :id "loading-indicator" :style "display: none;"
			  "Submitting... Please wait.")
		    (:div :id "error-container" :style "display: none;"
			  (:progress :id "error-progress" :value "100" :style "color: #FF6060"))
		    (:div :id "error-indicator" :style "display: none; color: #FF6060"
			  "An error occurred, please try again.")
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))

(define-easy-handler (convert-excel-to-pdf-route
		      :uri (define-matching-functions "^/convert-(excel|xlsx|xls)-to-pdf$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let ((files (post-parameters*))
	(uuid (to-string (make-v4))))
    (convert-format-to-format "pdf" uuid files)
    (jzon:stringify (hash-create `(("directory" ,uuid)
				   ("success" t))))))

(define-easy-handler (process-excel-to-pdf
		      :uri (define-matching-functions "^/(excel|xlsx|xls)-to-pdf/([^/]+)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((dir-list (str:split "/" (script-name*)))
	 (dir (caddr dir-list))
	 (from (car (str:split "-to-" (cadr dir-list))))
	 (from-capital (str:upcase from)))
    (with-html-output-to-string (*standard-output*)
      (:html :lang "en"
	     (:head
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:link :rel "manifest" :href "/spotpdf/manifest.json")
	      (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")

	      (:title (cl-who:fmt "Convert ~a sheets to PDF Documents. FAST. FREE. ONLINE. " from-capital))
	      (:meta :name "description" :content (format nil "Convert ~a sheets to PDF Documents in seconds. ONLINE. FREE. ACCURATE" from-capital))
	      (:meta :name "keywords" :content (format nil "spotpdf spot pdf convert ~a to word, online, free." from))
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))     
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to PDF." from-capital))
		    (:p "Your files have been converted to PDF.")
		    (:a :class "download-btn" :target "_blank" :href (format nil "/download-file/~a" dir) (:button "Download now."))
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))



;;; Epub/MOBI/AZW3/PDF TO PDF/Epub/Mobi/AZW3

(define-easy-handler (epub-to-pdf-route
		      :uri (define-matching-functions "^/(mobi|azw3|epub|pdf|pdb|fb2|lit|txt|rtf|docx)-to-(mobi|azw3|epub|pdf|pdb|fb2|lit|txt|rtf|docx)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (with-html-output-to-string (*standard-output*)
    (let* ((from-to (str:split "-to-" (script-name*)))
	   (from (str:replace-first "/" "" (car from-to)))
	   (from-capital (str:upcase from))
	   (to (cadr from-to))
	   (to-capital (str:upcase to)))
      (htm (:html :lang "en"
		  (:head
		   (:meta :charset "UTF-8")
		   (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
		   (:link :rel "manifest" :href "/spotpdf/manifest.json")
		   (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
		   (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")

		   (:title (cl-who:fmt "Convert ~a to ~a. FAST. FREE. ONLINE." from-capital to-capital))
		   (:meta :name "description" :content (format nil "Convert ~a to ~a in seconds. FAST. FREE. ONLINE" from-capital to-capital))
		   (:meta :name "keywords" :content (format nil "spotpdf spot pdf convert ~a to ~a, online, free." from to))
		   (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
		   (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	     	   (:style (str (home-css))))
		  (:body
		   (:div :class "main"
			 (header)
			 (:h1 (cl-who:fmt "Convert ~a to ~a" from-capital to-capital))
			 (:p (cl-who:fmt "Convert your ~a files to ~a." from-capital to-capital))

			 (:div :id "drop-zone" :class "drop-zone" (cl-who:fmt "Drag and drop files here or click the Choose ~a button" from-capital))
			 (:input :type "file" :id "file-input" :style "display: none;" :accept (format nil ".~a" from) :multiple t)
			 (:div :id "files-container")
			 (:div :class "btns"
			       (:button :class "upload-btn" :id "upload-btn"
					(:span :class "add-symbol" "+")
					(cl-who:fmt "Choose ~a" from-capital))
			       (:button :class "submit-btn" :id "submit-btn" (cl-who:fmt "Convert to ~a" to-capital)))
			 (:div :id "loading-container" :class "loading-container" :style "display: none;"
			       (:div :class "bar")
			       (:div :class "bar")
			       (:div :class "bar"))
			 (:div :id "progress-container" :style "display: none;"
			       (:progress :id "upload-progress" :value "0" :max "100"))
			 (:div :id "loading-indicator" :style "display: none;"
			       "Submitting... Please wait.")
			 (:div :id "error-container" :style "display: none;"
			       (:progress :id "error-progress" :value "100" :style "color: #FF6060"))
			 (:div :id "error-indicator" :style "display: none; color: #FF6060"
			       "An error occurred, please try again.")
			 (:script (str (home-js))))
		   (:div :class "ad")
		   (footer)
		   (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js")))))))


(define-easy-handler (convert-epub-to-pdf-route
		      :uri (define-matching-functions "^/convert-(mobi|azw3|epub|pdf|pdb|fb2|lit|txt|rtf|docx)-to-(mobi|azw3|epub|pdf|pdb|fb2|lit|txt|rtf|docx)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let ((files (post-parameters*))
	(uuid (to-string (make-v4)))
	(to (cadr (str:split "-to-" (script-name*)))))
    (ebook-convert to uuid files)
    (jzon:stringify (hash-create `(("directory" ,uuid)
				   ("success" t))))))

(define-easy-handler (process-epub-to-pdf
		      :uri (define-matching-functions "^/(mobi|azw3|epub|pdf|pdb|fb2|lit|txt|rtf|docx)-to-(mobi|azw3|epub|pdf|pdb|fb2|lit|txt|rtf|docx)/([^/]+)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((to-from (str:split "-to-" (cadr (str:split "/" (script-name*)))))
	 (to (cadr to-from))
	 (to-capital (str:upcase to))
	 (from (car to-from))
	 (from-capital (str:upcase from))
	 (dir (caddr (str:split "/" (script-name*)))))
    (with-html-output-to-string (*standard-output*)
      (:html :lang "en"
	     (:head
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:link :rel "manifest" :href "/spotpdf/manifest.json")
	      (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")

	      (:title (cl-who:fmt "Convert ~a to ~a. FAST. FREE. ONLINE." from-capital to-capital))
	      (:meta :name "description" :content (format nil "Convert ~a to ~a in seconds. FAST. FREE. ONLINE." from-capital to-capital))
	      (:meta :name "keywords" :content (format nil "spotpdf spot document convert ~a to ~a, online, free." from to))
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))     
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to ~a." from-capital to-capital))
		    (:p (cl-who:fmt "Your files have been converted to ~a." to-capital))
		    (:a :class "download-btn" :target "_blank" :href (format nil "/download-file/~a" dir) (:button "Download now."))
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))

;;; JPEG/JPG/PNG/TIFF/WEBP TO JPEG/JPG/PNG/TIFF/WEBP

(define-easy-handler (image-to-image-route
		      :uri (define-matching-functions "^/(jpeg|jpg|png|tiff|webp)-to-(jpeg|jpg|png|tiff|webp)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (with-html-output-to-string (*standard-output*)
    (let* ((from-to (str:split "-to-" (script-name*)))
	   (from (str:replace-first "/" "" (car from-to)))
	   (from-capital (str:upcase from))
	   (to (cadr from-to))
	   (to-capital (str:upcase to)))
      (htm (:html :lang "en"
		  (:head
		   (:meta :charset "UTF-8")
		   (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
		   (:link :rel "manifest" :href "/spotpdf/manifest.json")
		   (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
		   (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")

		   (:title (cl-who:fmt "Convert ~a to ~a. FAST. FREE. ONLINE." from-capital to-capital))
		   (:meta :name "description" :content (format nil "Convert ~a to ~a in seconds. FAST. FREE. ONLINE" from-capital to-capital))
		   (:meta :name "keywords" :content (format nil "spotpdf image convert ~a to ~a, online, free." from to))
		   (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
		   (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	     	   (:style (str (home-css))))
		  (:body
		   (:div :class "main"
			 (header)
			 (:h1 (cl-who:fmt "Convert ~a to ~a" from-capital to-capital))
			 (:p (cl-who:fmt "Convert your ~a files to ~a." from-capital to-capital))

			 (:div :id "drop-zone" :class "drop-zone" (cl-who:fmt "Drag and drop files here or click the Choose ~a button" from-capital))
			 (:input :type "file" :id "file-input" :style "display: none;" :accept (format nil ".~a" from) :multiple t)
			 (:div :id "files-container")
			 (:div :class "btns"
			       (:button :class "upload-btn" :id "upload-btn"
					(:span :class "add-symbol" "+")
					(cl-who:fmt "Choose ~a" from-capital))
			       (:button :class "submit-btn" :id "submit-btn" (cl-who:fmt "Convert to ~a" to-capital)))
			 (:div :id "loading-container" :class "loading-container" :style "display: none;"
			       (:div :class "bar")
			       (:div :class "bar")
			       (:div :class "bar"))
			 (:div :id "progress-container" :style "display: none;"
			       (:progress :id "upload-progress" :value "0" :max "100"))
			 (:div :id "loading-indicator" :style "display: none;"
			       "Submitting... Please wait.")
			 (:div :id "error-container" :style "display: none;"
			       (:progress :id "error-progress" :value "100" :style "color: #FF6060"))
			 (:div :id "error-indicator" :style "display: none; color: #FF6060"
			       "An error occurred, please try again.")
			 (:script (str (home-js))))
		   (:div :class "ad")
		   (footer)
		   (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js")))))))

(define-easy-handler (convert-image-to-image-route
		      :uri (define-matching-functions "^/convert-(jpeg|jpg|png|tiff|webp)-to-(jpeg|jpg|png|tiff|webp)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let ((files (post-parameters*))
	(uuid (to-string (make-v4)))
	(to (cadr (str:split "-to-" (script-name*)))))
    (image-convert to uuid files)
    (jzon:stringify (hash-create `(("directory" ,uuid)
				   ("success" t))))))

(define-easy-handler (process-image-to-image
		      :uri (define-matching-functions "^/(jpeg|jpg|png|tiff|webp)-to-(jpeg|jpg|png|tiff|webp)/([^/]+)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((to-from (str:split "-to-" (cadr (str:split "/" (script-name*)))))
	 (to (cadr to-from))
	 (to-capital (str:upcase to))
	 (from (car to-from))
	 (from-capital (str:upcase from))
	 (dir (caddr (str:split "/" (script-name*)))))
    (with-html-output-to-string (*standard-output*)
      (:html :lang "en"
	     (:head
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:link :rel "manifest" :href "/spotpdf/manifest.json")
	      (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")

	      (:title (cl-who:fmt "Convert ~a to ~a. FAST. FREE. ONLINE." from-capital to-capital))
	      (:meta :name "description" :content (format nil "Convert ~a to ~a in seconds. FAST. FREE. ONLINE." from-capital to-capital))
	      (:meta :name "keywords" :content (format nil "spotpdf spot image convert ~a to ~a, online, free." from to))
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))     
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to ~a." from-capital to-capital))
		    (:p (cl-who:fmt "Your files have been converted to ~a." to-capital))
		    (:a :class "download-btn" :target "_blank" :href (format nil "/download-file/~a" dir) (:button "Download now."))
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))


;;; PNG/JPEG/JPG TO PDF

(define-easy-handler (image-to-pdf
		      :uri (define-matching-functions "^/(image|png|jpg|jpeg|webp)-to-pdf$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((to (str:replace-all "/" "" (car (str:split "-to-" (script-name*)))))
	 (to-capital (str:upcase to)))
    (with-html-output-to-string (*standard-output*)
      (:html :lang "en"
	     (:head
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:link :rel "manifest" :href "/spotpdf/manifest.json")
	      (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")
	      (:title (cl-who:fmt "Convert ~a to PDF. FAST. FREE. ONLINE." to-capital))
	      (:meta :name "description" :content (format nil "Convert ~a images to PDFs in seconds. FAST. FREE. ONLINE" to-capital))
	      (:meta :name "keywords" :content (format nil "spotpdf spot image convert ~a to pdf,  online, free." to))
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to PDF" to-capital))
		    (:p (cl-who:fmt "Convert your ~a documents to PDFs." to-capital))

		    (:div :id "drop-zone" :class "drop-zone" (cl-who:fmt "Drag and drop files here or click the Choose ~a button" to-capital))
		    (:input :type "file" :id "file-input" :style "display: none;" :accept (if (string= "image" to)
											      ".png, .jpeg, .jpg, .webp"
											      (format nil ".~a" to))
		     :multiple t)
		    (:div :id "files-container")
		    (:div :class "btns"
			  (:button :class "upload-btn" :id "upload-btn"
				   (:span :class "add-symbol" "+")
				   (cl-who:fmt "Choose ~a" to-capital))
			  (:button :class "submit-btn" :id "submit-btn" "Convert to PDF"))
		    (:div :id "loading-container" :class "loading-container" :style "display: none;"
			  (:div :class "bar")
			  (:div :class "bar")
			  (:div :class "bar"))
		    (:div :id "progress-container" :style "display: none;"
			  (:progress :id "upload-progress" :value "0" :max "100"))
		    (:div :id "loading-indicator" :style "display: none;"
			  "Submitting... Please wait.")
		    (:div :id "error-container" :style "display: none;"
			  (:progress :id "error-progress" :value "100" :style "color: #FF6060"))
		    (:div :id "error-indicator" :style "display: none; color: #FF6060"
			  "An error occurred, please try again.")
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))

(define-easy-handler (convert-image-to-pdf-route
		      :uri (define-matching-functions "^/convert-(image|jpeg|jpg|png|webp)-to-pdf$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let ((files (post-parameters*))
	(uuid (to-string (make-v4))))
    (convert-image-to-pdf uuid files)
    (jzon:stringify (hash-create `(("directory" ,uuid)
				   ("success" t))))))

(define-easy-handler (process-image-to-pdf
		      :uri (define-matching-functions "^/(image|jpeg|jpg|png|webp)-to-pdf/([^/]+)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((dir-list (str:split "/" (script-name*)))
	 (dir (caddr dir-list))
	 (from (car (str:split "-to-" (cadr dir-list))))
	 (from-capital (str:upcase from)))
    (with-html-output-to-string (*standard-output*)
      (:html :lang "en"
	     (:head
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:link :rel "manifest" :href "/spotpdf/manifest.json")
	      (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")

	      (:title (cl-who:fmt "Convert ~a to PDF. FAST. FREE. ONLINE." from-capital))
	      (:meta :name "description" :content (format nil "Convert ~a images to PDFs in seconds. FAST. FREE. ONLINE" from-capital))
	      (:meta :name "keywords" :content (format nil "spotpdf spot pdf convert ~a to pdf,  online, free." from))
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))     
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to PDF" from-capital))
		    (:p "Your files have been converted to PDF.")
		    (:a :class "download-btn" :target "_blank" :href (format nil "/download-file/~a" dir) (:button "Download now."))
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))



;;; PDF TO PNG/JPEG/JPG/TIFF

(define-easy-handler (pdf-to-image-route
		      :uri (define-matching-functions "^/pdf-to-(image|jpeg|jpg|png|tiff)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((to (cadr (str:split "-to-" (script-name*))))
	 (to-capital (str:upcase to)))
    (with-html-output-to-string (*standard-output*)
      (:html :lang "en"
	     (:head
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:link :rel "manifest" :href "/spotpdf/manifest.json")
	      (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")
	      (:title (cl-who:fmt "Convert PDF to ~a images. FAST. FREE. ONLINE." to-capital))
	      (:meta :name "description" :content (format nil "Convert PDF to ~a images in seconds. FAST. FREE. ONLINE" to-capital))
	      (:meta :name "keywords" :content (format nil "spotpdf spot pdf convert pdf to ~a,  online, free." to))
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert PDF to ~a images" to-capital))
		    (:p (cl-who:fmt "Convert your PDFs to ~a images" to-capital))

		    (:div :id "drop-zone" :class "drop-zone" "Drag and drop files here or click the Choose PDF button")
		    (:input :type "file" :id "file-input" :style "display: none;" :accept ".pdf"
		     :multiple t)
		    (:div :id "files-container")
		    (:div :class "btns"
			  (:button :class "upload-btn" :id "upload-btn"
				   (:span :class "add-symbol" "+")
				   "Choose PDF")
			  (:button :class "submit-btn" :id "submit-btn" (cl-who:fmt "Convert to ~a" to-capital)))
		    (:div :id "loading-container" :class "loading-container" :style "display: none;"
			  (:div :class "bar")
			  (:div :class "bar")
			  (:div :class "bar"))
		    (:div :id "progress-container" :style "display: none;"
			  (:progress :id "upload-progress" :value "0" :max "100"))
		    (:div :id "loading-indicator" :style "display: none;"
			  "Submitting... Please wait.")
		    (:div :id "error-container" :style "display: none;"
			  (:progress :id "error-progress" :value "100" :style "color: #FF6060"))
		    (:div :id "error-indicator" :style "display: none; color: #FF6060"
			  "An error occurred, please try again.")
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))

(define-easy-handler (convert-pdf-to-image
		      :uri (define-matching-functions "^/convert-pdf-to-(image|jpeg|jpg|png|tiff)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let ((files (post-parameters*))
	(uuid (to-string (make-v4)))
	(to (cadr (str:split "-to-" (script-name*)))))
    (pdf-to-image-convert to uuid files)
    (jzon:stringify (hash-create `(("directory" ,uuid)
				   ("success" t))))))

(define-easy-handler (process-pdf-to-image
		      :uri (define-matching-functions "^/pdf-to-(image|jpeg|jpg|png|tiff)/([^/]+)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let* ((dir-list (str:split "/" (script-name*)))
	 (dir (caddr dir-list))
	 (to (cadr (str:split "-to-" (cadr dir-list))))
	 (to-capital (str:upcase to))
	 )
    (with-html-output-to-string (*standard-output*)
      (:html :lang "en"
	     (:head
	      (:meta :charset "UTF-8")
	      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
	      (:link :rel "manifest" :href "/spotpdf/manifest.json")
	      (:link :rel "icon" :href "/spotpdf/static/icons/web/favicon.ico" :sizes "any")
	      (:link :rel "apple-touch-icon" :href "/spotpdf/static/icons/web/apple-touch-icon.png")

	      (:title (cl-who:fmt "Convert PDF to ~a. FAST. FREE. ONLINE." to-capital))
	      (:meta :name "description" :content (format nil "Convert PDFs to ~a images in seconds. FAST. FREE. ONLINE" to-capital))
	      (:meta :name "keywords" :content (format nil "spotpdf spot pdf convert pdf to ~a,  online, free." to))
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))     
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert PDF to ~a" to-capital))
		    (:p (cl-who:fmt "Your files have been converted to ~a." to-capital))
		    (:a :class "download-btn" :target "_blank" :href (format nil "/download-file/~a" dir) (:button "Download now."))
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))

;;;============ CONVERSION FUNCTIONS ===========================

(defun pdf-to-format (dir file-path format &aux (infilter (trivia:match format
							    ("pptx" "impress_pdf_import")
							    ("ppt" "impress_pdf_import")
							    ("docx" "writer_pdf_import")
							    ("doc" "writer_pdf_import")
							    )))
  "convert a file pdf to a given format
  dir is the uuid dir name for the request."
  (let ((cmd (format nil "/usr/bin/libreoffice --headless --infilter=~s --convert-to ~a --outdir ~s ~s"
		     infilter format (namestring (truename dir)) (namestring (truename file-path)))))
    (uiop:run-program cmd)))

(defun convert-pdf-to-format (format uuid post-parameters &aux (dir (format nil "~~/common-lisp/ninx/apps/spotpdf/files/~a/" uuid)))
  "given a list of post parameters, create a directory for them at uuid.
   copy all files to it, then convert them to pptx, remove the pdf files,
   and return after that."
  (ensure-directories-exist dir)
  (dolist (param post-parameters)
    (trivia:match param
      ((list _ path file-name _)
       (let ((pdf-path (version-name dir file-name)))
	 (uiop:copy-file path pdf-path)
	 (pdf-to-format dir pdf-path format)))
      (_ nil)))
  (delete-dir-files dir format))

(defun ebook-convert-fn (pdf-path epub-path)
  "convert a file pdf to a given format
  dir is the uuid dir name for the request."
  (let* ((cmd (format nil "/usr/bin/ebook-convert ~a ~a --enable-heuristics" pdf-path epub-path)))
    (uiop:run-program cmd)))

(defun ebook-convert (to uuid post-parameters &aux (dir (format nil "~~/common-lisp/ninx/apps/spotpdf/files/~a/" uuid)))
  "given a list of post parameters, create a directory for them at uuid.
   copy all files to it, then convert them to pptx, remove the pdf files,
   and return after that."
  (ensure-directories-exist dir)
  (dolist (param post-parameters)
    (trivia:match param
      ((list _ path file-name _)
       (let* ((from-path (version-name dir file-name))
	      (to-path (format nil "~a~a.~a" dir (pathname-name from-path) to)))
    	 (uiop:copy-file path from-path)
	 (ebook-convert-fn from-path to-path)))
      (_ nil)))
  (delete-dir-files dir to))

(defun convert-image-to-pdf-fn (string-of-files pdf-path)
  "convert a file pdf to a given format
  dir is the uuid dir name for the request."
  (let* ((cmd (format nil "/usr/bin/img2pdf -o ~a ~a" pdf-path string-of-files)))
    (uiop:run-program cmd)))

(defun convert-image-to-pdf (uuid post-parameters &aux (dir (format nil "~~/common-lisp/ninx/apps/spotpdf/files/~a/" uuid)))
  "given a list of post parameters, create a directory for them at uuid.
   copy all files to it, then convert them to pptx, remove the pdf files,
   and return after that.
   the pdf will be named spotpdf-image-to-pdf-dd-mm-yyyy.pdf or the name of the image if there's only one file"
  (ensure-directories-exist dir)
  (format *terminal-io* "~%post: ~a~%" post-parameters)
  ;; sort the files according to file-name, remember this is sent in an array in order of first file.
  ;; append each of the files to the list of files to be passed to the fn function
  (let* ((string-of-files "")
	 (image-files (remove-if (lambda (l) (string= "number-of-files" (car l))) post-parameters))
	 (pdf-name (if (> (length post-parameters) 2)
		       (format nil "spotpdf-~a.pdf" (ninx:get-dd-mm-yyyy))
		       ;; the remove if is to remove number-of-files
		       (format nil "~a.pdf"  (pathname-name (caddar image-files)))))
	(pdf-path (format nil "~a~a" dir pdf-name)))
      (format *terminal-io* "~%post1: ~a~%~%" post-parameters)
    (dolist (param (sort image-files #'string< :key #'caddr))
      (trivia:match param
	((list _ path file-name _)
	 (let* ((new-image-path (version-name dir file-name)))
    	   (uiop:copy-file path new-image-path)
	   (setq string-of-files (format nil "~a ~a" string-of-files (namestring (truename new-image-path))))))
	(_ nil)))
    (convert-image-to-pdf-fn string-of-files pdf-path))
  (delete-dir-files dir "pdf"))

(defun get-downloadable-data (dir &aux (path (format nil "~~/common-lisp/ninx/apps/spotpdf/files/~a/" dir)))
  "counts the number of files in a given directory, if it is 1, returns it and its content-type.
   if it has more than 1, then the files are compressed into a zip and that is returned to the user."
  (let* ((files (uiop:directory-files path))
	 (len (length files)))
    (cond
      ((= len 1)
       (list (cl-mime-from-string:mime-type-from-string (namestring (car files)))
	     (format nil "~a.~a" (pathname-name (car files)) (pathname-type (car files)))
	     (ninx::read-binary-file-to-octets (car files))))
      ((> len 1)
       (let* ((zip-dir (namestring (format nil "~~/common-lisp/ninx/apps/spotpdf/zip/~a/" dir)))
	      (zip-path (format nil "~aspotpdf.zip" zip-dir)))
	 (ensure-directories-exist zip-dir)
	 (uiop:run-program (format nil "/usr/bin/zip -r -j ~a ~a"
				   (namestring zip-path)
				   (namestring path)))
	 (list "application/zip"
	       "spotpdf.zip"
	       (ninx::read-binary-file-to-octets zip-path)))))))

(defun format-to-format (to dir file-path)
  "convert a given format to pdf
  dir is the uuid dir name for the request."
  (let ((cmd (format nil "/usr/bin/libreoffice --headless --convert-to ~a --outdir ~s ~s"
		     to (namestring (truename dir)) (namestring (truename file-path)))))
    (uiop:run-program cmd)))

(defun convert-format-to-format (to uuid post-parameters &aux (dir (format nil "~~/common-lisp/ninx/apps/spotpdf/files/~a/" uuid)))
  "given a list of post parameters, create a directory for them at uuid.
   copy all files to it, then convert them to pptx, remove the pdf files,
   and return after that."
  (ensure-directories-exist dir)
  (dolist (param post-parameters)
    (trivia:match param
      ((list _ path file-name _)
       (let ((pdf-path (version-name dir file-name)))
    	 (uiop:copy-file path pdf-path)
	 (format-to-format to dir pdf-path)))
      (_ nil)))
  (delete-dir-files dir to))

(defun image-convert-fn (from-path to-path)
  "convert a file pdf to a given format
  dir is the uuid dir name for the request."
  (let* ((cmd (format nil "/usr/bin/convert ~a ~a" from-path to-path)))
    (uiop:run-program cmd)))

(defun image-convert (to uuid post-parameters &aux (dir (format nil "~~/common-lisp/ninx/apps/spotpdf/files/~a/" uuid)))
  "given a list of images, convert them to a given format."
  (ensure-directories-exist dir)
  (dolist (param post-parameters)
    (trivia:match param
		  ((list _ path file-name _)
		   (let* ((from-path (version-name dir file-name))
			  (to-path (format nil "~a~a.~a" dir (pathname-name from-path) to)))
    		     (uiop:copy-file path from-path)
		     (image-convert-fn from-path to-path)))
		  (_ nil)))
  (delete-dir-files dir to))
;;mkdir -p images && pdftoppm -jpeg -jpegopt quality=100 -r 300 mypdf.pdf images/pg

(defun pdf-to-image-convert-fn (to pdf-path dir)
  "convert a file pdf to a given format
  dir is the uuid dir name for the request."
  (let* ((to-cmd (cond ((or (string= to "jpg") (string= to "jpeg")) "jpeg -jpegopt quality=100")
		       (t to)))
	 (cmd (format nil "/usr/bin/pdftoppm -~a -r 300 ~a ~apage" to-cmd pdf-path dir)))
    (format t "~%cmd: ~a~%" cmd)
    (uiop:run-program cmd)))

(defun pdf-to-image-convert (to uuid post-parameters &aux (dir (format nil "~~/common-lisp/ninx/apps/spotpdf/files/~a/" uuid)))
  "given a list of images, convert them to a given format."
  (ensure-directories-exist dir)
  (dolist (param post-parameters)
    (trivia:match param
		  ((list _ path file-name _)
		   (let* ((name (pathname-name file-name))
			  (pdf-path (format nil "~a~a.pdf" dir name)))
    		     (uiop:copy-file path pdf-path)
		     (pdf-to-image-convert-fn to (namestring (truename pdf-path)) (namestring (truename dir)))))
		  (_ nil)))
  ;; replace the jpeg because only files with jpg will be exported.
  (delete-dir-files dir (ppcre:regex-replace-all "jpeg" to "jpg")))

(defun delete-dir-files (dir type)
  "delete files not of type from dir"
  (dolist (file (uiop:directory-files dir))
    (when (null (ppcre:scan (format nil "(.~a)$" type) (namestring file)))
      (delete-file file))))

(defun version-name (dir file-name)
  "return a file with a version; the returned version-name is a pathspec"
  (let* ((pdf-path-1 (format nil "~a~a" dir file-name))
	 (pdf-path (let ((version (version-file pdf-path-1)))
		     (format nil "~a~a" dir (format nil "~a~a"
						    (pathname-name file-name)
						    (if (equal 0 version)
							(format nil ".~a" (pathname-type file-name))
							(format nil "_~a.~a"
								version
								(pathname-type file-name))))))
		   ))
    pdf-path))

(defun version-file (pathspec)
  "this will give a number to a file eg test_1.txt depending on how many files have 'name'_ver.ext"
  (let* ((type (pathname-type pathspec))
	 (file (pathname-name pathspec)))
    (count-if
     (lambda (spec)
       (ppcre:scan (format nil "^(.*?/)?(~a(_[0-9]+)?).~a$" file type) (namestring spec)))
     (uiop:directory-files (truename (ppcre:regex-replace-all (format nil "~a.~a" file type) (namestring pathspec) ""))))))

(defun clean-up-files ()
  "delete folders having files created atleast 2 hours ago, this will run every two hours deleting files.
   this is essential to prevent buildup of unneccessary files."
  (let ((dirs (directory "~/common-lisp/ninx/apps/spotpdf/files/*")))
    (dolist (dir dirs)
      (let* ((files (directory dir))
	     (t-now (get-universal-time)))
	(when files
	  (when (< (file-write-date (car files)) (- t-now 7200))
	    (delete-directory dir :recursive t)))))))

(defun schedule-cleanup ()
  "run the cleanup every two hours."
  (let ((timer (sb-ext:make-timer #'spotpdf::clean-up-files :name 'file-cleanup :thread t)))
    (sb-ext:schedule-timer timer 7200 :repeat-interval 7200)
    timer))
