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
      "this function will handle addition of files when a user submits or drops them"
      (let ((files-container (ps:chain document (get-element-by-id "files-container"))))
	(ps:chain -array (from files)
		  (for-each
		   (lambda (file)
		     ;; first add the file to form-data created above
		     (ps:chain files-array (push file))
		     (let ((file-frame (ps:chain document (create-element "div")))
			   (close-button (ps:chain document (create-element "button")))
			   (frame-id (generate-random-id))
			   (btn-id (generate-random-id)))
		       ;; handle frame
		       (ps:chain file-frame class-list (add "file-frame"))
		       (ps:chain file-frame (set-attribute "id" frame-id))
		       (ps:chain file-frame (set-attribute "draggable" "true")) ;; Allow frames to be draggable

		       ;; handle close button
		       (ps:chain close-button class-list (add "close-btn"))
		       (ps:chain close-button (set-attribute "id" btn-id))
		       (setf (ps:chain close-button inner-h-t-m-l) "&times;")
		       (ps:chain close-button (add-event-listener "click"
								  (lambda ()
								    ;; remove the file from files-array
								    (let ((index (ps:chain files-array (index-of file))))
								      (if (> index -1)
									  (ps:chain files-array (splice index 1))))
								    (ps:chain file-frame (remove))))))
 		     (cond
		       ((ps:chain file type (starts-with "image/"))
			(let ((img (ps:chain document (create-element "img")))
			      (reader (new -file-reader)))
			  (setf (ps:chain reader onload) (lambda (e) (setf (ps:chain img src) (ps:chain e target result))))
			  (ps:chain reader (read-as-data-u-r-l file))
			  (ps:chain file-frame (append-child img))))
		       ((eql (ps:chain file type) "application/pdf")
			(let ((iframe (ps:chain document (create-element "iframe")))
			      (file-name (ps:chain document (create-element "span"))))
			  (setf (ps:chain iframe src) (ps:chain -u-r-l (create-object-u-r-l file)))
			  (ps:chain file-frame (append-child iframe))
			  ;; file name
			  (ps:chain file-name class-list (add "file-name"))
			  (setf (ps:chain file-name text-content) (ps:chain file name (to-lower-case)))
			  (ps:chain file-frame (append-child file-name)))))
		     (ps:chain file-frame (append-child close-button))
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
				    (setf (ps:chain document (get-element-by-id "success-indicator") style display) "none")
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

    (defun show-toast (message duration)
      "show a toast to the user"
      (let ((toast-container (ps:chain document (get-element-by-id "toast-container")))
	    (toast (ps:chain document (create-element "div"))))
	(ps:chain toast class-list (add "toast"))
	(setf (ps:chain toast text-content) message)
	(ps:chain toast-container (append-child toast))
	(set-timeout (lambda () "remove the toast"
		       (ps:chain toast class-list (add "fade-out"))
		       (set-timeout (lambda () "wait fo the fade-out to complete"
				      (ps:chain toast (remove))) 500))
		     duration)))

    
    ;; Upload files to the "/files" endpoint using plain JS
    (let ((submit-btn (ps:chain document (get-element-by-id "submit-btn")))
	  (progress-container (ps:chain document (get-element-by-id "progress-container")))
	  (progress-bar (ps:chain document (get-element-by-id "upload-progress")))
	  (loading-indicator (ps:chain document (get-element-by-id "loading-indicator")))
	  (error-container (ps:chain document (get-element-by-id "error-container")))
	  (error-indicator (ps:chain document (get-element-by-id "error-indicator")))
	  (success-indicator (ps:chain document (get-element-by-id "success-indicator")))
	  (loading-container (ps:chain document (get-element-by-id "loading-container")))
	  )
      (defun base64-to-array (base64data)
	"This function processes base64data returned from the server and converts it into a Uint8Array."
	(let* ((binary-string (ps:chain window (atob base64data)))
               (len (ps:chain binary-string length))
               (bytes (new (-uint8-array len)))
               (i 0))
	  (loop for i from 0 to len do
	    (setf (aref bytes i) (ps:chain binary-string (char-code-at i))))
	  bytes))

      (defun download-file (blob name)
	"This function triggers the download of the file returned by the server. Shows a toast with the filename,
   hides the progress indicator, and resets forms and file array."
	(let ((blob-data (new (-blob (array (base64-to-array blob)) 
				     (create :type "application/vnd.openxmlformats-officedocument.presentationml.presentation"))))
              (link (ps:chain document (create-element "a")))
              (filename (+ name ".pptx")))
	  
	  (setf (ps:chain link href) (ps:chain window -u-r-l (create-object-u-r-l blob-data)))
	  (setf (ps:chain link download) filename)
	  (ps:chain document body (append-child link))
	  (ps:chain link (click))
	  (ps:chain document body (remove-child link))
	  (setf (ps:chain progress-container style display) "none")
	  (setf (ps:chain loading-indicator style display) "none")
	  (setf (ps:chain loading-container style display) "none")
	  (let ((txt (+ "Your deck is saved as " filename " in your downloads folder as " name ".")))
	    (show-toast txt 500)
	    (setf (ps:chain success-indicator inner-h-t-m-l) txt))
	  
	  (setf (ps:chain success-indicator style display) "block")
	  (setf files-array (array))
	  (setf (ps:chain document (get-element-by-id "description") value) "")
	  (setf (ps:chain document (get-element-by-id "files-container") inner-h-t-m-l) ""))
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
		      (setf (ps:chain loading-indicator inner-h-t-m-l) "Generating deck, sit back and wait...")))))

	  (setf (ps:chain xhr onloadstart) (lambda ()
					     "Show loading indicator when uploading starts"
					     (setf (ps:chain loading-container style display) "none")
					     (setf (ps:chain success-indicator style display) "none")
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

			    ;;		    (setf (ps:chain window location href) (+ "/pdf-to-pptx/" (ps:chain response directory)))
			    ;; (progn (download-file (ps:chain response data) (ps:chain response title))
			    ;; 	   (incr-user-doc-count)
			    ;; 	   (change-user-balance (ps:chain response balance)))
			    (progn
			      (ps:chain console (log response))
			      (show-toast "An error occured during making the deck, please try again" 3000)
			      (let* ((error-code (ps:chain response error-code))
				     (error-text (cond
						   ((eql error-code 429) "Server currently busy, please try again later.")
						   ((eql error-code 500) "Your documents are too long, they will take forever to process. Decrease the number and/or size of the documents and try again.")
						   ((eql error-code 503) "Server currently experiencing some technical problems. Try again later.")
						   ((eql error-code 504) "The documents are taking very long to process, won't finish in time. Decrease number and/or size of the documents and try again.")
						   (t "An error occurred while processing the request. Please try again.")))))
			      (setf (ps:chain progress-container style display) "none")
			      (setf (ps:chain loading-indicator style display) "none")
			      (setf (ps:chain loading-container style display) "none")
			      (setf (ps:chain error-indicator inner-h-t-m-l) error-text)
			      (setf (ps:chain error-container style display) "block")
			      (setf (ps:chain error-indicator style display) "block")
			      )))
		      (progn (show-toast "An error occured during making the deck, please try again" 3000)
			     (setf (ps:chain loading-container style display) "none")
			     (setf (ps:chain progress-container style display) "none")
			     (setf (ps:chain loading-indicator style display) "none")
			     (setf (ps:chain error-container style display) "block")
			     (setf (ps:chain error-indicator style display) "block")
			     ))))

	  ;; Send the request only if there's a description or files to send
	  (when (> (length files-array) 0)
	    (ps:chain xhr (send form-data))
	    (show-toast "Data is being submitted" 1500))))

      (ps:chain submit-btn (add-event-listener "click" (lambda ()
							 "submit button click listener"
							 (upload-files)))))))

(defun home-css ()
  "the css for the /home endpoint"
  (let ((fg-color "#1a1a1a")
	(red "#FF6060")
	(blue "#CCCCFF")
	(bg-color "#e8e8e8")
	(link-blue "#1e90ff"))
    (cl-css:css
     `((body :text-align center :background-color ,bg-color :color ,fg-color :margin 20px :font-size 16px)
       (button :background-color "#00b800" :border "none" :color "#e8e8e8" :padding "10px 20px" :text-align "center" :text-decoration "none" :display "inline-block" :font-size "16px" :margin "4px 2px" :cursor "pointer" :border-radius "12px" :font-weight bold)
       (.submit-btn :float right :background-color "#00b800")
       (.upload-btn :float left)
       (.add-symbol :margin-right 8px :font-size :16px :font-weight bold)
       ("button:hover" :background-color "#45a049")
       (.file-frame :display inline-block :margin 10px :padding 10px :border "1px solid #1a1a1a" :position relative :text-align center :max-width "150px")
       (.file-name :font-size "12px" :color ,fg-color :margin-top "5px" :word-wrap "normal" :max-width "150px" :overflow "hidden" :text-overflow "ellipsis")
       (.close-btn :position absolute :top 5px :right 5px :background-color red :color white :border none :cursor pointer)
       ("img, iframe" :max-width 150px :max-height 150px)
       (.drop-zone :width "100%" :height "200px" :border "2px dashed #1a1a1a" :display "flex" :align-items "center" :justify-content "center" :text-align "center" :margin "20px 0" :cursor "pointer")
       (.drop-zone.dragging :border-color "#666")
       (h2 :color ,fg-color)
       (p :text-align "justify")
       (.separator :color ,link-blue)
       (.error-p :color ,red)
       (.top :font-weight bold)
       (.buy-tokens-large :font-size 21px :font-weight bold)
       (footer :text-align center :margin-top 10%)
       (a :color ,link-blue :text-decoration none :decoration none :margin-left 10px :margin-right 10px)
       (a.dropdown-item :color ,link-blue :text-decoration none :decoration none :margin-left 10px :margin-right 10px)
       ("a:hover::after" :color ,link-blue)
       ("a:visited" :color ,link-blue :decoration none)
       ("a:hover" :color ,link-blue :decoration underline)
					;  ("a:not(.logo-link)::after" :content "\"↪\"" :font-weight "bold" :color "inherit" :vertical-align baseline)
       (.logo :display flex :justify-content center :flex-direction :row :align-items :center :gap "10px")
       ("a.logo-link" :text-align center :color ,link-blue :text-decoration none :font-size 30px)
       (.logo-image :width 50px)
       ("span.balance" :color ,fg-color)
       ("a.feedback" :font-size 20px :font-weight bold)
       ("a.logo-link:hover" :color ,link-blue)
       (".copyright" :color ,fg-color :text-align center)
       (.toast-container :position "fixed" :bottom "20px" :left "50%" :transform "translateX(-50%)" :z-index "9999" :display "flex" :flex-direction "column" :align-items "center")
       (.toast :background-color "#333" :color "white" :padding "15px" :margin-top "10px" :border-radius "5px" :box-shadow "0px 4px 8px rgba(0, 0, 0, 0.1)" :opacity "0.9" :transition "opacity 0.3s ease" :max-width "80%" :word-wrap "break-word" :text-align "center")
       (".toast.fade-out" :opacity "0")
       (textarea :width 99.5% :margin-top 5px :border "1px solid #1a1a1a" :background-color ,bg-color :color ,fg-color :padding 5px)

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
       (footer :position absolute :width 80% :bottom 0 :height 60px :line-height 60px :background-color "#f5f5f5")
       ("@media only screen and (max-width: 768px)"
	(footer :margin-top 50% :text-align left :font-size 15px)
	("a.feedback" :font-size 16px :font-weight bold)
	(a :margin-left 2px :margin-right 2px)
	(.ad :width 95%)
	(".copyright" :color ,fg-color :text-align left))))))

(defparameter *file-types* '(("azw3" . ("docx" "epub" "fb2" "lit" "mobi" "pdf" "pdb" "rtf" "txt"))
			     ("doc" . ("pdf"))
			     ("docx" . ("azw3" "epub" "fb2" "lit" "mobi" "pdf" "pdb" "rtf" "txt"))
			     ("epub" . ("azw3" "docx" "fb2" "lit" "mobi" "pdf" "pdb" "rtf" "txt"))
			     ("excel" . ("pdf"))
			     ("fb2" . ("azw3" "docx" "epub" "lit" "mobi" "pdf" "pdb" "rtf" "txt"))
			     ("lit" . ("azw3" "docx" "epub" "fb2" "mobi" "pdf" "pdb" "rtf" "txt"))
			     ("mobi" . ("azw3" "docx" "epub" "fb2" "lit" "pdf" "pdb" "rtf" "txt"))
			     ("pdf" . ("azw3" "doc" "docx" "epub" "fb2" "lit" "mobi" "pdb" "powerpoint" "ppt" "pptx" "rtf" "txt" "word"))
			     ("pdb" . ("azw3" "docx" "epub" "fb2" "lit" "mobi" "pdf" "rtf" "txt"))
			     ("powerpoint" . ("pdf"))
			     ("ppt" . ("pdf"))
			     ("pptx" . ("pdf"))
			     ("rtf" . ("azw3" "docx" "epub" "fb2" "lit" "mobi" "pdf" "pdb" "txt"))
			     ("txt" . ("azw3" "docx" "epub" "fb2" "lit" "mobi" "pdf" "pdb" "rtf"))
			     ("word" . ("pdf"))
			     ("xls" . ("pdf"))
			     ("xlsx" . ("pdf"))))

(defun header ()
  (with-html-output (*standard-output*)
    (htm
     (:nav :class "navbar navbar-expand-lg"
	   (:div :class "container-fluid"
		 (:a :class "navbar-brand" :href "/"
		     (:img :src "/spotpdf/spotpdf-logo.png" :width "68" :height "60" :class "d-inline-block align-top" :alt ""))
		 (:button :class "navbar-toggler" :type "button" :data-bs-toggle "collapse" :data-bs-target "navbar-content"
			  :aria-controls "navbar-content" :aria-expanded "false" :aria-label "Toggle navigation"
			  (:span :class "navbar-toggler-icon"))
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
								    (htm (:a :href from-to :class from-to-class (cl-who:fmt "~a to ~a" from-capital (str:upcase to)))))))))))))))))))

(defun footer ()
  (with-html-output (*standard-output*)
    (htm (:footer :class "footer"
		  (:div :class "container"
			(:b (cl-who:fmt "© Ninx Technology Limited ~a - A one stop conversion site." (ninx:get-current-year))))))))

(define-easy-handler (home
		      :uri (define-matching-functions "^/$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (with-html-output-to-string (*standard-output*)
    (:html :lang "en"
	   (:head
	    (:title "SpotPDF. FAST. FREE. ONLINE.")
	    (:meta :name "description" :content "Convert documents from one format to another. FAST. FREE. ONLINE.")
	    (:meta :name "keywords" :content "spotpdf spot pdf convert pdf, word, excel, powerpoint.")
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
		   (header))
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
	      (:script :src "spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert PDF to ~a" to-capital))
		    (:p (cl-who:fmt "Convert your PDFs to ~a" to-capital))

		    (:div :id "drop-zone" :class "drop-zone" " Drag and drop files here or click the Add PDF button")
		    (:input :type "file" :id "file-input" :style "display: none;" :accept ".pdf" :multiple t)
		    (:div :id "files-container")
		    (:div :class "btns"
			  (:button :class "upload-btn" :id "upload-btn"
				   (:span :class "add-symbol" "+")
				   "Add PDF")
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
		    (:div :id "success-indicator" :style "display: none; color: #1e90ff"
			  "The deck has been created, downloaded and saved in downloads.")
		    (:div :id "toast-container" :class "toast-container")
		    
		    
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))

(define-easy-handler (convert-pdf-to-pptx-route
		      :uri (define-matching-functions "^/convert-pdf-to-(ppt|pptx|powerpoint)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let ((files (post-parameters*))
	(uuid (to-string (make-v4))))
    (convert-pdf-to-format "pptx" uuid files)
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
	      (:script :src "spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      
	      (:style (str (home-css))))
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert PDF to ~a" to-capital))
		    (:p (cl-who:fmt "Your files have been converted to ~a." to-capital))
		    (:button (:a :class "download-btn" :target "_blank" :href (format nil "/download-file/~a" dir) "Download now."))
		    
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
	      (:script :src "spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert PDF to ~a" to-capital))
		    (:p (cl-who:fmt "Convert your PDFs to ~a." to-capital))

		    (:div :id "drop-zone" :class "drop-zone" " Drag and drop files here or click the Add PDF button")
		    (:input :type "file" :id "file-input" :style "display: none;" :accept ".pdf" :multiple t)
		    (:div :id "files-container")
		    (:div :class "btns"
			  (:button :class "upload-btn" :id "upload-btn"
				   (:span :class "add-symbol" "+")
				   "Add PDF")
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
		    (:div :id "success-indicator" :style "display: none; color: #1e90ff"
			  "The deck has been created, downloaded and saved in downloads.")
		    (:div :id "toast-container" :class "toast-container")
		    
		    
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))

(define-easy-handler (convert-pdf-to-word-route
		      :uri (define-matching-functions "^/convert-pdf-to-(word|doc)$" *spotpdf-host*)
		      :host *spotpdf-host*) ()
  (let ((files (post-parameters*))
	(uuid (to-string (make-v4))))
    (convert-pdf-to-format "docx" uuid files)
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
	      (:script :src "spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert PDF to ~a" to-capital))
		    (:p (cl-who:fmt "Your files have been converted to ~a." to-capital))
		    (:button (:a :class "download-btn" :target "_blank" :href (format nil "/download-file/~a" dir) "Download now."))
		    
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
	      (:script :src "spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to PDF" to-capital))
		    (:p (cl-who:fmt "Convert your ~a documents to PDFs." to-capital))

		    (:div :id "drop-zone" :class "drop-zone" (cl-who:fmt "Drag and drop files here or click the Add ~a button" to-capital))
		    (:input :type "file" :id "file-input" :style "display: none;" :accept ".doc, .docx" :multiple t)
		    (:div :id "files-container")
		    (:div :class "btns"
			  (:button :class "upload-btn" :id "upload-btn"
				   (:span :class "add-symbol" "+")
				   (cl-who:fmt "Add ~a" to-capital))
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
		    (:div :id "success-indicator" :style "display: none; color: #1e90ff"
			  "The deck has been created, downloaded and saved in downloads.")
		    (:div :id "toast-container" :class "toast-container")
		    
		    
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
  (let* ((dir-list (caddr (str:split "/" (script-name*))))
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
	      (:script :src "spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))     
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to PDF" from-capital))
		    (:p "Your files have been converted to PDF.")
		    (:button (:a :class "download-btn" :target "_blank" :href (format nil "/download-file/~a" dir) "Download now."))
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
	      (:script :src "spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to PDF" from-capital))
		    (:p (cl-who:fmt "Convert your ~a documents to PDFs." from-capital))

		    (:div :id "drop-zone" :class "drop-zone" (cl-who:fmt "Drag and drop files here or click the Add ~a button" from-capital))
		    (:input :type "file" :id "file-input" :style "display: none;" :accept ".ppt, .pptx" :multiple t)
		    (:div :id "files-container")
		    (:div :class "btns"
			  (:button :class "upload-btn" :id "upload-btn"
				   (:span :class "add-symbol" "+")
				   (cl-who:fmt "Add ~a" from-capital))
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
		    (:div :id "success-indicator" :style "display: none; color: #1e90ff"
			  "The deck has been created, downloaded and saved in downloads.")
		    (:div :id "toast-container" :class "toast-container")
		    
		    
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
	      (:script :src "spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))     
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to PDF" from-capital))
		    (:p "Your files have been converted to PDF.")
		    (:button (:a :class "download-btn" :target "_blank" :href (format nil "/download-file/~a" dir) "Download now."))
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
	      (:script :src "spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to PDF" from-capital))
		    (:p (cl-who:fmt "Convert your ~a sheets to PDF Documents" from-capital))

		    (:div :id "drop-zone" :class "drop-zone" (cl-who:fmt "Drag and drop files here or click the Add ~a button" from-capital))
		    (:input :type "file" :id "file-input" :style "display: none;" :accept ".xlsx, .xls" :multiple t)
		    (:div :id "files-container")
		    (:div :class "btns"
			  (:button :class "upload-btn" :id "upload-btn"
				   (:span :class "add-symbol" "+")
				   (cl-who:fmt "Add ~a" from-capital))
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
		    (:div :id "success-indicator" :style "display: none; color: #1e90ff"
			  "The deck has been created, downloaded and saved in downloads.")
		    (:div :id "toast-container" :class "toast-container")
		    
		    
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
	      (:script :src "spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))     
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to PDF." from-capital))
		    (:p "Your files have been converted to PDF.")
		    (:button (:a :class "download-btn" :target "_blank" :href (format nil "/download-file/~a" dir) "Download now."))
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
		   (:script :src "spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	     	   (:style (str (home-css))))
		  (:body
		   (:div :class "main"
			 (header)
			 (:h1 (cl-who:fmt "Convert ~a to ~a" from-capital to-capital))
			 (:p (cl-who:fmt "Convert your ~a files to ~a." from-capital to-capital))

			 (:div :id "drop-zone" :class "drop-zone" (cl-who:fmt "Drag and drop files here or click the Add ~a button" from-capital))
			 (:input :type "file" :id "file-input" :style "display: none;" :accept (format nil ".~a" from) :multiple t)
			 (:div :id "files-container")
			 (:div :class "btns"
			       (:button :class "upload-btn" :id "upload-btn"
					(:span :class "add-symbol" "+")
					(cl-who:fmt "Add ~a" from-capital))
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
			 (:div :id "success-indicator" :style "display: none; color: #1e90ff"
			       "The deck has been created, downloaded and saved in downloads.")
			 (:div :id "toast-container" :class "toast-container")
			 
			 
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
	      (:meta :name "keywords" :content (format nil "spotpdf spot pdf convert ~a to ~a, online, free." from to))
	      (:link :rel "stylesheet" :href "/spotpdf/static/bootstrap-5.0.2/css/bootstrap.min.css")
	      (:script :src "spotpdf/static/bootstrap-5.0.2/js/bootstrap.min.js")
	      (:style (str (home-css))))     
	     (:body
	      (:div :class "main"
		    (header)
		    (:h1 (cl-who:fmt "Convert ~a to ~a." from-capital to-capital))
		    (:p (cl-who:fmt "Your files have been converted to ~a." to-capital))
		    (:button (:a :class "download-btn" :target "_blank" :href (format nil "/download-file/~a" dir) "Download now."))
		    (:script (str (home-js))))
	      (:div :class "ad")
	      (footer)
	      (:script :src "/spotpdf/static/bootstrap-5.0.2/js/multilevel-dropdown.js"))))))

;;;============ CONVERSION FUNCTIONS ===========================

(defun pdf-to-format (dir file-path format &aux (infilter (trivia:match format
							    ("pptx" "impress_pdf_import")
							    ("docx" "writer_pdf_import")
							    )))
  "convert a file pdf to a given format
  dir is the uuid dir name for the request."
  (let ((cmd (format nil "/usr/bin/libreoffice --headless --infilter=~s --convert-to ~a --outdir ~s ~s"
		     infilter format (namestring (truename dir)) (namestring (truename file-path)))))
    (uiop:run-program cmd)
    (delete-file file-path)))

(defun convert-pdf-to-format (format uuid post-parameters &aux (dir (format nil "~~/common-lisp/ninx/apps/spotpdf/files/~a/" uuid)))
  "given a list of post parameters, create a directory for them at uuid.
   copy all files to it, then convert them to pptx, remove the pdf files,
   and return after that."
  (ensure-directories-exist dir)
  (dolist (param post-parameters)
    (trivia:match param
      ((list _ path file-name "application/pdf")
       (let ((pdf-path (format nil "~a~a" dir file-name)))
    	 (uiop:copy-file path pdf-path)
	 (pdf-to-format dir pdf-path format)))
      (_ nil))))


(defun epub-to-pdf (epub-path pdf-path)
  "convert a file pdf to a given format
  dir is the uuid dir name for the request."
  (let* ((cmd (format nil "/usr/bin/pandoc -f epub -t pdf ~a -o ~a" epub-path pdf-path)))
    (uiop:run-program cmd)
    (delete-file epub-path)))

(defun convert-epub-to-pdf (uuid post-parameters &aux (dir (format nil "~~/common-lisp/ninx/apps/spotpdf/files/~a/" uuid)))
  "given a list of post parameters, create a directory for them at uuid.
   copy all files to it, then convert them to pptx, remove the pdf files,
   and return after that."
  (ensure-directories-exist dir)
  (dolist (param post-parameters)
    (trivia:match param
      ((list _ path file-name _)
       (let ((epub-path (format nil "~a~a" dir file-name))
	     (pdf-path (format nil "~a~a.pdf" dir (pathname-name file-name))))
    	 (uiop:copy-file path epub-path)
	 (epub-to-pdf epub-path pdf-path)))
      (_ nil))))

(defun ebook-convert-fn (pdf-path epub-path)
  "convert a file pdf to a given format
  dir is the uuid dir name for the request."
  (format *terminal-io* "~%from-path: ~a~%to-path: ~a~%~%" pdf-path epub-path)
  (let* ((cmd (format nil "/usr/bin/ebook-convert ~a ~a --enable-heuristics" pdf-path epub-path)))
    (uiop:run-program cmd)
    (delete-file pdf-path)))

(defun ebook-convert (to uuid post-parameters &aux (dir (format nil "~~/common-lisp/ninx/apps/spotpdf/files/~a/" uuid)))
  "given a list of post parameters, create a directory for them at uuid.
   copy all files to it, then convert them to pptx, remove the pdf files,
   and return after that."
  (ensure-directories-exist dir)
  (format *terminal-io* "~%~%to: ~a~%~%" to)
  (dolist (param post-parameters)
    (trivia:match param
      ((list _ path file-name _)
       (let ((from-path (format nil "~a~a" dir file-name))
	     (to-path (format nil "~a~a.~a" dir (pathname-name file-name) to)))
    	 (uiop:copy-file path from-path)
	 (ebook-convert-fn from-path to-path)))
      (_ nil))))

(deftest convert-pdf-to-epub (convert-pdf-to-epub (to-string (make-v4)) '(("test.pdf" #p"~/common-lisp/ninx/apps/spotpdf/test-files/test.pdf" "test.pdf" "application/pdf"))) nil)

(deftest convert-epub-to-pdf (convert-epub-to-pdf (to-string (make-v4)) '(("test.epub" #p"~/common-lisp/ninx/apps/spotpdf/test-files/test.epub" "test.epub" "application/epub+zip"))) nil)

(deftest convert-pdf-to-pptx (convert-pdf-to-format "pptx" (to-string (make-v4)) '(("test.pdf" #p"~/common-lisp/ninx/apps/spotpdf/test-files/test.pdf" "test.pdf" "application/pdf"))) nil)

(deftest convert-pdf-to-word (convert-pdf-to-format "docx" (to-string (make-v4)) '(("test.pdf" #p"~/common-lisp/ninx/apps/spotpdf/test-files/test.pdf" "test.pdf" "application/pdf"))) nil)

(defun get-downloadable-data (dir &aux (path (format nil "~~/common-lisp/ninx/apps/spotpdf/files/~a/" dir)))
  "counts the number of files in a given directory, if it is 1, returns it and its content-type.
   if it has more than 1, then the files are compressed into a zip and that is returned to the user."
  (let* ((files (uiop:directory-files path))
	 (len (length files)))
    (format t "~%~a~%~%" len)
    (cond
      ((= len 1)
       (list (cl-mime-from-string:mime-type-from-string (namestring (car files)))
	     (format nil "~a.~a" (pathname-name (car files)) (pathname-type (car files)))
	     (ninx::read-binary-file-to-octets (car files))))
      ((> len 1)
       (let* ((zip-name (format nil "~a.zip" dir))
	      (zip-path (namestring (format nil "~~/common-lisp/ninx/apps/spotpdf/zip/~a" zip-name))))
	 (uiop:run-program (format nil "/usr/bin/zip -r -j ~a ~a"
				   zip-path
				   (namestring path)))
	 (list "application/zip"
	       zip-name
	       (ninx::read-binary-file-to-octets zip-path)))))))

(defun format-to-format (to dir file-path)
  "convert a given format to pdf
  dir is the uuid dir name for the request."
  (let ((cmd (format nil "/usr/bin/libreoffice --headless --convert-to ~a --outdir ~s ~s"
		     to (namestring (truename dir)) (namestring (truename file-path)))))
    (uiop:run-program cmd)
    (delete-file file-path)))

(defun convert-format-to-format (to uuid post-parameters &aux (dir (format nil "~~/common-lisp/ninx/apps/spotpdf/files/~a/" uuid)))
  "given a list of post parameters, create a directory for them at uuid.
   copy all files to it, then convert them to pptx, remove the pdf files,
   and return after that."
  (ensure-directories-exist dir)
  (dolist (param post-parameters)
    (trivia:match param
      ((list _ path file-name _)
       (let ((pdf-path (format nil "~a~a" dir file-name)))
    	 (uiop:copy-file path pdf-path)
	 (format-to-format to dir pdf-path)))
      (_ nil))))

(deftest convert-xlsx-to-docx (convert-format-to-format "docx" (to-string (make-v4)) '(("file" #p"~/common-lisp/ninx/apps/spotpdf/test-files/test.xlsx" "test.xlsx" "any"))))

(deftest convert-xlsx-to-pdf (convert-format-to-format "pdf" (to-string (make-v4)) '(("file" #p"~/common-lisp/ninx/apps/spotpdf/test-files/test.xlsx" "test.xlsx" "any"))))
