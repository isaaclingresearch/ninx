(in-package :llms)

(defparameter *main-prompt*
  "You will act as a text summariser and flashcard creator who communicates in common lisp s-expressions. You are given photo(s) of parts of books, you extract all definitions and return them in this p-list: ((:definitions ((:title1 \"Definition1) ...))")

(defun make-prompt (messages)
  "when given a list of messages, use it to create a system prompt"
  (format nil "You will act as a friendly assitant, kind, polite and caring. Return a strict JSON object {\"response\": RESPONSE) where RESPONSE is your answer. If you don't know a thing, don't try to make it up. Respond with {\"response\": \"I don't know that\"}. But if it is provided in context, then use the context to respond. The context for this chat is this list of messages between you and the user: (~{~a ~}). Return only the JSON" messages))

(defparameter *sexp-chat-prompt*
  "" "the main prompt")

;;;;;;;;;;;;;======================= GEMINI =================================================

(defun make-gemini-multiturns (messages system-prompt)
  "this will create a specific multiturn chat for the gemini api. data must be a alist of user as key and message as value.
add the system message at the end, to test if that will have a noticeable effect.

messages are (sender . message)
message can be a string or (srtring-message . (path filename mimetype))"
  (let* ((encoded-messages `(,@(loop for (role . message) in (acons "user" system-prompt messages)
				     collect (make-gemini-turn role message))))
	 (h1 (make-hash-table :test 'equal)))
    (setf (gethash "contents" h1) encoded-messages)
    (jzon:stringify h1)))

(defun make-gemini-turn (role message)
  "message can be a string or a list (path name mimetype)
sample with image
{
 \"contents\":[
    {
      \"parts\":[
        {\"text\": \"What is this picture?\"},
        {
          \"inline_data\": {
            \"mime_type\":\"image/jpeg\",
            \"data\": \"'$(base64 -w0 image.jpg)'\"
          }
        }
      ]
    }
  ]
}
sample with no image
{\"role\":\"user\",
         \"parts\":[{
           \"text\": \"Write the first line of a story about a magic backpack.\"}]},
"
  (let ((h1 (make-hash-table :test 'equal))
	(h2 (make-hash-table :test 'equal))
	(h3 (make-hash-table :test 'equal)))
    (if (stringp message)
	(progn
	  (setf (gethash "text" h2) message)
	  (setf (gethash "role" h1) role)
	  (setf (gethash "parts" h1) (list h2)))
	(let ((file (cdr message)))
	  (setf (gethash "mime_type" h3) (third file))
	  (setf (gethash "data" h3) (base64:usb8-array-to-base64-string (read-binary-file-to-octets (first file))))
	  (setf (gethash "inline_data" h2) h3)
	  (setf (gethash "role" h1) role)
	  (setf (gethash "parts" h1) (list h2))))
    h1))


(defun read-binary-file-to-octets (file-path)
  "Read the binary file at FILE-PATH and return its contents as a vector of octets."
  (with-open-file (stream file-path :direction :input :element-type '(unsigned-byte 8))
    (let* ((file-size (file-length stream))
           (octets (make-array file-size :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))


(defun write-octets-to-binary-file (octets file-path)
  "Write the octet array OCTETS to the binary file at FILE-PATH."
  (with-open-file (stream file-path :direction :output
                                    :element-type '(unsigned-byte 8)
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
    (write-sequence octets stream)))

(defun query-gemini (message-alist &key (model-keyparam :gemini-1.5-flash) (system-prompt *sexp-chat-prompt*))
  "Send a query to gemini flash. this function is designed to extract data from the gemini endpoint. message turns should be a list:
a message is (user . message)
you can include an image into the message by passing '(\"user\" . (text-message . (path name content-type)))"
  (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
      (http-request
       (format nil "https://generativelanguage.googleapis.com/v1beta/models/~a:generateContent?key=~a"
	       (trivia:match model-keyparam
		 (:gemini-1.5-flash "gemini-1.5-flash-latest")
		 (:gemini-1.5-pro "gemini-1.5-pro-latest")
		 (:gemini-1.0-pro "gemini-1.0-pro"))
	       (uiop:getenv "NINXAI_GEMINI_KEY"))
       :external-format-in :UTF-8 :external-format-out :UTF-8
       :method :post :content-type "application/json" :content (make-gemini-multiturns message-alist system-prompt))
    (declare (ignore response-headers status-text request-uri flexi-response response-bool))
    (if (equal response-code 200)
	(let*((response-json (jzon:parse (flexi-streams:octets-to-string response :external-format :utf8)))
	      (total-tokens (gethash "totalTokenCount" (gethash "usageMetadata" response-json)))
	      (input-tokens (gethash "promptTokenCount" (gethash "usageMetadata" response-json)))
	      (output-tokens (gethash "candidatesTokenCount" (gethash "usageMetadata" response-json)))
	      (content (gethash "content" (aref (gethash "candidates" response-json) 0)))
	      (response-content (when content
				  (gethash "text" (aref (gethash "parts" content) 0)))))
	  (cons response-content (list input-tokens output-tokens total-tokens)))
	`(:error ,response-code ,(flexi-streams:octets-to-string response)))))

;;;; ==================================== AZURE AI ===================;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-openai-data (messages &optional tuning-params system-prompt model)
  "we add the model parameter because o1 doesn'e support system messages."
  (let ((h1 (make-hash-table)))
    (dolist (tuning-param tuning-params)
      (setf (gethash (car tuning-param) h1) (cdr tuning-param)))
    (setf (gethash "messages" h1) "json_object")
    (setf (gethash "messages" h1) (mapcar (lambda (a) (make-openai-message (car a) (cdr a)))
					  (if system-prompt
					      (if (equal "o1-mini" model)
						  `(("user" . ,system-prompt) ,@messages)
						  `(("system" . ,system-prompt) ,@messages))
					      messages)))
    (jzon:stringify h1)))

(defun make-openai-message (role message)
  "for now, we are not going to be doing multiturn, so we don't expect an assistant role in the messages.
only system and user make the appropriate message depending on whether the message is a string or a list, 
if it's a list, then it contains files, which maybe one or more.
file will be a hunchentoot file (path name mime-type)
unfortunately, openai currently takes only images.
"
  (if (stringp message)
      (hash ("role" role) ("content" message))
      (hash ("role" role) ("content" (remove-if #'null
						`(,(when (car message)
						     (hash ("type" "text") ("text" (car message))))
						  ,@(loop for file in (cdr message)
							  collect  (hash ("type" "image_url")
									 ("image_url" (hash ("url" (format nil "data:~a;base64,~a"
													   (third file)
													   (base64:usb8-array-to-base64-string (read-binary-file-to-octets (car file)))))))))))))))


(defun query-azure-ai (messages &key (model-keyparam "gpt-4o") tuning-params (system-prompt *main-prompt*))
  "query models served by azure openai service"
  (let* ((endpoint (trivia:match model-keyparam
		     ("gpt-4o" "https://ninxai-openai.openai.azure.com/openai/deployments/gpt-4o/chat/completions?api-version=2024-02-15-preview")
		     ("gpt-4o-mini" "https://ninxai-openai.openai.azure.com/openai/deployments/gpt-4o-mini/chat/completions?api-version=2023-03-15-preview")
		     ("o1-mini" "https://ninxai-openai.openai.azure.com/openai/deployments/o1-mini/chat/completions?api-version=2024-08-01-preview")))
	 (key (uiop:getenv "AZURE_OPENAI_KEY"))
	 (json-data (make-openai-data messages tuning-params system-prompt model-keyparam)))
    (multiple-value-bind (response response-code response-headers request-uri flexi-response response-bool status-text)
	(http-request
	 endpoint
	 :external-format-in :UTF-8 :external-format-out :UTF-8
	 :additional-headers `(("api-key" . ,key)
			       ("Authorization" . ,(format nil "Bearer ~a" key)))
       	 :method :post :content-type "application/json" :content json-data)
      (declare (ignore status-text response-headers request-uri flexi-response response-bool))
      (if (equal response-code 200)
	  (let* ((response-plist (jzon:parse (flexi-streams:octets-to-string response :external-format :utf8)))
		 (total-tokens (gethash "total_tokens" (gethash "usage" response-plist)))
		 (completion-tokens (gethash "completion_tokens" (gethash "usage" response-plist)))
		 (prompt-tokens (gethash "prompt_tokens" (gethash "usage" response-plist)))
		 (response-message (gethash "content" (gethash "message" (aref (gethash "choices" response-plist) 0)))))
	    (cons response-message (list prompt-tokens completion-tokens total-tokens)))
	  `(:error ,response-code ,(flexi-streams:octets-to-string response))))))
