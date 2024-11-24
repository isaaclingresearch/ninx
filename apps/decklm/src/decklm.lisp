(in-package :decklm)

(defparameter *current-gemini-model* "gemini-1.5-flash")
(defparameter *current-gpt-model* "o1-mini")

(tt:set-contextual-variable :footer-enabled t)

;; load fonts
(pdf:load-ttf-font #p"~/common-lisp/ninx/apps/decklm/fonts/NotoSans-Regular.ttf")
(pdf:load-ttf-font #p"~/common-lisp/ninx/apps/decklm/fonts/Roboto-Regular.ttf")


(defun make-slides (cookie description files)
  "given a user cookie, make slides from files using description, description and/or files may be provided by the user.
description os text. files is returned from hunchentoot by post-parameter

save the data to the database
then send the file id back to the user for redirection to the download page."
  (let* ((file-data (mapcar (lambda (f) (cons "user" (cons nil f))) files))
	 (llm-data (cond
		     ((and description files)
		      (llms:query-gemini file-data :system-prompt (slides-with-files-and-description description) :model-keyparam :gemini-1.5-pro))
		     ((and description (null files))
		      (llms:query-gemini () :system-prompt (slides-with-no-files description)))
		     ((and files (null description))
		      (llms:query-azure-ai file-data :system-prompt (slides-with-only-files))))))
    ;; when saving gemini, we save depending on length of the the input, normal is the *current-gemini-model*
    ;; > 128k is *current-gemini-model*-128k
    (trivia:match llm-data
      ((list :error error-code _) (hash-create `(("success" nil)
						 ("errorCode" ,error-code))))
      ((list nil input-tokens nil _)
       (incr-parsing-errors :model *current-gemini-model*)
       (let ((model (if (< 128000 input-tokens) (format nil "~a-128k" *current-gemini-model*) *current-gemini-model*)))
	 (if (has-free-trial-p cookie)
	     (incr-trial-input-tokens input-tokens :model model)
	     (incr-user-tokens nil (- input-tokens) cookie))
	 (incr-input-tokens input-tokens :model model))
       (hash-create `(("success" nil)
		      ("errorCode" 500))))
      (_
       (let ((input-tokens (first (cdr llm-data)))
	     (output-tokens (second (cdr llm-data)))
	     (total-tokens (third (cdr llm-data))))
	 ;; save tokens for user and general count, except if the user has a free trial, this is done before decoding the data,
	 ;; as that some times crashes, we should have already billed the user.
	 (let ((model (if (< 128000 input-tokens) (format nil "~a-128k" *current-gemini-model*) *current-gemini-model*)))
	   (if (has-free-trial-p cookie)
	       (progn (incr-trial-input-tokens input-tokens :model model)
		      (incr-trial-output-tokens output-tokens :model model))
 	       (incr-user-tokens nil (- total-tokens) cookie)) ;; this decreases the remaining user tokens
	   
	   (incr-input-tokens input-tokens :model model)
	   (incr-output-tokens output-tokens :model model))

	 ;; after we send the data to a gpt model for further explanation.
	 ;; we need to define the function so that we can recursively call it incase of a parsing error.
	 (labels ((get-gpt-data (data &optional (tries 0))
		    (if (equal tries 1) ;; try again only five times before returning an error
			(hash-create `(("success" nil)
				       ("errorCode" 500)))
			(let* ((gpt-data (llms:query-azure-ai () :model-keyparam *current-gpt-model* :system-prompt (gpt-prompt description data))))
			  (trivia:match gpt-data
			    ((list :error error-code _) (hash-create `(("success" nil)
								       ("errorCode" ,error-code))))
			    ((list nil input-tokens nil _)
			     (incr-parsing-errors :model *current-gpt-model*)
			     (if (has-free-trial-p cookie)
				 (incr-trial-input-tokens input-tokens :model *current-gpt-model*)
				 (incr-user-tokens nil (- input-tokens) cookie))
			     (incr-input-tokens input-tokens :model *current-gpt-model*)
			     (hash-create `(("success" nil)
					    ("errorCode" 500))))
			    (_
			     (let ((input-tokens (first (cdr gpt-data)))
				   (output-tokens (second (cdr gpt-data)))
				   (total-tokens (third (cdr gpt-data))))
			       ;; save tokens for user and general count, except if the user has a free trial, this is done before decoding the data,
			       ;; as that some times crashes, we should have already billed the user.
			       (if (has-free-trial-p cookie)
				   (progn (incr-trial-input-tokens input-tokens :model *current-gpt-model*)
					  (incr-trial-output-tokens output-tokens :model *current-gpt-model*))
 				   (incr-user-tokens nil (- total-tokens) cookie))
			       
			       (incr-input-tokens input-tokens :model *current-gpt-model*)
			       (incr-output-tokens output-tokens :model *current-gpt-model*)

			       ;; for the testing part, test if the data can be parsed,
			       ;; then if it can be saved to pdf and then if it can be saved to ppt
			       ;; we only proceed if it has passed all three tests
			       (let*  ((clean-data (nlp:remove-json-encapsulation (car gpt-data)))
				       (data (if (member :error (list (handler-case (jzon:parse clean-data)
									(error (err)
									  (declare (ignore err))
									  :error))
								      (handler-case (present-data-json clean-data)
									(error (err)
									  (declare (ignore err))
									  :error))
								      (handler-case (make-pptx-bytes clean-data)
									(error (err)
									  (declare (ignore err))
					      				  :error))))
						 :parsing-error
						 (jzon:parse clean-data))))
				 (print data)
				 (trivia:match data
				   (:parsing-error
				    (incr-parsing-errors :model *current-gpt-model*)
				    (get-gpt-data llm-data (1+ tries)))
				   (_
				    ;; when the data is parsed successfully, save it to the database and return it's id.
				    ;; the saved data can then be used to make either pdfs or pptxs upon user request.
				    (let ((title (gethash "overall-title" data)))
				      (let* ((doc-id (save-document cookie title clean-data)))
					;; file back to the user that we were successful, with the data and a title for the file created.
					(prog1
					    (hash-create `(("success" t)
							   ("docid" ,doc-id)
							   ("title" ,(str:downcase title))
							   ("balance" ,(let ((tokens (get-user-tokens nil cookie)))
									 (if tokens tokens 0)))))
					  )))))))))))))
	   (get-gpt-data llm-data)))))))

;;(present-data-json (get-document-data "692f2421-7882-4eee-8659-ee45bb94d354") :file "/tmp/test.pdf")
;;(gethash "definitions" (jzon:parse (get-document-data "692f2421-7882-4eee-8659-ee45bb94d354")))
(defun make-pdf-bytes (data)
  (let ((file (format nil "/tmp/~a.pdf" (to-string (make-v4)))))
    (present-data-json data :file file :show-label nil)
    (prog1
	(llms::read-binary-file-to-octets file)
      (delete-file file))))

(defun make-pptx-bytes (data)
  (let ((file (format nil "/tmp/~a.pptx" (to-string (make-v4)))))
    (pycall "decklm.make_pptx" file data)
    (prog1
	(llms::read-binary-file-to-octets file)
      (delete-file file))))

(defun remove-trailing-period (str)
  "Removes the trailing period from a string if it exists."
  (if (and (> (length str) 0) (char= (aref str (1- (length str))) #\.))
      (subseq str 0 (1- (length str)))
      str))

(defun present-data-json (json-data &key (file #p"~/common-lisp/ninx/apps/decklm/test.pdf") (show-label t))
  "when given json data, present it as a PDF file."
  (let* ((data (jzon:parse json-data))
	 (helvetica (pdf:get-font "roboto-regular" pdf::*unicode-encoding*))
	 (p-style `(:font-size 14 :top-margin 5 :font ,helvetica))
	 (body (coerce (gethash "content" data #()) 'list))
	 (body-data (apply #'append
			   (mapcar
			    (lambda (slide)
			      `((tt:paragraph (:font tt::*font-bold* :h-align :center :font-size 16 :font ,helvetica)
				  ,(gethash "title" slide))
				,@(loop for point across (gethash "content" slide)
					collect
					(cond
					  ((equal "paragraph" (gethash "type" point))
					   (let ((paragraph-content (gethash "content" point)))
					     (if (arrayp paragraph-content)
						 `(tt::itemize (:item-fmt "— " :text-style ,p-style)
						    ,@(loop for subpoint across paragraph-content
							    collect
							    (format nil "~a: ~a"
								    (remove-trailing-period (gethash "point" subpoint))
								    (gethash "explanation" subpoint))))
						 `(tt:paragraph ,p-style
						    "— "
						    (tt:with-style (:font tt::*font-bold*)
						      ,(remove-trailing-period (gethash "point" paragraph-content)))
						    ,@(let ((explanation (gethash "explanation" paragraph-content)))
							(when explanation
							  (list ": " explanation)))))))
					  ((equal "list" (gethash "type" point))
					   `(tt::itemize (:item-fmt "— " :text-style ,p-style)
					      ,@(mapcar (lambda (i-point)
							  (cond ((stringp i-point) i-point)
								((hash-table-p i-point)
								 (format nil "~a: ~a"
									 (remove-trailing-period (gethash "point" i-point))
									 (gethash "explanation" i-point)))
								(t i-point)))
							(coerce (gethash "content" point) 'list)))
					   )))
				(tt:paragraph ,p-style :eop)))
			    body))))
    (tt::render-document
     `((tt:set-contextual-variable :footer-enabled t)
       ,@(when show-label
	   `((tt:set-contextual-variable :version "Made with DeckLM.com")))
       (tt:paragraph (:font tt::*font-bold* :h-align :center :v-align :center :top-margin 100 :font-size 20 :font ,helvetica)
	 ,(gethash "overall-title" data)
	 :eop)
       ;; outline
       (tt:paragraph (:font tt::*font-bold* :h-align :center :font-size 16 :font ,helvetica) "Outline")
       (tt::itemize (:item-fmt "— " :text-style ,p-style)
	 ,@(remove-if-not #'stringp (coerce (gethash "outline" data #())
					    'list)))
       (tt:paragraph ,p-style :eop)
       ;; Definitions
       (tt:paragraph (:font tt::*font-bold* :h-align :center :font-size 16 :font ,helvetica) "Definitions")
       ,@(loop for definition across (gethash "definitions" data #())
	       collect
	       `(tt:paragraph ,p-style
		  "— "
		  (tt:with-style (:font tt::*font-bold*)
		    ,(remove-trailing-period (gethash "term" definition)))
		  ,@(let ((explanation (gethash "definition" definition)))
		      (when explanation
			(list ": " explanation)))))
       ;; (tt::itemize (:item-fmt "— " :text-style ,p-style)
       ;; 		    ,@(loop for definition across (gethash "definitions" data #())
       ;; 			    collect
       ;; 			    (format nil "~a: ~a" (remove-trailing-period (gethash "term" definition)) (gethash "definition" definition))))
       (tt:paragraph ,p-style :eop)
       ,@body-data
       ;; references
       (tt:paragraph (:font tt::*font-bold* :h-align :center :font-size 16 :font ,helvetica) "References")
       (tt::itemize (:item-fmt "— " :text-style ,p-style)
	 ,@(remove-if-not #'stringp (coerce (gethash "references" data #())
					    'list)))
       (tt:paragraph ,p-style :eop)
       (tt:paragraph (:font tt::*font-bold* :h-align :center :v-align :center :top-margin 100 :font-size 20 :font ,helvetica)
	 "The End."
	 (tt:mark-ref-point "DocumentEnd")))
     :paper-size :slide
     :file file)))

(defvar *test-json* "
{
  \"overall-title\": \"Hydroxyurea for Sickle Cell Disease: A Patient's Guide\",
  \"content\": [
    {
      \"title\": \"Introduction\",
      \"content\": [
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Sickle cell disease and its impact\",
            \"explanation\": \"Sickle cell disease is a chronic condition that affects millions of people worldwide. It primarily impacts the blood by causing red blood cells to become rigid and shaped like a sickle. This abnormal shape makes it difficult for red blood cells to move smoothly through blood vessels, leading to blockages and a reduced ability to transport oxygen throughout the body. The resulting lack of oxygen can cause severe pain, known as pain crises, and can lead to damage in various organs over time. These complications often necessitate frequent hospitalizations and can significantly diminish the quality of life for those affected.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Role of Hydroxyurea in Managing Sickle Cell Disease\",
            \"explanation\": \"Hydroxyurea is a medication that has been instrumental in managing sickle cell disease. By increasing the production of fetal hemoglobin, hydroxyurea helps to prevent red blood cells from becoming sickle-shaped. This reduction in sickle cell formation leads to fewer pain crises and decreases the risk of organ damage. Additionally, hydroxyurea can reduce the need for blood transfusions and shorten hospital stays, contributing to a longer and healthier life for patients. Understanding how hydroxyurea works and its benefits can empower patients to make informed decisions about their treatment plans in consultation with their healthcare providers.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Purpose of the Guide\",
            \"explanation\": \"This patient guide, developed by the American Society of Hematology (ASH), serves as an educational resource for individuals diagnosed with sickle cell disease. It provides comprehensive information about hydroxyurea, including its benefits, usage, safety, and considerations for family planning. The guide aims to facilitate informed discussions between patients and their doctors, helping to determine whether hydroxyurea is a suitable treatment option based on individual health needs and circumstances.\"
          }
        }
      ]
    },
    {
      \"title\": \"Sickle Cell Disease Basics\",
      \"content\": [
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"What is sickle cell disease?\",
            \"explanation\": \"Sickle cell disease is a hereditary blood disorder caused by a mutation in the gene that tells the body how to make hemoglobin, the protein in red blood cells responsible for carrying oxygen. This mutation results in the production of abnormal hemoglobin known as hemoglobin S. Unlike normal red blood cells, which are soft and round, sickle-shaped cells are rigid and can get stuck in small blood vessels, obstructing blood flow and causing various health complications. The disease is most commonly inherited in an autosomal recessive pattern, meaning a child must receive a defective gene from both parents to develop the condition.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Genetics of Sickle Cell Disease\",
            \"explanation\": \"Sickle cell disease is inherited through autosomal recessive genetics. This means that an individual must inherit two copies of the mutated hemoglobin gene, one from each parent, to develop the disease. If a person inherits only one mutated gene and one normal gene, they are considered carriers (sickle cell trait) and typically do not show symptoms but can pass the gene to their offspring. Understanding the genetic basis of sickle cell disease is crucial for family planning and assessing the risk of passing the condition to future generations.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Symptoms and Diagnosis\",
            \"explanation\": \"Symptoms of sickle cell disease often appear in early childhood and can include severe pain episodes, anemia, frequent infections, delayed growth, and vision problems. Diagnosis is typically made through newborn screening programs, which test for abnormal hemoglobin shortly after birth. Early diagnosis allows for prompt treatment and management of the disease, which can significantly improve health outcomes and quality of life for affected individuals.\"
          }
        }
      ]
    },
    {
      \"title\": \"Problems Caused by Sickle Cell Disease\",
      \"content\": [
        {
          \"type\": \"list\",
          \"content\": [
            \"Anemia: Sickle cell disease causes the body to produce fewer red blood cells, leading to a condition known as anemia. This results in reduced oxygen delivery to tissues and organs, causing chronic fatigue, weakness, and shortness of breath.\",
            \"Pain crises: The rigid, sickle-shaped red blood cells can block blood flow in small vessels, leading to severe pain episodes called pain crises. These crises can occur in the chest, abdomen, joints, and bones, and may last for hours or days.\",
            \"Acute chest syndrome: This is a severe lung complication characterized by chest pain, fever, and difficulty breathing. It is caused by sickle cells blocking blood flow in the lungs, leading to inflammation and infection. Acute chest syndrome is life-threatening and may require hospitalization and intensive treatment.\",
            \"Organ damage: Chronic blockage of blood flow can lead to damage in vital organs such as the brain, heart, kidneys, and eyes. Over time, this can result in complications like stroke, heart failure, kidney disease, and vision loss, significantly impacting overall health and lifespan.\",
            \"Increased risk of infections: Sickle cell disease can compromise the immune system, making individuals more susceptible to infections. Bacterial infections, particularly those caused by Streptococcus pneumoniae, can be life-threatening and require prompt medical attention.\"
          ]
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Impact on Daily Life\",
            \"explanation\": \"The complications of sickle cell disease extend beyond physical health, affecting various aspects of daily life. Frequent hospitalizations, chronic pain, and fatigue can interfere with education, employment, and social activities. Additionally, the psychological burden of managing a chronic illness can lead to mental health challenges such as depression and anxiety. Comprehensive care and supportive services are essential to address both the physical and emotional needs of individuals with sickle cell disease.\"
          }
        }
      ]
    },
    {
      \"title\": \"Hydroxyurea Facts\",
      \"content\": [
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Hydroxyurea and its use in sickle cell disease\",
            \"explanation\": \"Hydroxyurea is a medication that has been a cornerstone in the treatment of sickle cell disease since the 1980s. Initially used in cancer therapy due to its ability to inhibit cell growth, hydroxyurea was later found to be effective in reducing the frequency of pain crises in sickle cell patients. The U.S. Food and Drug Administration (FDA) approved hydroxyurea for treating adults with sickle cell disease in 1998 and extended the approval to children in 2017. Unlike higher doses used in cancer treatment, the dosage for sickle cell disease is carefully managed to maximize benefits while minimizing side effects. By reducing the occurrence of sickle cell-related complications, hydroxyurea contributes to improved life expectancy and quality of life for individuals with sickle cell disease.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"FDA Approval and Guidelines\",
            \"explanation\": \"The FDA's approval of hydroxyurea for sickle cell disease marks a significant milestone in treatment options. Clinical guidelines recommend hydroxyurea as a standard treatment for eligible patients, emphasizing its role in reducing pain episodes and preventing organ damage. Healthcare providers follow specific protocols to determine the appropriate dosage and monitor patients for efficacy and safety. Adhering to these guidelines ensures that patients receive optimal care tailored to their individual health profiles.\"
          }
        }
      ]
    },
    {
      \"title\": \"Benefits of Hydroxyurea\",
      \"content\": [
        {
          \"type\": \"list\",
          \"content\": [
            \"Reduced pain crises: By decreasing the formation of sickle-shaped red blood cells, hydroxyurea lowers the frequency and severity of pain episodes, allowing patients to experience fewer interruptions in their daily lives.\",
            \"Fewer episodes of acute chest syndrome: Hydroxyurea minimizes the risk of developing acute chest syndrome, a serious lung complication, thereby reducing hospitalizations and the need for intensive treatments.\",
            \"Decreased need for blood transfusions: With fewer complications and better-managed symptoms, patients often require fewer blood transfusions, which can decrease the risk of iron overload and related health issues.\",
            \"Shorter hospital stays: Improved disease management leads to shorter durations of hospitalization during sickle cell crises, enabling patients to return to their normal activities more quickly.\",
            \"Prevention or slowing of organ damage: By ensuring better blood flow and oxygen delivery, hydroxyurea helps protect vital organs from damage, preserving organ function and extending overall health.\"
          ]
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Quality of Life Improvements\",
            \"explanation\": \"The benefits of hydroxyurea extend beyond the physical reduction of symptoms. By minimizing pain crises and other complications, patients can maintain a more active and fulfilling lifestyle. Reduced hospitalizations mean less disruption to education, work, and social activities. Additionally, the emotional relief from experiencing fewer pain episodes can lead to improved mental health and overall well-being. These quality-of-life enhancements are crucial for individuals managing a chronic condition like sickle cell disease.\"
          }
        }
      ]
    },
    {
      \"title\": \"How Hydroxyurea Works\",
      \"content\": [
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Mechanism of action\",
            \"explanation\": \"Hydroxyurea works by increasing the production of fetal hemoglobin (hemoglobin F) in red blood cells. Hemoglobin F is the primary form of hemoglobin present in fetuses and newborns, and it has a higher affinity for oxygen compared to adult hemoglobin. By elevating hemoglobin F levels, hydroxyurea reduces the ability of red blood cells to sickle under low-oxygen conditions. This stabilization prevents the rigid, sickle shape, allowing red blood cells to remain flexible and pass smoothly through blood vessels. Consequently, the likelihood of blockages decreases, leading to fewer pain crises and a lower risk of organ damage.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Additional Biological Effects\",
            \"explanation\": \"Beyond increasing fetal hemoglobin, hydroxyurea has several other beneficial effects on the blood and vascular system. It induces the production of nitric oxide, a molecule that helps dilate blood vessels, improving blood flow and reducing vascular inflammation. Hydroxyurea also decreases the number of white blood cells and platelets, which can contribute to the formation of blood clots. These combined actions work synergistically to enhance overall blood circulation and reduce the complications associated with sickle cell disease.\"
          }
        }
      ]
    },
    {
      \"title\": \"Who Should Take Hydroxyurea?\",
      \"content\": [
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Recommended for specific types of sickle cell disease\",
            \"explanation\": \"Hydroxyurea is particularly beneficial for individuals diagnosed with sickle cell disease type SS (homozygous sickle cell anemia) and sickle beta zero (Sβ0) thalassemia. These forms of the disease are associated with more severe symptoms and a higher risk of complications, making the protective effects of hydroxyurea especially valuable. For those with sickle cell disease type SC or sickle beta plus (Sβ+) thalassemia, hydroxyurea may also offer benefits, although further research is needed to fully understand its effectiveness in these variants. Determining the suitability of hydroxyurea involves a thorough evaluation by a healthcare provider, considering factors such as disease severity, patient age, and overall health.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Eligibility Criteria\",
            \"explanation\": \"To determine eligibility for hydroxyurea treatment, doctors assess various clinical parameters, including the frequency and severity of pain crises, hemoglobin levels, and the presence of complications like acute chest syndrome. Pediatric patients with frequent pain episodes or those at risk of severe complications are often considered good candidates for hydroxyurea. Additionally, individuals who have not responded adequately to other treatments may benefit from hydroxyurea therapy. Personalized assessments ensure that treatment plans are tailored to each patient's unique medical needs.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Consultation with Healthcare Providers\",
            \"explanation\": \"Before starting hydroxyurea, it is essential to have a detailed discussion with a hematologist or specialist familiar with sickle cell disease. This consultation involves reviewing the potential benefits and risks, understanding the treatment regimen, and addressing any concerns the patient may have. Healthcare providers will also establish a monitoring plan to track the medication's effectiveness and manage any side effects that may arise during treatment.\"
          }
        }
      ]
    },
    {
      \"title\": \"The Research on Hydroxyurea\",
      \"content\": [
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Multicenter Study of Hydroxyurea (1992)\",
            \"explanation\": \"The Multicenter Study of Hydroxyurea, conducted in 1992, was a pivotal clinical trial that evaluated the safety and efficacy of hydroxyurea in treating sickle cell disease. This study involved multiple research centers and a diverse cohort of participants, allowing for comprehensive data collection and analysis. The results demonstrated that hydroxyurea significantly reduced the frequency of pain crises, hospitalizations, episodes of acute chest syndrome, and the need for blood transfusions compared to a placebo. Importantly, the study found no increase in adverse side effects among participants taking hydroxyurea, establishing it as a safe and effective treatment option for sickle cell disease.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Subsequent Research and Long-Term Studies\",
            \"explanation\": \"Following the initial findings, numerous studies have further validated the benefits of hydroxyurea in managing sickle cell disease. Long-term research has shown that continuous use of hydroxyurea can improve overall survival rates and reduce the incidence of complications over time. Additional studies have explored the optimal dosing strategies, age groups that benefit most from the treatment, and the potential for hydroxyurea to enhance quality of life. Ongoing research continues to refine the understanding of hydroxyurea's mechanisms and its role in comprehensive sickle cell disease management.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Current Clinical Trials and Future Directions\",
            \"explanation\": \"Current clinical trials are investigating the use of hydroxyurea in combination with other therapies to further improve outcomes for sickle cell patients. Researchers are also exploring biomarkers that can predict individual responses to hydroxyurea, enabling more personalized treatment approaches. Future directions include developing novel formulations of hydroxyurea with enhanced efficacy and minimal side effects, as well as expanding its use to different populations and variants of sickle cell disease. These advancements aim to optimize treatment protocols and extend the benefits of hydroxyurea to a broader patient base.\"
          }
        }
      ]
    },
    {
      \"title\": \"Safety and Side Effects\",
      \"content\": [
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Long-term safety and cancer risk\",
            \"explanation\": \"Extensive research and long-term studies have demonstrated that hydroxyurea is safe for ongoing use in individuals with sickle cell disease, including children. Despite its origins as a chemotherapy agent, when used at the appropriate doses for sickle cell treatment, hydroxyurea does not pose a significant risk of cancer. Monitoring data have consistently shown no association between hydroxyurea use and an increased incidence of malignancies in sickle cell patients. This reassurance allows patients and healthcare providers to confidently incorporate hydroxyurea into long-term management plans.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Common side effects\",
            \"explanation\": \"Hydroxyurea is generally well-tolerated, but some patients may experience mild side effects. These can include thinning of the hair or mild hair loss, darkening of the fingernail beds, and nausea. These side effects are typically transient and manageable. In some cases, patients may notice a reduction in the growth rate of hair, which usually regrows once treatment is adjusted or discontinued. Nausea can often be mitigated by taking the medication with food. It is important for patients to communicate any side effects with their healthcare provider to ensure proper management and dosage adjustments if necessary.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Serious side effects\",
            \"explanation\": \"While serious side effects are rare, they can occur in some individuals taking hydroxyurea. These may include significant bone marrow suppression, leading to decreased production of blood cells, or allergic reactions. Symptoms such as unusual bleeding, persistent infections, severe fatigue, or signs of an allergic reaction (rash, itching, swelling, or difficulty breathing) should be promptly reported to a doctor. Regular blood tests are essential to monitor blood cell counts and ensure that any serious side effects are detected and managed early.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Monitoring and Management of Side Effects\",
            \"explanation\": \"To ensure the safe use of hydroxyurea, regular monitoring through blood tests is crucial. These tests help assess the medication's effectiveness and detect any adverse effects on the bone marrow or other bodily functions. If side effects become problematic, healthcare providers may adjust the dosage or recommend supportive treatments to alleviate symptoms. Open communication between patients and their medical team is vital for managing side effects effectively and maintaining the benefits of hydroxyurea therapy.\"
          }
        }
      ]
    },
    {
      \"title\": \"Family Planning and Hydroxyurea\",
      \"content\": [
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Considerations for women and men\",
            \"explanation\": \"Family planning is an important consideration for individuals with sickle cell disease taking hydroxyurea. For women who are pregnant or planning to become pregnant, hydroxyurea poses potential risks. Although many women have had healthy pregnancies while on hydroxyurea, the medication may increase the risk of birth defects. Therefore, it is crucial for women to discuss their reproductive plans with their healthcare provider before starting or continuing hydroxyurea therapy. Alternative treatments or contraceptive measures may be recommended to minimize risks during pregnancy. In men, hydroxyurea can affect fertility by lowering sperm counts, which may already be compromised by sickle cell disease. Additionally, hydroxyurea might reduce the likelihood of painful erections, a complication associated with sickle cell disease. Men considering fathering children should consult with their doctor to understand the implications and explore potential fertility preservation options if necessary.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Contraception and Hydroxyurea\",
            \"explanation\": \"Due to the potential teratogenic effects of hydroxyurea, effective contraception is recommended for both men and women of reproductive age who are undergoing treatment. This helps prevent unintended pregnancies during hydroxyurea therapy. Healthcare providers can offer guidance on suitable contraceptive methods and discuss strategies to ensure safe family planning while managing sickle cell disease.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Preconception Counseling\",
            \"explanation\": \"Preconception counseling is advised for individuals with sickle cell disease who wish to start a family. During these sessions, healthcare providers can discuss the risks and benefits of hydroxyurea, evaluate the patient's overall health, and provide recommendations for minimizing potential complications. This proactive approach facilitates informed decision-making and helps ensure the health and safety of both parents and future children.\"
          }
        }
      ]
    },
    {
      \"title\": \"Taking Hydroxyurea: Dosage and Monitoring\",
      \"content\": [
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Dosage and administration\",
            \"explanation\": \"Hydroxyurea is typically administered orally in pill form once daily. The dosage is carefully determined by a healthcare provider based on factors such as the patient's weight, age, blood count levels, and overall health status. It often starts at a lower dose to allow the body to adjust, with gradual increases to achieve the optimal therapeutic effect while minimizing side effects. Adherence to the prescribed dosage schedule is essential for maintaining consistent levels of fetal hemoglobin and effectively managing sickle cell disease symptoms.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Administration Tips\",
            \"explanation\": \"To ensure proper absorption and reduce the risk of gastrointestinal side effects, hydroxyurea should be taken with food unless otherwise directed by a healthcare provider. Patients should follow their doctor's instructions regarding the timing of doses and avoid missing scheduled intake. Using a daily pillbox, setting alarms, or establishing a routine can help maintain consistency in medication adherence.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Blood count monitoring\",
            \"explanation\": \"Regular blood count monitoring is a critical component of hydroxyurea therapy. Initial phase monitoring involves frequent blood tests to assess the medication's impact on blood cell counts and to ensure that hydroxyurea is effectively increasing fetal hemoglobin levels. Over time, the frequency of monitoring may decrease but remains an ongoing necessity to detect any potential complications early. These blood tests provide valuable information that helps healthcare providers adjust dosages as needed and maintain the balance between efficacy and safety.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Adjusting Treatment Based on Monitoring\",
            \"explanation\": \"Based on the results of regular blood count tests, healthcare providers may adjust the hydroxyurea dosage to optimize treatment outcomes. If blood counts drop too low, indicating bone marrow suppression, the dosage may be reduced or temporarily halted to allow recovery. Conversely, if the desired increase in fetal hemoglobin is not achieved, the dosage may be increased cautiously. Continuous monitoring ensures that hydroxyurea therapy remains both safe and effective over the long term.\"
          }
        }
      ]
    },
    {
      \"title\": \"Missed Dose and Long-Term Effects\",
      \"content\": [
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Managing missed doses\",
            \"explanation\": \"If a dose of hydroxyurea is missed, it is important to take the medication as soon as remembered. If it is nearly time for the next dose, the missed dose should be skipped to avoid doubling up. Missing a single dose is generally not dangerous and is unlikely to reverse the medication's benefits. However, maintaining a consistent dosing schedule is crucial for the effectiveness of hydroxyurea in managing sickle cell disease. Implementing strategies such as using a daily pill organizer, setting alarm reminders, or involving family members can help ensure adherence to the treatment regimen.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Long-term effects and persistence\",
            \"explanation\": \"Hydroxyurea's full benefits may take up to a year to become fully apparent, as it gradually increases fetal hemoglobin levels and reduces the formation of sickle cells. Persistence with the medication is essential, even if immediate results are not noticeable. Patients should maintain regular communication with their healthcare providers to discuss progress, address any concerns, and make necessary adjustments to the treatment plan. Staying committed to the prescribed regimen enhances the likelihood of achieving long-term disease management and improved health outcomes.\"
          }
        },
        {
          \"type\": \"paragraph\",
          \"content\": {
            \"point\": \"Strategies for Consistent Use\",
            \"explanation\": \"To support long-term adherence to hydroxyurea therapy, patients can adopt various strategies such as integrating medication-taking into their daily routines, using digital reminders, or seeking support from family and friends. Regular follow-up appointments and open dialogue with healthcare providers also help reinforce the importance of consistency and address any barriers to adherence. These strategies contribute to the sustained effectiveness of hydroxyurea in managing sickle cell disease.\"
          }
        }
      ]
    }
  ],
  \"references\": [
    \"Charache, S., Terrin, M. L., Moore, R. D., Dover, G. J., Barton, F. B., Eckert, S. V., McMahon, R. P., & Bonds, D. R. (1995). Effect of hydroxyurea on the frequency of painful crises in sickle cell anemia. The New England Journal of Medicine, 332(20), 1317-1322. http://doi.org/10.1056/NEJM199505183322001\",
    \"Ware, R. E., & Ilves, I. (1998). Hydroxyurea for the treatment of patients with sickle cell disease. Blood, 92(4), 125-131. http://doi.org/10.1182/blood.V92.4.125\",
    \"Steinberg, M. H., McCarthy, W., Bernaudin, F., Brambilla, D., Fonseca, V., Hanspal, E., ... & Humblet, O. (2017). Hydroxyurea for sickle cell disease: A report from the NHLBI working group. The New England Journal of Medicine, 374(16), 1573-1582. http://doi.org/10.1056/NEJMra1604198\"
  ],
  \"outline\": [
    \"Introduction\",
    \"Sickle Cell Disease Basics\",
    \"Problems Caused by Sickle Cell Disease\",
    \"Hydroxyurea Facts\",
    \"Benefits of Hydroxyurea\",
    \"How Hydroxyurea Works\",
    \"Who Should Take Hydroxyurea?\",
    \"The Research on Hydroxyurea\",
    \"Safety and Side Effects\",
    \"Family Planning and Hydroxyurea\",
    \"Taking Hydroxyurea: Dosage and Monitoring\",
    \"Missed Dose and Long-Term Effects\"
  ],
  \"definitions\": [
    {
      \"term\": \"Sickle cell disease\",
      \"definition\": \"A blood disorder characterized by abnormal hemoglobin, causing red blood cells to become sickle-shaped, leading to various health problems.\"
    },
    {
      \"term\": \"Hydroxyurea\",
      \"definition\": \"A medication used to treat sickle cell disease by increasing fetal hemoglobin levels, reducing the formation of sickle cells, and improving blood flow.\"
    },
    {
      \"term\": \"Pain crisis\",
      \"definition\": \"An episode of intense pain experienced by individuals with sickle cell disease, often due to blocked blood vessels.\"
    },
    {
      \"term\": \"Acute chest syndrome\",
      \"definition\": \"A life-threatening complication of sickle cell disease involving lung inflammation and difficulty breathing, often caused by blocked blood vessels in the lungs.\"
    },
    {
      \"term\": \"Anemia\",
      \"definition\": \"A condition characterized by a reduced number of red blood cells, leading to fatigue and weakness.\"
    },
    {
      \"term\": \"Hemoglobin\",
      \"definition\": \"A protein in red blood cells that carries oxygen throughout the body.\"
    },
    {
      \"term\": \"Fetal hemoglobin (hemoglobin F)\",
      \"definition\": \"A type of hemoglobin found in newborns that helps keep red blood cells round and flexible.\"
    },
    {
      \"term\": \"Thalassemia\",
      \"definition\": \"A group of inherited blood disorders characterized by reduced or absent production of normal hemoglobin.\"
    },
    {
      \"term\": \"Autosomal recessive\",
      \"definition\": \"A pattern of inheritance where two copies of an abnormal gene must be present for the disease to develop.\"
    },
    {
      \"term\": \"Nitric oxide\",
      \"definition\": \"A molecule produced by the body that helps relax blood vessels, improving blood flow and reducing inflammation.\"
    },
    {
      \"term\": \"Bone marrow suppression\",
      \"definition\": \"A decrease in the production of blood cells in the bone marrow, which can lead to anemia, increased risk of infections, and bleeding problems.\"
    }
  ]
}
" )

(deftest present-data-json (present-data-json *test-json*) nil)
(deftest present-data-pptx (null (make-pptx-bytes *test-json*)) nil)
