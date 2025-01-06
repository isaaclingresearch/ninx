(in-package :cenna)

(define-easy-handler (cenna-save-user-profile :uri (define-matching-functions "^/save-user-profile$" *cenna-host*)
					  :host *cenna-host*
					  :default-request-type :post
					  :acceptor-names '(ninx::ninx)) (profile-json)
  (let* ((profile (jzon:parse profile-json))
	 (user-id (create-user-id)))
    (set-full-name user-id (gethash "full-name" profile))
    (set-gender user-id (gethash "gender" profile))
    (set-occupation user-id (gethash "occupation" profile))
    (set-date-of-birth user-id (gethash "date-of-birth" profile))
    (set-race-ethnicity user-id (gethash "race" profile))
    (set-marital-status user-id (gethash "marital-status" profile))
    (set-country-of-birth user-id (gethash "country-of-birth" profile))
    (set-level-of-education user-id (gethash "level-of-education" profile))
    (set-sex-assigned-at-birth user-id (gethash "sex-assigned-at-birth" profile))
    (set-contact user-id (gethash "telephone-number" profile) (gethash "email" profile))
    (set-next-of-kin user-id (gethash "next-of-kin-name" profile) (gethash "next-of-kin-email" profile)
		     (gethash "next-of-kin-telephone-number" profile) (gethash "next-of-kin-relationship" profile))
    (jzon:stringify (hash-create `((user-id ,user-id))))))

(define-easy-handler (cenna-save-allergy-history :uri (define-matching-functions "^/save-allergy-history$" *cenna-host*)
					  :host *cenna-host*
					  :default-request-type :post
					  :acceptor-names '(ninx::ninx)) (json-data)
  (let* ((data (jzon:parse json-data))
	 (user-id (gethash "user-id" data))
	 (allergies (gethash "allergies" data)))
    (dolist (i allergies)
      (set-allergy user-id
		   (gethash "name" i)
		   (gethash "severity" i)
		   (gethash "description" i)
		   (gethash "management" i)
		   (gethash "date-of-start" i))))
  (jzon:stringify (hash-create '(("result" "success")))))

(define-easy-handler (cenna-save-chronic-disease-history :uri (define-matching-functions "^/save-chronic-disease-history$" *cenna-host*)
					  :host *cenna-host*
					  :default-request-type :post
					  :acceptor-names '(ninx::ninx)) (json-data)
  (let* ((data (jzon:parse json-data))
	 (user-id (gethash "user-id" data))
	 (chronic-diseases (gethash "chronic-diseases" data)))
    (dolist (i chronic-diseases)
      (set-chronic-disease user-id
		   (gethash "name" i)
		   (gethash "seevrity" i)
		   (gethash "description" i)
		   (gethash "management" i)
		   (gethash "complications" i)
		   (gethash "date-of-start" i)))))

(define-easy-handler (cenna-save-allergy-history :uri (define-matching-functions "^/save-allergy-history$" *cenna-host*)
					      :host *cenna-host*
					      :default-request-type :post
					      :acceptor-names '(ninx::ninx)) (json-data)
  (let* ((data (jzon:parse json-data))
	 (user-id (gethash "user-id" data))
	 (admissions (gethash "admissions" data)))
    (dolist (i admissions)
      (set-admission user-id
		     (gethash "reason-for-admission" i)
		     (gethash "duration-of-stay" i)
		     (gethash "activities-done" i)
		     (gethash "complications" i)
		     (gethash "medications" i)
		     (gethash "date-of-admission" i)))))
