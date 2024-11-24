(in-package :ninx-app)

(defvar *ninx-url* (uiop:getenv "NINX_HOST"))
(defvar *ninx-host* (format nil "~a:~a" *ninx-url* ninx:*ninx-https-port*))

(defun home-css ()
  (cl-css:css
   `((body :margin 40px :font-family "Roboto, san-serif")
     (a :text-decoration none :color blue)
     ("a:visited" :text-decoration none :color blue))))

(define-easy-handler (ninx-index :uri "/" :host *ninx-host*) ()
  (with-html-output-to-string (*standard-output*)
    "<!DOCTYPE html>"
    (:html :lang "en"
	   (:head
            (:title "Ninx Technology Limited")
            (:meta :charset "UTF-8")
            (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
            (:meta :name "description" :content "The accounts page for DeckLM")
	    (:link :rel "icon" :href "/blog-priv/favicons/favicon.ico" :sizes "any")
	    (:link :rel "apple-touch-icon" :href "/blog-priv/favicons/apple-touch-icon.png")
	    (:link :rel "manifest" :href "/blog-priv/favicons/manifest.json")
	    (:style (str (home-css)))
	    (:link :href "https://fonts.googleapis.com/css?family=Roboto&display=swap" :rel "stylesheet")
	    )
	   (:body
	    (:h1 (:a :href "/" "Ninx Technology Limited."))
	    (:p "Ninx Technology Limited (Ninxtech) is studying how advances in technology in health, education and agriculture can be applied and spread to all humans. We are engineers, doctors and teachers building software, researching and applying ideas from marketing and spread of technologies to all our products.")
	    (:h2 "Team")
	    (:p "Lubwama Abdul Muswawir!")

	    (:p "Contact us at " (:a :href "mailto:info@ninx.xyz" "Info@ninx.xyz"))
	    (:h2 "Products")
	    (:p (:a :href "https://decklm.com" "Decklm") " - Generate slides from your learning resources in minutes.")
	    :hr
	    "Ninx Technology Limited,"
	    :br
	    "Lugoba North, Kazo Lugoba, Nansana Division."
	    :br
	    "P.O.Box 112999, Wakiso"
	    :br
	    "Wakiso, Uganda."))))
