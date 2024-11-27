(in-package :ninx-app)

(defvar *ninx-url* (uiop:getenv "NINX_HOST"))
(defvar *ninx-host* (let ((host (uiop:getenv "NINX_HOST")))
			   (if (equal "443" ninx:*ninx-https-port*)
			       host
			       (format nil "~a:~a" host ninx:*ninx-https-port*))))

		 ;; (:style "body {line-height: 1.4; font-size: 16px; padding: 0 10px; margin: 50px auto; max-width: 650px; text-align: left; text-wrap: pretty;} a:visited {color: blue;}")
(defun home-css ()
  (cl-css:css
   `((body :line-height 1.4 :font-size 16px :padding "0 10px" :margin "50px auto" :max-width 650px :text-align left :text-wrap pretty)
     ("a:visited" :color blue)
     )))

(define-easy-handler (ninx-index :uri (define-matching-functions "^/$" *ninx-host*)
				 :host *ninx-host*
				 :acceptor-names '(ninx::ninx)) ()
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
	    (:p (:a :target "_blank" :href (format nil "https://~a/lam" ninx-blog::*ninx-blog-host*) "Lubwama Abdul Muswawir"))

	    (:b (:p "Mail us at " (:a :href "mailto:info@ninx.xyz" "info@ninx.xyz")))
	    (:h2 "Products")
	    (:p (:a :target "_blank" :href (format nil "https://~a" decklm::*declm-host*) "DeckLM") " - Generate slides from your learning resources in minutes.")
	    :hr
	    (:b "Ninx Technology Limited,")
	    :br
	    (:b "Lugoba North, Kazo Lugoba, Nansana Division.")
	    :br
	    (:b "P.O.Box 112999, Wakiso,")
	    :br
	    (:b "Wakiso, Uganda.")))))
