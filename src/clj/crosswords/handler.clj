(ns crosswords.handler
  (:require [compojure.core :refer [GET defroutes]]
            [compojure.route :refer [not-found resources]]
            [ring.middleware.defaults :refer [site-defaults wrap-defaults]]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [include-js include-css]]
            [prone.middleware :refer [wrap-exceptions]]
            [ring.middleware.reload :refer [wrap-reload]]
            [environ.core :refer [env]]
            [ring.util.response :refer [response content-type]]
            [clojure.data.json :as json]
            [crosswords.generator :refer [generate-crossword]]))

(def home-page
  (html
   [:html
    [:head
     [:title "Crosswords"]
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport"
             :content "width=device-width, initial-scale=1"}]
     (include-css (if (env :dev) "css/site.css" "css/site.min.css"))]
    [:body
     [:div#app]
     (include-js "js/app.js")]]))

(defn get-puzzle [req]
  (let [id (get-in req [:params :id])]
    (-> (generate-crossword id)
        (json/write-str)
        (response)
        (content-type "text/json"))))

(defroutes routes
  (GET "/" [] home-page)
  (GET "/get-puzzle/:id" [id] get-puzzle)
  (resources "/")
  (not-found "Not Found"))

(def app
  (let [handler (wrap-defaults #'routes site-defaults)]
    (if (env :dev) (-> handler wrap-exceptions wrap-reload) handler)))
