(ns crosswords.core
  (:require [reagent.core :as r]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [crosswords.js-util :refer [set-hash!]]
            [crosswords.player :refer [crossword-player]]
            [ajax.core :refer [GET]])
  (:import goog.History))

(defonce puzzle (r/atom nil))


;; -------------------------
;; Views
(defn puzzle-page []
  (let [id (session/get :puzzle-id)]
    [:div
     [:h2 (str "Crossword " id)]
     [:a {:href "/"} "New Crossword"]
     [crossword-player puzzle]]))

(defn app []
  (when-let [page (session/get :current-page)]
    [page]))

;; -------------------------
;; Routes
(secretary/set-config! :prefix "#")

(secretary/defroute puzzle-route "/puzzle/:id" [id]
  ;; Generate a crossword, and when it is finished, set the value of
  ;; puzzle.
  (GET (str "get-puzzle/" id)
       {:response-format :json
        :keywords? true
        :handler #(reset! puzzle %)})
  (session/put! :puzzle-id id)
  (session/put! :current-page #'puzzle-page))

(secretary/defroute home-route "/" []
  (session/put! :current-page nil)
  ;; Go to a puzzle with a random ID between 0 and 999,999. This is an
  ;; arbitrary number, as the puzzle id can be any string.
  (let [puzzle-id (rand-int 1000000)]
    (set-hash! (puzzle-route {:id puzzle-id}))))

;; -------------------------
;; History
;; must be called after routes have been defined
(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))

;; -------------------------
;; Initialize app
(defn mount-components []
  (r/render [#'app] (.getElementById js/document "app")))

(defn init! []
  (hook-browser-navigation!)
  (mount-components))
