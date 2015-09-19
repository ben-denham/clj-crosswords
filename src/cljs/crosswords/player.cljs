(ns crosswords.player
  (:require [reagent.core :as reagent :refer [atom create-class]]
            [goog.events :as events])
  (:import [goog.events KeyHandler]
           [goog.events.KeyHandler EventType]))

(defrecord Square [col row])

(defonce cursor (atom {:square nil
                       :across? false}))
(defonce game-state {})

(def key-handler (KeyHandler. js/document))

(defn- square-in-word [square clue]
  (let [across? (:across? clue)
        length (count (:answer clue))
        start-col (:start-col clue)
        start-row (:start-row clue)
        end-col (if across? (+ start-col length) start-col)
        end-row (if across? start-row (+ start-row length))]
    (and (and (>= (:col square) start-col) (<= (:col square) end-col))
         (and (>= (:row square) start-row) (<= (:row square) end-row)))))

(defn- containing-words [puzzle square]
  (filter #(square-in-word square %) (:clues puzzle)))

(defn- direction-allowed? [puzzle across? square]
  (some #(= across? (:across? %)) (containing-words puzzle square)))

(defn- update-cursor [puzzle square]
  (let [old-cursor @cursor
        same-location (= square (:square old-cursor))
        old-across? (:across? old-cursor)
        flipped-across? (if same-location (not old-across?) old-across?)
        new-across? (if (direction-allowed? puzzle flipped-across? square)
                      flipped-across? (not flipped-across?))]
    (reset! cursor {:square square
                    :across? new-across?})))

(defn- handle-keypress [e]
  (let [keycode (.-keyCode e)
        char (.fromCharCode js/String keycode)]
    (js/console.log keycode char)))

(defn- crossword-table-cell [puzzle cursor col-idx row-idx cell]
  (if (nil? cell)
    [:td.empty]
    (let [square (Square. col-idx row-idx)
          classes (if (= square (:square cursor)) "selected" "")]
      [:td {:on-click #(update-cursor puzzle square)
            :class classes}
       (if-let [number (:number cell)]
         [:span.clue-number number])])))

(defn- crossword-table-row [puzzle cursor row-idx row]
  (let [grid-size (:grid-size puzzle)
        cells (->> (range 0 grid-size)
                   (map #(get row (keyword (str %)))))]
    [:tr
     (for [[idx cell] (map-indexed vector cells)]
       ^{:key idx} [crossword-table-cell puzzle cursor idx row-idx cell])]))

(defn- crossword-table [puzzle cursor]
  (let [grid (:grid puzzle)
        grid-size (:grid-size puzzle)
        rows (->> (range 0 grid-size)
                  (map #(get grid (keyword (str %)))))]
    [:table.crossword-table {:cell-spacing 0}
     (for [[idx row] (map-indexed vector rows)]
       ^{:key idx} [crossword-table-row puzzle cursor idx row])]))

(defn- crossword-clue [clue]
  (let [clue-number (:number clue)
        clue-text (:clue clue)
        clue-length (count (:answer clue))]
    [:p (str clue-number ". " clue-text " (" clue-length ")")]))

(defn- crossword-clues-list [title clues]
  [:div
   [:h3 title]
   (for [clue clues]
     ^{:key (:number clue)} [crossword-clue clue])])

(defn- crossword-clues [puzzle]
  (let [clues (:clues puzzle)
        across (sort-by :number (filter #(:across? %) clues))
        down (sort-by :number (filter #(not (:across? %)) clues))]
    [:table.crossword-clues
     [:tr
      [:td [crossword-clues-list "Across" across]]
      [:td [crossword-clues-list "Down" down]]]]))

(defn crossword-player-inner [puzzle-atom]
  (let [puzzle @puzzle-atom
        cursor @cursor]
    (if (nil? puzzle)
      [:p "Loading..."]
      [:div.crossword-player
       [:table [:tr
                [:td {:width 550} [crossword-table puzzle cursor]]
                [:td {:width 550} [crossword-clues puzzle]]]]])))

(defn- crossword-player-setup []
  (events/listen key-handler EventType.KEY handle-keypress))

(defn- crossword-player-teardown []
  (events/unlisten key-handler EventType.KEY handle-keypress))

(def crossword-player
  (create-class
   {:component-did-mount crossword-player-setup
    :component-will-unmount crossword-player-teardown
    :display-name "crossword-table"
    :reagent-render crossword-player-inner}))
