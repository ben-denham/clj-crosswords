(ns crosswords.player
  (:require [reagent.core :as r]
            [goog.events :as events]
            [crosswords.js-util :refer [class-list]])
  (:import [goog.events KeyHandler]
           [goog.events.KeyHandler EventType]))

(defn build-square [col row]
  {:col col :row row})
(defn build-cursor [square across?]
  {:square square :across? across?})

(defonce puzzle-atom (r/atom {}))
(defonce cursor-atom (r/atom (build-cursor nil true)))
(defonce game-state-atom (r/atom {}))


(defn squares-in-word [clue]
  (let [across? (:across? clue)
        length (count (:answer clue))
        start-row (:start-row clue)
        start-col (:start-col clue)
        row-count (if across? 1 length)
        col-count (if across? length 1)]
    (set
     (for [row (range start-row (+ start-row row-count))
           col (range start-col (+ start-col col-count))]
       (build-square col row)))))

(defn square-in-word? [square clue]
  (contains? (squares-in-word clue) square))

(defn words-containing-square-inner [square clues]
  (filter #(square-in-word? square %) clues))

(def words-containing-square
  (memoize words-containing-square-inner))

(defn selected-word [cursor clues]
  (->> (words-containing-square (:square cursor) clues)
       (filter #(= (:across? %) (:across? cursor)))
       (first)))

(defn direction-allowed? [cursor clues]
  (some #(= (:across? cursor) (:across? %))
        (words-containing-square (:square cursor) clues)))

(defn word-correct? [clue game-state]
  (let [squares (squares-in-word clue)
        squares-with-correct-letters (map vector squares (:answer clue))]
    (every? (fn [[square correct-letter]] (= (game-state square) correct-letter))
            squares-with-correct-letters)))

(defn square-correct? [square clues game-state]
  (some #(word-correct? % game-state) (words-containing-square square clues)))

(defn puzzle-complete? [puzzle game-state]
  (every? #(word-correct? % game-state) (:clues puzzle)))

(defn update-cursor [square clues]
  (let [old-cursor @cursor-atom
        same-location (= square (:square old-cursor))
        old-across? (:across? old-cursor)
        flipped-across? (if same-location (not old-across?) old-across?)
        new-across? (if (direction-allowed? (build-cursor square flipped-across?) clues)
                      flipped-across? (not flipped-across?))]
    (reset! cursor-atom (build-cursor square new-across?))))


(defn transform-cursor [idx-transform cursor puzzle]
  (let [cur-row (:row (:square cursor))
        cur-col (:col (:square cursor))
        across? (:across? cursor)
        new-row (if across? cur-row (idx-transform cur-row))
        new-col (if across? (idx-transform cur-col) cur-col)]
    (if (nil? (get-in (:grid puzzle) (map #(keyword (str %)) [new-row new-col])))
      cursor
      (build-cursor (build-square new-col new-row) across?))))

(defn next-cursor [cursor puzzle]
  (transform-cursor inc cursor puzzle))

(defn prev-cursor [cursor puzzle]
  (transform-cursor dec cursor puzzle))


(defn handle-keypress [e]
  (let [puzzle @puzzle-atom
        cursor @cursor-atom
        cur-square (:square cursor)]
    (defn letter-press [keycode]
      (let [letter (.fromCharCode js/String keycode)]
        (swap! cursor-atom next-cursor puzzle)
        (swap! game-state-atom assoc cur-square letter)))
    (defn letter-keycode? [keycode]
      (and (>= keycode 65) (<= keycode 90)))
    (defn backspace-press []
      (swap! cursor-atom prev-cursor puzzle)
      (swap! game-state-atom assoc cur-square nil))
    (let [keycode (.-keyCode e)]
      (.preventDefault e)
      (cond (letter-keycode? keycode) (letter-press keycode)
            (= 8 keycode) (backspace-press)))))


(defn crossword-table [puzzle cursor game-state]
  (let [grid (:grid puzzle)
        clues (:clues puzzle)
        grid-size (:grid-size puzzle)
        active-word (selected-word cursor clues)]
    (defn crossword-table-cell [col-idx row-idx cell]
      (if (nil? cell)
        [:td.empty]
        (let [square (build-square col-idx row-idx)
              classes (class-list {"selected" (= square (:square cursor))
                                   "active" (square-in-word? square active-word)
                                   "correct" (square-correct? square clues game-state)})
              click-handler #(update-cursor square clues)]
          [:td {:on-click click-handler
                :class classes}
           (when-let [letter (get game-state square)] [:span letter])
           (if-let [number (:number cell)]
             [:span.clue-number number])])))
    (defn crossword-table-row [row-idx row]
      (let [cells (->> (range 0 grid-size)
                       (map #(get row (keyword (str %)))))]
        [:tr
         (for [[idx cell] (map-indexed vector cells)]
           ^{:key idx} [crossword-table-cell idx row-idx cell])]))
    (let [rows (->> (range 0 grid-size)
                    (map #(get grid (keyword (str %)))))]
      [:div
       [:table.crossword-table {:cell-spacing 0}
        [:tbody
         (for [[idx row] (map-indexed vector rows)]
           ^{:key idx} [crossword-table-row idx row])]]
       (when (puzzle-complete? puzzle game-state)
         [:h3 {:class "win"} "Congratulations, you win!"])])))


(defn crossword-clues [puzzle]
  (defn crossword-clue [clue]
    (let [clue-number (:number clue)
          clue-text (:clue clue)
          clue-length (count (:answer clue))]
      [:p (str clue-number ". " clue-text " (" clue-length ")")]))
  (defn crossword-clues-list [title clues]
    [:div
     [:h3 title]
     (for [clue clues]
       ^{:key (:number clue)} [crossword-clue clue])])
  (let [clues (:clues puzzle)
        across (sort-by :number (filter #(:across? %) clues))
        down (sort-by :number (filter #(not (:across? %)) clues))]
    [:table.crossword-clues
     [:tr {:class "top-aligned"}
      [:td [crossword-clues-list "Across" across]]
      [:td [crossword-clues-list "Down" down]]]]))


(defn crossword-player [puzzle-atom-input]
  (let [puzzle @puzzle-atom-input
        cursor @cursor-atom
        game-state @game-state-atom]
    (reset! puzzle-atom puzzle)
    (if (nil? puzzle)
      [:p "Loading..."]
      [:div.crossword-player {:on-keydown handle-keypress}
       [:table [:tr {:class "top-aligned"}
                [:td {:width 550} [crossword-table puzzle cursor game-state]]
                [:td {:width 550} [crossword-clues puzzle]]]]])))

(defn init-handlers []
  (events/listen (KeyHandler. js/document) EventType.KEY handle-keypress))

(defonce init
  (init-handlers))
