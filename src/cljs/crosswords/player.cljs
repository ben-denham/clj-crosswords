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

(defn get-clue-id [clue]
  (str (:number clue) "-" (if (:across? clue) "across" "down")))

(defn squares-in-word [clue]
  (let [across? (:across? clue)
        length (count (:answer clue))
        start-row (:start-row clue)
        start-col (:start-col clue)
        row-count (if across? 1 length)
        col-count (if across? length 1)]
    (for [row (range start-row (+ start-row row-count))
          col (range start-col (+ start-col col-count))]
      (build-square col row))))

(defn square-in-word? [square clue]
  (contains? (set (squares-in-word clue)) square))

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


(defn valid-cursor-position? [square grid]
  (let [row (:row square)
        col (:col square)
        grid-keys (map #(keyword (str %)) [row col])]
    (not (nil? (get-in grid grid-keys)))))

(defn transform-cursor [col-transform row-transform cursor puzzle]
  (let [grid (:grid puzzle)
        clues (:clues puzzle)
        row (:row (:square cursor))
        col (:col (:square cursor))
        across? (:across? cursor)
        new-square (build-square (col-transform col)
                                 (row-transform row))
        new-across? (if (direction-allowed? (build-cursor new-square across?) clues)
                      across? (not across?))]
    (if (valid-cursor-position? new-square grid)
      (build-cursor new-square new-across?)
      cursor)))

(defn next-cursor [cursor puzzle]
  (let [across? (:across? cursor)
        row-transform (if across? identity inc)
        col-transform (if across? inc identity)]
    (transform-cursor col-transform row-transform cursor puzzle)))

(defn prev-cursor [cursor puzzle]
  (let [across? (:across? cursor)
        row-transform (if across? identity dec)
        col-transform (if across? dec identity)]
    (transform-cursor col-transform row-transform cursor puzzle)))

(defn up-cursor [cursor puzzle]
  (transform-cursor identity inc cursor puzzle))
(defn down-cursor [cursor puzzle]
  (transform-cursor identity dec cursor puzzle))
(defn left-cursor [cursor puzzle]
  (transform-cursor dec identity cursor puzzle))
(defn right-cursor [cursor puzzle]
  (transform-cursor inc identity cursor puzzle))

(defn sort-clues [clues]
  (let [clue-direction-groups (group-by :across? clues)
        across-clues (get clue-direction-groups true)
        down-clues (get clue-direction-groups false)]
    (concat (sort-by :number across-clues)
            (sort-by :number down-clues))))

(defn next-word-cursor [cursor puzzle]
  (let [clues (:clues puzzle)
        active-word (selected-word cursor clues)
        sorted-clues (cycle (sort-clues clues))
        next-word (second (drop-while #(not= active-word %) sorted-clues))
        new-square (build-square (:start-col next-word) (:start-row next-word))]
    (build-cursor new-square (:across? next-word))))
(defn prev-word-cursor [cursor puzzle]
  (let [clues (:clues puzzle)
        active-word (selected-word cursor clues)
        sorted-clues (cycle (reverse (sort-clues clues)))
        next-word (second (drop-while #(not= active-word %) sorted-clues))
        new-square (build-square (:start-col next-word) (:start-row next-word))]
    (build-cursor new-square (:across? next-word))))

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
    (defn move-cursor [transform]
      (swap! cursor-atom transform puzzle))
    (let [keycode (.-keyCode e)
          shift? (.-shiftKey e)]
      (.preventDefault e)
      (cond (letter-keycode? keycode) (letter-press keycode)
            (= 37 keycode) (move-cursor left-cursor)
            (= 38 keycode) (move-cursor down-cursor)
            (= 39 keycode) (move-cursor right-cursor)
            (= 40 keycode) (move-cursor up-cursor)
            (and (= 9 keycode) shift?) (move-cursor prev-word-cursor) ;; shift-tab
            (= 9 keycode) (move-cursor next-word-cursor) ;; tab
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
      [:div#crossword-table
       [:table {:cell-spacing 0}
        [:tbody
         (for [[idx row] (map-indexed vector rows)]
           ^{:key idx} [crossword-table-row idx row])]]
       (when (puzzle-complete? puzzle game-state)
         [:h3 {:class "win"} "Congratulations, you win!"])])))


(defn crossword-clues [puzzle cursor]
  (let [active-word (selected-word cursor (:clues puzzle))]
    (defn crossword-clue [clue]
      (let [clue-number (:number clue)
            clue-text (:clue clue)
            clue-length (count (:answer clue))
            id (get-clue-id clue)
            classes (class-list {"selected" (= clue active-word)})]
        [:p {:class classes :id id} (str clue-number ". " clue-text " (" clue-length ")")]))
    (defn crossword-clues-list [title clues]
      [:div
       [:div
        (for [[idx clue] (map-indexed vector clues)]
          ^{:key idx} [crossword-clue clue])]])
    (let [clues (:clues puzzle)
          across (sort-by :number (filter #(:across? %) clues))
          down (sort-by :number (filter #(not (:across? %)) clues))]
      [:div
       [:table {:width "100%" :position "fixed"}
        [:tr
         [:td {:width "50%"} [:h3 "Across"]]
         [:td {:width "50%"} [:h3 "Down"]]]]
       [:div#crossword-clues
        [:table
         [:tr {:class "top-aligned"}
          [:td {:width "50%"} [crossword-clues-list "Across" across]]
          [:td {:width "50%"} [crossword-clues-list "Down" down]]]]]])))


(defn crossword-player [puzzle-atom-input]
  (let [puzzle @puzzle-atom-input
        cursor @cursor-atom
        game-state @game-state-atom]
    (reset! puzzle-atom puzzle)
    (if (nil? puzzle)
      [:p "Loading..."]
      [:div.crossword-player {:on-keydown handle-keypress}
       [:table [:tr {:class "top-aligned"}
                [:td {:width 510} [crossword-table puzzle cursor game-state]]
                [:td {:width 510} [crossword-clues puzzle cursor]]]]])))

(defn scroll-to-active-clue-cursor-watch []
  (let [puzzle @puzzle-atom
        cursor @cursor-atom
        active-word (selected-word cursor (:clues puzzle))
        clue-id (get-clue-id active-word)
        clues-container (.getElementById js/document "crossword-clues")
        clue-element (.getElementById js/document clue-id)
        clue-offset (.-offsetTop clue-element)]
    (aset clues-container "scrollTop" clue-offset)))

(defn init-handlers []
  (add-watch cursor-atom :scroll-to-active-clue scroll-to-active-clue-cursor-watch)
  (events/listen (KeyHandler. js/document) EventType.KEY handle-keypress))

(defonce init
  (init-handlers))
