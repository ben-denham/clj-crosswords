(ns crosswords.js-util
  (:require [secretary.core :as secretary :include-macros true]
            [clojure.string :refer [join]]))

(defn set-hash! [location]
  (set! (.-hash js/window.location) location)
  (secretary/dispatch! location))

(defn class-list [classes]
  (join " "
        (for [[class include?] classes
              :when include?]
          class)))
