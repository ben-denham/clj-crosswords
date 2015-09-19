(ns crosswords.util
  (:require [secretary.core :as secretary :include-macros true]))

(defn set-hash! [location]
  (set! (.-hash js/window.location) location)
  (secretary/dispatch! location))
