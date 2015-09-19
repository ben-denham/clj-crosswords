(ns crosswords.clues
  (:require [cljx-sampling.random :as random]
            [clojure.java.io :refer [reader resource]]
            [clojure.string :as string]))

(def clues-path (resource "clues.tsv"))

(defn- file-line-count [file-path]
  "Returns the number of lines in the file at file-path."
  (with-open [rdr (reader file-path)]
    (count (line-seq rdr))))

(defn- random-line [rng file-path]
  "Retrieves a random line from file-path based on the rng."
  (let [line-count (file-line-count file-path)
        random-line-index (random/next-int! rng line-count)]
    (with-open [rdr (reader file-path)]
      (->> (line-seq rdr)
           (drop random-line-index) ;; Drop lines before
                                    ;; random-line-index.
           (first)))))

(defn- random-clue [rng]
  "Retrieves a random clue (a vector of [clue answer]) from the clues
  file based on the rng. The clues file is expected to contain one
  clue on each line, with clue text followed by a tab followed by the
  answer."
  (let [clue-line (random-line rng clues-path)]
    (string/split clue-line #"\t")))

(defn random-clue-generator [seed]
  "Returns a function that returns a random clue each time it is
  called based on the provided rng seed."
  (let [rng (random/create seed)]
    #(random-clue rng)))
