(require '[clojure.string :as string]
         '[clojure.set :as set])

(def current-dir (.getParent (java.io.File. *file*)))

;; Stopwords file from: http://www.ranks.nl/stopwords
(def en-stopwords (string/split-lines (slurp (str current-dir "/stopwords.txt"))))

(defn remove-until [text subtext]
  (let [start-of-match-index (.indexOf text subtext)]
    (if (= -1 start-of-match-index)
      text
      (let [end-of-match-index (+ start-of-match-index (.length subtext))]
        (subs text end-of-match-index (.length text))))))

(defn remove-after [text subtext]
  (let [start-of-match-index (.indexOf text subtext)]
    (if (= -1 start-of-match-index)
      text
      (subs text 0 start-of-match-index))))

(defn words-in-text [text]
  (->> (re-seq #"[a-zA-Z]+" text)
       (map string/upper-case)))

(defn sort-frequencies [freqs]
  (->> freqs
       (into [])
       (sort-by second)
       (reverse)))

(defn build-word-regex [word]
  (let [esc-word (string/re-quote-replacement word)]
    (re-pattern (str "(?i)\\b" esc-word "\\b"))))

(defn build-clue [word sentence]
  (let [blanks (string/join (repeat (.length word) "_"))
        word-regex (build-word-regex word)
        blanked-sentence (string/replace sentence word-regex blanks)]
    (str blanked-sentence "\t" word)))

(defn build-word-index [words sentences]
  (let [word-set (set words)
        single-maps (for [sentence sentences
                          word (words-in-text sentence)
                          :when (contains? word-set word)]
                      {word #{sentence}})]
    (apply (partial merge-with set/union) single-maps)))

(defn clues-for-word [word sentences-with-word count]
  (->> sentences-with-word
       (shuffle)
       (take count)
       (map #(build-clue word %))))

(defn text->verses [text]
  (->> text
       (re-seq #"(?s)\d+:\d+ (.*?)(\r\n\r\n\r\n\r\n|\d+:\d+)")
       (map second)
       (map #(string/replace % #"\s+" " "))
       (map string/trim)))
(def configs
  {"kjv" {:src "http://www.gutenberg.org/cache/epub/10/pg10.txt"
          :start "*** START OF THIS PROJECT GUTENBERG EBOOK THE KING JAMES BIBLE ***"
          :stop "*** END OF THIS PROJECT GUTENBERG EBOOK THE KING JAMES BIBLE ***"
          :text->sentences text->verses
          :extra-stopwords ["SHALL" "UNTO" "THY" "THOU" "YE" "THEE" "SAID"
                            "WILL" "HATH" "UPON" "COME" "NOW" "ONE" "US" "ALSO"
                            "SAITH" "THING" "SHALT" "MAKE" "EVERY" "HAST" "LET"
                            "AWAY" "SAY" "PUT" "THINE" "MAY" "GO" "EVEN"
                            "NEITHER" "MADE" "AMONG" "THEREFORE" "WAY" "THEREOF"
                            "TAKE" "CAME" "MADE" "NEITHER" "WENT" "NEITHER"
                            "BROUGHT" "THINGS" "ACCORDING" "TWO" "SPAKE" "MANY"
                            "YET" "BRING" "TOOK" "SENT"]
          :unique-words 250
          :max-clues-per-word 50}})

(defn generate-clues [config]
  (let [text (slurp (:src config))
        text->sentences (:text->sentences config)
        stopwords (set (concat en-stopwords (:extra-stopwords config)))
        sentences (-> text
                      (remove-until (:start config))
                      (remove-after (:stop config))
                      (text->sentences))
        word-list (->> (mapcat words-in-text sentences)
                       (remove #(contains? stopwords %)))
        freqs (frequencies word-list)
        clue-words (->> (sort-frequencies freqs)
                        (take (:unique-words config))
                        (map first))
        word-index (build-word-index clue-words sentences)
        clues (mapcat #(clues-for-word %1 (get word-index %1) (:max-clues-per-word config)) clue-words)]
    (doall (for [clue clues]
             (println clue)))))

(defn main [args]
  (let [config-key (first args)
        config (get configs config-key)]
    (if (nil? config)
      (println (str "Please provide one of the following config keys: " (string/join (keys configs))))
      (generate-clues config))))

(main (rest *command-line-args*))
