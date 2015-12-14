(require '[clojure.java.io :as io]
         '[clojure.string :as string]
         '[clojure.data.json :as json])

(def dictionary-file-path "/usr/share/dict/british-english")

(def words
  (if (.exists (io/as-file dictionary-file-path))
    (with-open [dictionary-file (io/reader dictionary-file-path)]
      ;; Use doall to force the expression to be evaluated while the file is open
      (doall (->> dictionary-file
                  (line-seq)
                  (map string/upper-case)
                  (set))))
    ;; Return an empty collection if the file does not exist.
    []))

(defn useful-clue? [clue]
  "Returns true if the clue is deemed 'useful'. Clues that reference
  other clues are deemed unuseful."
  (let [clue-val (:clue clue)]
    (and
     ;; Exclude clues with blank clue text.
     (not (empty? clue-val))
     ;; Exclude clues with only one letter or less.
     (> (count (:answer clue)) 1)
     ;; Exclude clues with answers longer than 15 letters.
     (<= (count (:answer clue)) 15)
     ;; Exclude clues that contain "across" or "down".
     (nil? (re-find #"((?i)across|down)" clue-val))
     ;; Exclude clues that mention other clues like "A1" or "D3".
     (nil? (re-find #"(?i)[ad][\d]+" clue-val)))))

(defn real-word-clue? [clue]
  (let [answer (:answer clue)]
    (if (empty? words)
      ;; Return true if the dictionary of words is empty.
      true
      ;; Return whether the answer is in the list of words.
      (contains? words answer))))

(defn parse-clue [clue-line]
  "Convert a clue line to a map."
  (let [[clue answer & rest] (string/split clue-line #"\t")]
    {:clue clue
     :answer (string/upper-case answer)}))

(defn read-clues [clues-path]
  "Returns a seq of lines in the file located at clues-path."
  (with-open [clues-file (io/reader clues-path)]
    ;; Use doall to force the expression to be evaluated while the file is open
    (doall (line-seq clues-file))))

(defn format-clue [clue]
  (str (:clue clue) "\t" (:answer clue)))

(defn format-clues [clues]
  (->> (map format-clue clues)
       (string/join "\n")))

(defn sort-clues [clues]
  (reverse (sort-by #(count (:answer %)) clues)))

(defn main [args]
  "Generate processed clues from original clues file (Tested with
  https://github.com/donohoe/nyt-crossword/blob/master/clues.txt)."
  (if-let [clues-path (first args)]
    (->> (read-clues clues-path)
         (map parse-clue)
         (filter useful-clue?)
         (filter real-word-clue?)
         (sort-clues)
         (format-clues)
         (print))
    (println "Please provide a clues path as the first argument")))

(main (rest *command-line-args*))
