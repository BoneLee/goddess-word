(ns goddess-word.core (:gen-class))
(require '[clojure.java.io :as io])
(use '[clojure.string :only (join split)])
(use '[clojure.set :only (union)])
(use '[clojure.contrib.combinatorics :only  (combinations)])

(defn assert-msg 
  "if assertion false print error msg"
   [expr msg]
  (if-not expr (do (println msg) (assert false))))

(defn levenshtein-distance [str1 str2]
  "calculate levenshtein distance between two words."
  {:pre [(and (string? str1) (string? str2))],
   };:post [(>= % 0)]} not work as a clojure bug
  (let [len1 (count str1)
        len2 (count str2)]
    (cond (zero? len1) len2
          (zero? len2) len1
          :else
          (let [cost (if (= (first str1) (first str2)) 0 1)]
            (min (inc (levenshtein-distance (rest str1) str2))
                 (inc (levenshtein-distance str1 (rest str2)))
                 (+ cost
                    (levenshtein-distance (rest str1) (rest str2))))))))

;; We cast a spell more powerful than either:
(def ^:dynamic levenshtein-distance (memoize levenshtein-distance))

(defn in? 
    "true if seq contains element."
    [seq element]  
    (some #(= element %) seq))

(defn get-word-by-distance 
  "find the word with a distance to center word."
  [word-graph center-word distance]
  (first (first 
    (filter 
      #(= distance (get % 1)) 
      (get word-graph center-word)))))

(defn find-matched-word-place  
  "find the word position put in the word graph."
  [word-graph center-word word-to-match]
  (let [distance (levenshtein-distance word-to-match center-word) 
        next-word (get-word-by-distance word-graph center-word distance)]
      (if (nil? next-word)
          center-word 
          (find-matched-word-place (get word-graph next-word) next-word word-to-match))))

(defn make-word-graph
  "make word graph(bk-trees) by given words set and a central word."
  [words center-word]
  {:pre [(and (set? words) (> (count words) 0)) (> (count center-word) 0) (contains? words center-word)]}
  ;algorithm:
  ;initialize word-graph={:center-word {}, :all-other-words {}}
  ;non-transient word-graph
  ;foreach word in words:
  ;  distance = calculate word to the center word.
  ;  if the center word already has not the distance:
  ;    append an edge to the :center-node in the graph with distance.
  ;    update the word node with a new edge.
  ;    caution: two edge.
  ;  else:
  ;    append the word to the founded child word node with steps:
  ;      calculate the distance to the child node.
  ;      if not found distance-matched node:
  ;         append word to it
  ;      else:
  ;         reclusively found a matched child node.
  (def word-graph (atom {}))

  (doseq [word words]
    (swap! word-graph assoc word {}))
  
  (doseq [word (disj words center-word)]
    (do
      (assert (contains? @word-graph word))
      (let [matched-word (find-matched-word-place @word-graph center-word word)
            distance (levenshtein-distance matched-word word)
            matched-edges (get @word-graph matched-word)
            edges (get @word-graph word)]
        (do
             (swap! word-graph assoc matched-word (assoc matched-edges word distance))))))
             ;(swap! word-graph assoc word (assoc edges matched-word distance))))))

  @word-graph)

(defn abs [number]
  (if (neg? number) (- number) number))

(defn in-range [number low high]
  {:pre (<= low high)}
  (and (>= number low) (<= number high)))

(defn search-word   
  "search word in word graph."
  [word-graph root-word word distance]
  {:pre [(> (count word-graph) 0) (>= distance 0) (contains? word-graph root-word)]}
 
  (if-not (contains? word-graph word) {} 
    (do
  (def result (atom {}))
  (defn search-graph [word-graph root-word word distance]
    ;algorithm
  ;search(graph root-word word distance):
    ;init result =[]
    ;dist=calcute root-word and word distance
    ;range=(|dist-distance|,|dist+distance|)
    ;if dist <= distance:
    ;   push the root node word in result.
    ;edges= root-word of its edges
    ;nodes= grep the child nodes which edge is in range
    ;foreach node in nodes:
    ;   push search(graph node word distance) into result if searched not null.
    (let [d (levenshtein-distance word root-word)]
      (do
        (when (<= d distance) (swap! result assoc root-word d))
        (let [low (- d distance)
              high (+ distance d)]  
          (doseq [edge (filter 
                    #(in-range (get % 1) low high) 
                    (get word-graph root-word))]
            (search-graph word-graph (first edge) word distance))))))
  (search-graph word-graph root-word word distance)
  @result)))


(defn import-en-dict
  "import words from file."
  [file]
  {:pre [(.exists (io/file file))]}
  (reduce into {} (for [line (split (slurp file) #"\n") :when (not= \# (first line))]
    (let [word-meaning (split line #"\s+")]
      (assert-msg  (= 2 (count word-meaning)) (str "line:" line))
      {(first word-meaning) 
       (let [attr-meaning (second word-meaning)]
          (assert-msg  (re-matches #"[a-zA-Z]+\.([|&][a-zA-Z]+\.)*.*" attr-meaning) (str "line:" line))
          (into [] (for [[_ attr _ meaning] (re-seq  #"([a-zA-Z]+\.([|&][a-zA-Z]+\.)*)(\W+)" attr-meaning)] (into [attr] (split meaning #";")))))}))))
 
(defn import-zh-dict
  "import chinese words from file."
  [file]
  {:pre [(.exists (io/file file))]}
  (reduce 
    (fn [reduced-map to-add-in] (let [[[key val]] (seq to-add-in)] (assoc reduced-map key (union (reduced-map key) val))))
    {}
  ;(reduce into {} (for [line (split (slurp file) #"\n") :when (not= \# (first line))]
    (for [line (split (slurp file) #"\n") :when (not= \# (first line))]
     (let [word-meaning (split line #"==>")]
      (assert-msg  (= 2 (count word-meaning)) (str "line:" line))
      {(first word-meaning) 
       (let [attr-meaning (second word-meaning)]
          (assert-msg  (re-matches #"^[(\w\"\-\.].*" attr-meaning) (str "line:" line))
          (set (split attr-meaning #"\s*;\s*")))}))))
 
(defn import-word-neighbor-dict
  "import word neighbor from file."
  [file]
  {:pre [(.exists (io/file file))]}
  (reduce into {} (for [line (split (slurp file) #"\n") :when (not= \# (first line))]
    (let [word-neighbors (split line #":")]
      (assert-msg  (= 2 (count word-neighbors)) (str "line:" line))
      {(first word-neighbors)
       (let [attr-neighbors (second word-neighbors)]
          (assert-msg  (re-matches #"^[a-zA-Z]+.*$" attr-neighbors) (str "line:" line))
          (into {} (for [[_ neighbor distance] (re-seq  #"(\w+)=>\((\d)\)" attr-neighbors)] {neighbor (Integer. distance)})))}))))

(defn third 
  "like first, second function."
  [coll]
  (assert-msg (coll? coll) "third argument should be a collection.")
  (nth coll 2))

(defn import-irlab-dict
  "imort irlab chinese dict."
  [file]
  {:pre [(.exists (io/file file))]}
    (vec (for [line (split (slurp file) #"\n") :when (not= \# (first line))]
     (let [ir-tree-line (split line #":")]
      (assert-msg  (= 3 (count ir-tree-line)) (str "line:" line))
      (let [tag-id (first ir-tree-line) attr (second ir-tree-line) meaning (third ir-tree-line)]
          (assert-msg  (or (= "@" attr) (= "#" attr) (= "=" attr)) (str "line:" line))
          {:tag tag-id :attr attr :meaning (split meaning #"\s+")})))))

(defn import-irlab-dict-map 
  "get the line no. of irlab dict by a certain word."
  [irlab-dict-words]
  (reduce into {} 
    (for [i (range (count irlab-dict-words))] 
      (let [meanings (:meaning  (get irlab-dict-words i))]  
        (zipmap meanings  (repeat (count meanings) i)))))) 

(defn choose-matched-words
  "get words collection by @#="
  [irlab-line is-self-line]
  (let [attr (:attr irlab-line) meaningful-words (:meaning irlab-line)]
    (condp = attr
      "=" (do 
              (assert-msg (<= 1 (count meaningful-words)) (str "= must >= 1.check:" irlab-line)) 
              (if-not is-self-line [(rand-nth meaningful-words)] []))
      "@" (do (assert-msg (= 1 (count meaningful-words)) (str "@ must == 1.check:" irlab-line)) meaningful-words)
      "#" (do (assert-msg (<= 1 (count meaningful-words)) (str "# must >= 1.check:" irlab-line)) meaningful-words)
      (assert-msg false ("error in irlab file:check " irlab-line)))))


(defn zmap
  "map [a b c] number to {a number, b number, c number}."
  [coll number]
  (assert-msg (coll? coll) "zmap argument error.")
  (zipmap coll (repeat (count coll) number)))

(defn generate-jacent-words
  "generate words map in 2 levenshtein distance."
  [word-set]
  (reduce 
    (fn [reduced-map to-add-in] (if (empty? to-add-in) reduced-map (recur (assoc reduced-map (ffirst to-add-in) (merge (reduced-map (ffirst to-add-in)) (second (first to-add-in)))) (rest to-add-in))))
    {}
    (for [word-comb (combinations word-set 2)] 
     (let [word1 (first word-comb) word2 (second word-comb) distance (levenshtein-distance word1 word2)]
       (if (>= 2 distance) (do  {word1 {word2 distance} word2 {word1 distance}}) {})))))

(defn join-to-a-string-vector
  "concat to a string vector."
  [data]
  (cond 
    (string? data) (list data)
    (every? string? data) (seq data)
    :else (reduce concat (for [multi-vec data] (join-to-a-string-vector multi-vec)))))

(defn now
  "current time in seconds."
  []
  (quot (System/currentTimeMillis) 1000))
(def now-time (now)) ;use for random-choose

(defn choose-by-time
  "choose seq by time."
  [seq]
  (get (vec seq) (mod now-time (count seq))))


(defn remove-unwanted-char-by-attr
  "remove unwanted in zh word."
  [word attr]
  (if (and (re-matches #".*(a[djv]*\.).*" attr) (or (= 3 (count word)) (= 5 (count word)))) (clojure.string/replace word  #"(地)|(的)$" "") word))

(defn get-fixed-word-meaning
  "get meaning removed zh de di."
  [word-meaning attr]
  #_(println "******check:******" word-meaning)
  (choose-by-time (for [same-meaning (split word-meaning #"\s*,\s*")] (remove-unwanted-char-by-attr same-meaning attr))))

(defn import-zh-words-as-random-option
  "import zh-words as random option from basic.txt."
  [file]
  {:pre [(.exists (io/file file))]}
  (reduce into [] (for [line (split (slurp file) #"\n") :when (not= \# (first line))] (split line #"\s+"))))


(defn init []
  (def option-zh-words (import-zh-words-as-random-option "resources/option-zh-words/words.txt"))
  (def en-dict (import-en-dict "resources/dict/en-zh.txt"))
  (def zh-dict (import-zh-dict "resources/dict/zh-en.txt"))
  #(def irlab-dict-vec (import-irlab-dict "resources/chinese-words-classification/HIT-IRLab-full_2005.3.3.txt"))
  (def irlab-dict-vec (import-irlab-dict "resources/chinese-words-classification/trimed-irlab.txt"))
  (def irlab-dict-map (import-irlab-dict-map irlab-dict-vec))

  #_(def en-dict-jacent-words (generate-jacent-words (set (keys en-dict))))
  (def en-dict-jacent-words (import-word-neighbor-dict "resources/dict/word-neighbor.txt")))

(defn search-en-word-by-distance
  "search word by a lein distance."
  [target-word distance]
  (assert-msg (> (count target-word) 0) "target word should not be null.")
  (assert-msg (>= distance 0) "distance should >=0.")
  (def central-word "goddess")
  (def en-dict-word-graph (make-word-graph (set (keys en-dict)) central-word))
  (search-word en-dict-word-graph central-word target-word distance))

(defn get-words-random-translation 
  "get words translation."
  [words-coll]
  (filter #(not= nil %) 
    (for [word words-coll]
      (if-let [word-meaning (get zh-dict word)] {:zh  word, :en (rand-nth (vec word-meaning))} nil))))

(defn choose-matched-en-words
  "get words collection by @#="
  [irlab-line is-self-line]
  (let [attr (:attr irlab-line) meaningful-words (:meaning irlab-line) random-translation (get-words-random-translation meaningful-words)]
    (condp = attr
      "=" (do 
              (assert-msg (<= 1 (count meaningful-words)) (str "= must >= 1.check:" irlab-line))
              (if (and (not is-self-line) (seq random-translation))
                [(rand-nth random-translation)] []))
      "@" (do (assert-msg (= 1 (count meaningful-words)) (str "@ must == 1.check:" irlab-line)) random-translation)
      "#" (do (assert-msg (<= 1 (count meaningful-words)) (str "# must >= 1.check:" irlab-line)) random-translation)
      (assert-msg false ("error in irlab file:check " irlab-line)))))

(defn search-zh-word-by-distance
  "search chinese word in some distance."
  [target-word distance]
  (assert-msg (> (count target-word) 0) "target word should not be null.")
  (assert-msg (>= distance 0) "distance should >=0.")
  (if-not (contains? irlab-dict-map target-word) 
    {} 
    (let [index (irlab-dict-map target-word) low-bound 0 high-bound (count irlab-dict-vec)
           from (if (< (- index distance) low-bound)  low-bound (- index distance)) 
           to (if (> (+ index distance) high-bound) high-bound (+ index distance))] 
     (reduce into {} 
      (for [i (range from to)] 
        (let [choosed-words (choose-matched-en-words (get irlab-dict-vec i) (= index i)) neighbor-distance (abs (- i index))]
          #_(if (zero? neighbor-distance) (println "****debug check choosed-words:" choosed-words))
          #_(println "****debug check target-word:" target-word " index:" index " i:" i " choosed-words:" choosed-words " neighbor-distance:" neighbor-distance)
          (if-not (seq choosed-words) {} (zmap choosed-words neighbor-distance))))))))

(defn get-relevant-words 
  "get relevant words by a word's meaning."
  [word-meaning attr]
  ;ad. a.
  (let [results (filter #(seq %) (for [same-meaning (split word-meaning #"\s*,\s*")] (search-zh-word-by-distance (remove-unwanted-char-by-attr same-meaning attr) 10)))]
      #_(println "*********debug:check results: " results " word-meaning:" word-meaning " attr:" attr)
      (if (seq results) (choose-by-time results) {})))  

(defn import-question-bank
  "import QA words from file."
  [file]
  {:pre [(.exists (io/file file))]}
  (reduce into {} (for [line (split (slurp file) #"\n") :when (not= \# (first line))]
    (let [word-meaning (split line #"\s+")]
      (assert-msg  (= 2 (count word-meaning)) (str "line:" line))
      {(first word-meaning) 
        {:levenshtein-neighbors (do #_(println "check word:" (first word-meaning)) (en-dict-jacent-words (first word-meaning))), ;;(search-en-word-by-distance (first word-meaning) 2),
         :attr-meaning 
           (let [attr-meaning (second word-meaning)]
             (assert-msg  (re-matches #"(([a-zA-Z]+\.([|&][a-zA-Z]+\.)*)([\W]+))+" attr-meaning) (str "error line:" line))
             #_(assert-msg  (re-matches  #"[a-zA-Z]+\.([|&][a-zA-Z]+\.)*.*" attr-meaning) (str "error line:" line))
             (into [] (for [[_ attr _ meaning] (re-seq  #"([a-zA-Z]+\.([|&][a-zA-Z]+\.)*)([\W]+)" attr-meaning)] ;a bug x-ray  n.x-line;bla bla...
                 {:attr attr, :meaning
                    (into []
                     (for [single-meaning (split meaning #";")]
                       (let [relevant-words (get-relevant-words single-meaning attr)]
                       #_(println "******debug check line:" line " single-meaning:" single-meaning  " relevant-words:" relevant-words)
                       {:content (get-fixed-word-meaning single-meaning attr), :relevant-words relevant-words})))})))}}))))
 


(defn weighted-rand-choice [m]
    "choose by map weighted values."
    {:pre [(< 0 (count (keys m)))]}
    (let [w (reductions #(+ % %2) (vals m))
          r (rand-int (last w))]
         (nth (keys m) (count (take-while #( <= % r ) w)))))

(defn rand-choose3-in-weighted-map
  "choose 3 elements in map by weighted val."
  [key-val-pairs]
  #{:pre [(<= 3 (count (keys key-val-pairs)))]}
  (assert-msg (<= 3 (count (keys key-val-pairs))) (str "error: check key-val-pairs:" key-val-pairs))
  (let [key1 (weighted-rand-choice key-val-pairs) key2 (weighted-rand-choice (dissoc key-val-pairs  key1)) key3 (weighted-rand-choice (dissoc key-val-pairs key1 key2))]
    [key1 key2 key3]))

(defn rand3-in-vec
  "return 3 random elements in vector."
  [coll]
  {:pre [(<= 3 (count coll))]}
  [(rand-nth coll) (rand-nth coll) (rand-nth coll)])

(defn get-three-relevant-words
  "get 3 zh relevant words."
  [target-word relevant-words]
  #_(println "****debug target-word:" target-word " relevant-words:" relevant-words)
  (let [filtered-words (into {} (for [[key val] relevant-words :when (and (not (zero? val)) (not= (:zh key) target-word))] {key (- 11 val)}))] 
   (if 
    (<= 3 (count (keys filtered-words))) 
    (map :zh (rand-choose3-in-weighted-map filtered-words)) ;;fix multi option with the same meaning
    (rand3-in-vec  option-zh-words)))) ;;choose rand3 zh words in irlab. some bug to fix

(defn get-three-relevant-en-words
  "get 3 en relevant words."
  [relevant-words]
  (if
    (<= 3 (count (keys relevant-words)))
    (map :en (rand-choose3-in-weighted-map
                (into {} (for [[key val] relevant-words] {key (- 11 val)}))))
    (rand3-in-vec (keys en-dict))))

(defn cub [num] (* num num num))
(defn get-three-lev-neighbor-words 
  "get 3 levenshtein neighbor words. if has less 3, random choose meaning-relevant words to complement."
  [lev-neighbors words-with-relevant-meaning]
  (let [lev-neighbors-cnt (count (keys lev-neighbors))]
    (cond 
      (zero? lev-neighbors-cnt) (get-three-relevant-en-words words-with-relevant-meaning)
      (<= 3 lev-neighbors-cnt) (rand-choose3-in-weighted-map
                (into {} (for [[key val] lev-neighbors] #_(println "val:" val " key:" key) {key (cub (/ 2 val))}))) 
      :else (let [neighbors (keys lev-neighbors)]
              (assert-msg (and (< 0 lev-neighbors-cnt) (> 3 lev-neighbors-cnt)) "logic error.")
              #_(println "neighbors:" lev-neighbors " relevant-words:" words-with-relevant-meaning)
              (concat neighbors (subvec (vec (get-three-relevant-en-words words-with-relevant-meaning)) 0 (- 3 lev-neighbors-cnt)))))))

(defn gen-en2zh-question
  "generate a question."
  [word-meaning]
  (let [word (first word-meaning)  meaning (second word-meaning)] 
    (assert-msg (< 0 (count (:attr-meaning meaning))) (str "word no meaning. impossiable, word:" word))
    (assert-msg (< 0 (count (:meaning (rand-nth (:attr-meaning meaning))))) (str "word no meaning. impossiable, word:" word))
    (let [random-meaning (rand-nth (:meaning (rand-nth (:attr-meaning meaning)))) 
          one-meaning (:content random-meaning) 
          relevant-words (get-three-relevant-words one-meaning (:relevant-words random-meaning))]
      (assert-msg (not (in? relevant-words one-meaning)) (str "option and answer is the same.word:" word " all-meaning:" meaning "  random-meaning:" random-meaning " choosed-meaning:" one-meaning " relevant-words:" (vec relevant-words)))
      {:word word, :meaning one-meaning, :relevant-words relevant-words})))

(defn gen-en2zh-qlib
  "generate cet4 en2zh lib."
  [Q-lib file-name]
  (spit file-name (str "#word,meaning,option1,option2,option3" "\n"))
  (doseq [a-question (map gen-en2zh-question Q-lib)] 
      (spit file-name (str (clojure.string/join "," [(:word a-question) (:meaning a-question)  (clojure.string/join "," (:relevant-words a-question))]) "\n"):append true)))

(defn gen-zh2en-question
  "generate a question."
  [word-meaning]
  (let [word (first word-meaning)  meaning (second word-meaning)] 
    (assert-msg (< 0 (count (:attr-meaning meaning))) (str "word no meaning. impossiable, word:" word))
    (assert-msg (< 0 (count (:meaning (rand-nth (:attr-meaning meaning))))) (str "word no meaning. impossiable, word:" word))
    (let [random-meaning (rand-nth (:meaning (rand-nth (:attr-meaning meaning)))) lev-neighbors (:levenshtein-neighbors meaning)]
      {:word word :meaning (:content random-meaning), :relevant-words (get-three-lev-neighbor-words lev-neighbors (:relevant-words random-meaning))})))

(defn gen-zh2en-qlib
  "generate cet4 zh2en lib."
  [Q-lib file-name]
  #_(println "#word,meaning,option1,option2,option3")
  (spit file-name (str "#word,meaning,option1,option2,option3" "\n"))
  (doseq [a-question (map gen-zh2en-question Q-lib)] 
      #_(println (clojure.string/join "," [(:meaning a-question) (:word a-question) (clojure.string/join "," (:relevant-words a-question))]))
      (spit file-name (str (clojure.string/join "," [(:meaning a-question) (:word a-question) (clojure.string/join "," (:relevant-words a-question))]) "\n") :append true)))

(defn -main
  "main function. do lein run"
  [& args]
  (init)
  (doseq [q-type ["cet4" "cet6" "gre" "toefl" "ielts" "bomb" "lucky"]]
    (let [q-file (str "resources/question-bank/" q-type ".txt") 
          zh2en-file (str "out/" q-type "-zh2en.csv") 
          en2zh-file (str "out/" q-type "-en2zh.csv") 
          Q-lib (import-question-bank q-file)]
      (println "processing:" q-type)
      (gen-zh2en-qlib Q-lib zh2en-file)
      (gen-en2zh-qlib Q-lib en2zh-file))))
