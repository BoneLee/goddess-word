(ns goddess-word.core-test
  (:require [clojure.test :refer :all]
            [goddess-word.core :refer :all]))

(deftest levenshtein-distance-test
  (testing "levenshtein-distance function test."
    (is (= (levenshtein-distance "" "") 0))
    (is (= (levenshtein-distance "a" "") 1))
    (is (= (levenshtein-distance "" "a") 1))
    (is (= (levenshtein-distance "" "123456") 6))
    (is (= (levenshtein-distance "123456" "") 6))
    (is (= (levenshtein-distance "bone" "cone") 1))
    (is (= (levenshtein-distance "kitten" "sitting") 3))
    (is (= (levenshtein-distance "xyx" "xyyyx") 2))
    (is (= (levenshtein-distance "avada kedavra" "abracadabra") 7))
    (is (= (levenshtein-distance "ttttattttctg" "tcaaccctaccat") 10))
    (is (= (levenshtein-distance "gaattctaatctc" "caaacaaaaaattt") 9))
    (is (= (levenshtein-distance "bone" "on") 2))))


(deftest make-word-graph-test
  (testing "make-word-graph function test."
    (let [correct-graph {"game" {"fame" 1, "gain" 2, "aim" 3}, "fame" {}, "gain" {}, "aim" {}}]
    (is (= (make-word-graph (set ["game" "fame" "gain" "aim"]) "game") correct-graph))))
  (testing "make-word-graph function test 2."
    (let [correct-graph {"game" {"fame" 1, "gain" 2, "aim" 3}, "fame" {"same" 1}, "same" {}, "gain" {}, "aim" {}}]
    (is (= (make-word-graph  (sorted-set "game" "fame" "gain" "aim" "same") "game") correct-graph))))
  (testing "make-word-graph function test 4."
    (let [correct-graph {"aim" {}, "same" {}, "acm" {"aim" 1}, "gate" {}, "frame" {"home" 3, "gay" 4, "gain" 4}, "game" {"frame" 2, "fame" 1, "acm" 3}, "fame" {"same" 1, "gate" 2}, "home" {}, "gay" {}, "gain" {}}]
    (is (= (make-word-graph  (sorted-set "gate" "gay" "frame" "home" "acm" "game" "fame" "gain" "aim" "same") "game") correct-graph))))
  (testing "make-word-graph function test 3."
    (let [correct-graph {"game" {"fame" 1}, "fame" {}}]
    (is (= (make-word-graph (set ["game" "fame"]) "game") correct-graph)))))

(deftest search-word-test
  (testing "search-word function test."
    (let [word-graph (make-word-graph (set ["game" "fame" "gain" "aim" "same" "gate" "gay" "frame" "acm" "home"]) "game")]
      (is (= (search-word word-graph "game" "game" 0) {"game" 0}))
      (is (= (search-word word-graph "game" "game" 1) {"game" 0, "fame" 1, "same" 1, "gate" 1}))
      (is (= (search-word word-graph "game" "game" 2) {"game" 0, "fame" 1, "same" 1, "gate" 1, "gain" 2, "gay" 2, "frame" 2, "home" 2}))
      (is (= (search-word word-graph "game" "game" 3) {"game" 0, "fame" 1, "same" 1, "gate" 1, "gain" 2, "gay" 2, "frame" 2, "home" 2, "aim" 3, "acm" 3}))
      (is (= (search-word word-graph "game" "acm" 0) {"acm" 0}))
      (is (= (search-word word-graph "game" "acm" 1) {"aim" 1, "acm" 0}))
      (is (= (search-word word-graph "game" "acm" 2) {"aim" 1, "acm" 0}))
      (is (= (search-word word-graph "game" "acm" 3) {"aim" 1, "acm" 0,"game" 3, "gay" 3, "fame" 3, "same" 3, "gain" 3, "home" 3, "gate" 3}))
      (is (= (search-word word-graph "game" "acm" 4) {"aim" 1, "acm" 0,"game" 3, "gay" 3, "fame" 3, "same" 3, "gain" 3, "gate" 3, "home" 3, "frame" 4}))
      (is (= (search-word word-graph "game" "same" 1) {"game" 1, "same" 0, "fame" 1})))))


(deftest import-en-dict-test
  (testing "import-en-dict function test."
    (is (= (import-en-dict "test/goddess_word/en-words-test.txt") {"dying" [["a." "垂死的,临终的"]], "dye" [["vt.&vi." "染,染色"], ["n." "染料,染色"]], "duty" [["n." "责任,义务" "职责,职务" "税,关税"]]}))))

(deftest import-zh-dict-test
  (testing "import-zh-dict function test."
    (is (= (import-zh-dict "test/goddess_word/zh-words-test.txt") {"岳" #{"(surname)" "mountain" "wife's father"}, "钥匙" #{"key"}, "啊" #{"(interj. for surprise)" "ah" "to express doubt or to question" "oh (interjection)" "to show realization" "(interj.)" "an interjection" "to stress"}}))))

(deftest import-irlab_dict
  (testing "import-irlab-dict function test."
    (is (= (import-irlab-dict "test/goddess_word/irlab-words-test.txt") [{:tag "Ab03A04", :attr "=", :meaning ["少年人" "少年" "苗子" "苗" "年幼" "未成年" "未成年人"]} {:tag "Ab03A05", :attr "=", :meaning ["芝兰" "龙驹" "千里驹"]} {:tag "Ab03A06", :attr "#", :meaning ["待业青年" "务工青年"]} {:tag "Ab03A07", :attr "#", :meaning ["男孩子" "少男"]} {:tag "Ab04A01", :attr "=", :meaning ["婴儿" "婴孩" "婴" "新生儿" "产儿" "赤子" "乳儿" "毛毛" "小儿" "早产儿" "婴幼儿"]} {:tag "Ab04A02", :attr "=", :meaning ["宝宝" "宝贝" "乖乖" "囡囡" "小鬼" "宝贝疙瘩" "宝贝儿" "小宝宝"]} {:tag "Ab04A03", :attr "#", :meaning ["女婴" "男婴"]} {:tag "Ab04A04", :attr "@", :meaning ["弃婴"]} {:tag "Ab04A05", :attr "@", :meaning ["圣婴"]}]))))


(deftest irlab-dict-map-test
  (testing "irlab-dict-map function test."
    (is (= (import-irlab-dict-map (import-irlab-dict "test/goddess_word/irlab-words-test.txt")) {"囡囡" 5, "宝贝" 5, "龙驹" 1, "乖乖" 5, "毛毛" 4, "宝宝" 5, "早产儿" 4, "女婴" 6, "少年" 0, "少男" 3, "务工青年" 2, "年幼" 0, "婴儿" 4, "乳儿" 4, "未成年人" 0, "赤子" 4, "小鬼" 5, "未成年" 0, "小宝宝" 5, "千里驹" 1, "宝贝疙瘩" 5, "新生儿" 4, "小儿" 4, "圣婴" 8, "弃婴" 7, "芝兰" 1, "婴" 4, "婴孩" 4, "少年人" 0, "苗" 0, "待业青年" 2, "婴幼儿" 4, "产儿" 4, "苗子" 0, "男婴" 6, "男孩子" 3, "宝贝儿" 5}))))

(deftest zmap-test
  (testing "zmap function test."
    (is (= (zmap [1 2 3] 123) {1 123, 2 123, 3 123}))
    (is (= (zmap [1 2 3] "bone") {1 "bone", 2 "bone", 3 "bone"}))))


(deftest choose-matched-words-test
  (testing "choose-matched-words function test."
    (is (= (choose-matched-words {:tag "Ab03A04", :attr "=", :meaning ["少年人" "少年" "苗子" "苗" "年幼" "未成年" "未成年人"]} true) []))
    (is (= (choose-matched-words {:tag "Ab03A06", :attr "#", :meaning ["待业青年" "务工青年"]} false) ["待业青年" "务工青年"]))
    (is (= (choose-matched-words {:tag "Ab04A04", :attr "@", :meaning ["弃婴"]} false) ["弃婴"]))
    (is (= (choose-matched-words {:tag "Ab03A05", :attr "=", :meaning ["芝兰" "龙驹" "千里驹"]} true) []))))


(deftest generate-jacent-words-test
  (testing "generate-jacent-words function test."
   (binding [levenshtein-distance (fn mock-levenshtein-distance [a b] 3)]
    (is (= ((generate-jacent-words #{}) {})))
    (is (= ((generate-jacent-words #{"kaka"}) {})))
    (is (= ((generate-jacent-words #{"bone" "kaka"}) {})))
    (is (= ((generate-jacent-words #{"bone" "jack" "kaka"}) {}))))
   (binding [levenshtein-distance (fn mock-levenshtein-distance [a b] 1)]
    (is (= ((generate-jacent-words #{}) {})))
    (is (= ((generate-jacent-words #{"kaka"}) {})))
    (is (= ((generate-jacent-words #{"bone" "kaka"}) {"kaka" {"bone" 1}, "bone" {"kaka" 1}})))
    (is (= ((generate-jacent-words #{"bone" "jack" "kaka"}) {"jack" {"kaka" 1, "bone" 1}, "kaka" {"jack" 1, "bone" 1}, "bone" {"jack" 1, "kaka" 1}}))))
   (binding [levenshtein-distance (fn mock-levenshtein-distance [a b] 1)]
    (is (= ((generate-jacent-words #{}) {})))
    (is (= ((generate-jacent-words #{"kaka"}) {})))
    (is (= ((generate-jacent-words #{"bone" "kaka"}) {"kaka" {"bone" 2}, "bone" {"kaka" 2}})))
    (is (= ((generate-jacent-words #{"bone" "jack" "kaka"}) {"jack" {"kaka" 2, "bone" 2}, "kaka" {"jack" 2, "bone" 2}, "bone" {"jack" 2, "kaka" 2}}))))))


(deftest join-to-a-string-vector-test
  (testing "join-to-a-string-vector function test."
    (is (= (join-to-a-string-vector  "") (list "")))
    (is (= (join-to-a-string-vector  "bone") (list "bone")))
    (is (= (join-to-a-string-vector  ["bone" "lee"]) (list "bone" "lee")))
    (is (= (join-to-a-string-vector  ["bone" ["xixi"] "lee"]) (list "bone" "xixi" "lee")))
    (is (= (join-to-a-string-vector  [["bone"] ["xixi"] ["lee"]]) (list "bone" "xixi" "lee")))
    (is (= (join-to-a-string-vector  [["bone"] "xixi" ["lee"]]) (list "bone" "xixi" "lee")))
    (is (= (join-to-a-string-vector  [["bone" "kaka" ["how" ["are" "you"]]] "xixi" ["lee"]]) (list "bone" "kaka" "how"  "are"  "you" "xixi" "lee")))))


;;;---------------------------------integration test---------------------------------
#_ (deftest search-en-word-by-distance-test
  (testing "search-en-word-by-distance function test."
    (is (= (search-en-word-by-distance "bone" 0) {"bone" 0}))
    (is (= (search-en-word-by-distance "bone" 1) {"bore" 1, "bond" 1, "cone" 1, "bone" 0, "one" 1, "bole" 1, "zone" 1, "tone" 1, "bane" 1, "bony" 1, "none" 1, "lone" 1, "hone" 1}))
    (is (= (search-en-word-by-distance "bone" 2) {"bile" 2, "money" 2, "hole" 2, "boat" 2, "boob" 2, "bore" 1, "robe" 2, "dope" 2, "gong" 2, "wane" 2, "once" 2, "cope" 2, "both" 2, "bike" 2, "June" 2, "bee" 2, "babe" 2, "con" 2, "vane" 2, "body" 2, "bomb" 2, "begone" 2, "bosh" 2, "bowl" 2, "be" 2, "don" 2, "mode" 2, "bond" 1, "cone" 1, "dome" 2, "bored" 2, "prone" 2, "lode" 2, "bone" 0, "come" 2, "dole" 2, "stone" 2, "sane" 2, "noone" 2, "bold" 2, "lobe" 2, "one" 1, "bole" 1, "coke" 2, "bin" 2, "ion" 2, "dune" 2, "bounce" 2, "bide" 2, "pane" 2, "wine" 2, "book" 2, "yore" 2, "toe" 2, "bow" 2, "blonde" 2, "tonne" 2, "vote" 2, "vine" 2, "ooze" 2, "box" 2, "boom" 2, "bung" 2, "boy" 2, "lane" 2, "boon" 2, "bonny" 2, "zone" 1, "bout" 2, "booze" 2, "code" 2, "boll" 2, "boss" 2, "rote" 2, "phone" 2, "bye" 2, "ban" 2, "rose" 2, "sore" 2, "nonce" 2, "ode" 2, "bunk" 2, "pine" 2, "brine" 2, "boil" 2, "yoke" 2, "pose" 2, "base" 2, "move" 2, "bowel" 2, "pore" 2, "nine" 2, "rope" 2, "tone" 1, "son" 2, "bare" 2, "boot" 2, "love" 2, "note" 2, "mine" 2, "ton" 2, "atone" 2, "tome" 2, "mote" 2, "nose" 2, "Pope" 2, "some" 2, "boa" 2, "line" 2, "sole" 2, "song" 2, "bob" 2, "band" 2, "bolt" 2, "cane" 2, "lose" 2, "gene" 2, "more" 2, "pond" 2, "role" 2, "doze" 2, "bygone" 2, "bane" 1, "lore" 2, "nope" 2, "mope" 2, "cony" 2, "bite" 2, "bale" 2, "bang" 2, "lope" 2, "bony" 1, "drone" 2, "none" 1, "pole" 2, "boxer" 2, "tune" 2, "bend" 2, "hose" 2, "bake" 2, "poke" 2, "broke" 2, "blue" 2, "dove" 2, "doe" 2, "bronze" 2, "fine" 2, "bog" 2, "lone" 1, "ozone" 2, "gore" 2, "blond" 2, "alone" 2, "bun" 2, "mole" 2, "honey" 2, "dote" 2, "bonnet" 2, "fore" 2, "foe" 2, "dine" 2, "hope" 2, "bank" 2, "long" 2, "dose" 2, "bind" 2, "ebony" 2, "owe" 2, "above" 2, "hoe" 2, "hone" 1, "boar" 2, "bonus" 2, "fond" 2, "home" 2, "joke" 2, "bloke" 2, "on" 2, "monk" 2, "core" 2}))))

(deftest my-test-sample  
   (testing "Arithmetic"
       (testing "with positive integers"
             (is (= 4 (+ 2 2)))
             (is (= 7 (+ 3 4))))
       (testing "with negative integers"
             (is (= -4 (+ -2 -2)))
             (is (= -1 (+ 3 -4))))))


