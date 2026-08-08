(ns my-regex.capture-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.string :as str]
            [my-regex.vm :as vm]))

;; ── Java 参照モデル（group(0..N) を取り出す）─────────
(defn- java-match-groups [pat s]
  (let [m (re-matcher (re-pattern pat) s)]
    (when (.matches m)
      (mapv #(.group m %) (range (inc (.groupCount m)))))))

(defn- java-find-groups [pat s]
  (let [m (re-matcher (re-pattern pat) s)]
    (when (.find m)
      (mapv #(.group m %) (range (inc (.groupCount m)))))))

;; ── 決め打ち単体テスト（Day5 チェックポイント）──────
(deftest capture-basics
  (is (= ["aaabb" "aaa" "bb"]        (vm/match-groups-pattern "(a+)(b+)" "aaabb")))
  (is (= ["123-4567" "123" "4567"]   (vm/match-groups-pattern "(\\d{3})-(\\d{4})" "123-4567")))
  (is (= ["abc" "abc" "b"]           (vm/match-groups-pattern "(a(b)c)" "abc")))
  (is (= ["a" "a" nil]               (vm/match-groups-pattern "(a)(b)?" "a"))))

(deftest greedy-vs-lazy
  (is (= ["axbxb" "xbx"] (vm/find-groups-pattern "a(.*)b"  "axbxb")))
  (is (= ["axb"   "x"]   (vm/find-groups-pattern "a(.*?)b" "axbxb"))))

(deftest find-basics
  (is (= ["427" "427"] (vm/find-groups-pattern "(\\d+)" "id=427x")))
  (is (nil? (vm/find-groups-pattern "z+" "abc"))))

;; ── 生成器（キャプチャ用の安全な部分集合）─────────
;; 最低1文字を消費する「非空」要素だけを使う
(def ^:private gen-solid-atom
  (gen/one-of [(gen/fmap str (gen/elements [\a \b \c]))
               (gen/return "\\d")
               (gen/elements ["[a-c]" "[0-2]" "[^a]"])]))

(def ^:private gen-solid-quant
  (gen/one-of [(gen/return "")
               (gen/return "+")
               (gen/let [n (gen/choose 1 2), k (gen/choose 0 2)]
                 (str "{" n "," (+ n k) "}"))]))       ; n>=1（空にならない）

(def ^:private gen-solid-piece
  (gen/let [a gen-solid-atom, q gen-solid-quant] (str a q)))

;; グループの中身は非空ピースの連接（→ グループも必ず1文字以上）
(def ^:private gen-group-body
  (gen/fmap str/join (gen/vector gen-solid-piece 1 3)))

(def ^:private gen-piece
  (gen/one-of
   [gen-solid-piece
    (gen/let [body gen-group-body
              q    (gen/elements ["" "*" "+" "?" "*?" "+?" "??"])] ; lazy 含む
      (str "(" body ")" q))]))

(def ^:private gen-cap-pattern
  (gen/fmap str/join (gen/vector gen-piece 1 4)))

(def ^:private gen-cap-input
  (gen/fmap str/join (gen/vector (gen/elements [\a \b \c \d \0 \1 \2]) 0 10)))

;; ── 等価検査 ────────────────────────────────
(defspec match-groups-equiv-java 1500
  (prop/for-all [pat gen-cap-pattern, s gen-cap-input]
                (= (vm/match-groups-pattern pat s) (java-match-groups pat s))))

(defspec find-groups-equiv-java 1500
  (prop/for-all [pat gen-cap-pattern, s gen-cap-input]
                (= (vm/find-groups-pattern pat s) (java-find-groups pat s))))
