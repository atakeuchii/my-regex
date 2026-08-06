(ns my-regex.equiv-test
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.string :as str]
            [my-regex.sim :as sim]))

(def ^:private lit (gen/elements [\a \b \c]))

(declare gen-alt)

;; [...] / [^...]: 単一文字か範囲を 1〜3 個
(def ^:private gen-class
  (let [item (gen/one-of
              [(gen/fmap str (gen/elements [\a \b \c \d \0 \1 \2]))
               (gen/elements ["a-c" "0-2" "x-z" "A-C"])])]
    (gen/let [neg?  gen/boolean
              items (gen/vector item 1 3)]
      (str "[" (when neg? "^") (str/join items) "]"))))

(defn- gen-atom [depth]
  (gen/frequency
   (cond-> [[6 (gen/fmap str lit)]
            [1 (gen/return ".")]
            [2 (gen/return "\\d")]
            [2 (gen/return "\\w")]
            [3 gen-class]]
     (pos? depth) (conj [3 (gen/fmap #(str "(" % ")") (gen-alt (dec depth)))]))))

;; 量化子: なし / * + ? / {n} {n,} {n,m}
(def ^:private gen-quant
  (gen/one-of
   [(gen/return "")
    (gen/elements ["*" "+" "?"])
    (gen/let [n (gen/choose 0 3)]            (str "{" n "}"))
    (gen/let [n (gen/choose 0 2)]            (str "{" n ",}"))
    (gen/let [n (gen/choose 0 2)
              k (gen/choose 0 2)]            (str "{" n "," (+ n k) "}"))]))

(defn- gen-repeat [depth]
  (gen/let [a (gen-atom depth), q gen-quant] (str a q)))

(defn- gen-concat [depth]
  (gen/fmap str/join (gen/vector (gen-repeat depth) 1 4)))

(defn- gen-alt [depth]
  (gen/fmap #(str/join "|" %) (gen/vector (gen-concat depth) 1 3)))

;; パターン全体に、任意で ^ を前・$ を後ろに付ける
(def ^:private gen-pattern
  (gen/let [bol? gen/boolean
            body (gen-alt 2)
            eol? gen/boolean]
    (str (when bol? "^") body (when eol? "$"))))

;; 入力: パターンに無い d/数字/_/空白も混ぜ、. や否定クラスの分岐を踏む
(def ^:private gen-input
  (gen/fmap str/join
            (gen/vector (gen/elements [\a \b \c \d \0 \1 \2 \_ \space]) 0 8)))

(defn- java-matches? [pat s] (some? (re-matches (re-pattern pat) s)))
(defn- java-find?    [pat s] (some? (re-find    (re-pattern pat) s)))

;; 完全一致: 自作 matches? ≡ java re-matches
(defspec matches-equiv-java 2000
  (prop/for-all [pat gen-pattern, s gen-input]
                (= (sim/matches-pattern? pat s) (java-matches? pat s))))

;; 部分一致: 自作 find? ≡ java re-find
(defspec find-equiv-java 2000
  (prop/for-all [pat gen-pattern, s gen-input]
                (= (sim/find-pattern? pat s) (java-find? pat s))))
