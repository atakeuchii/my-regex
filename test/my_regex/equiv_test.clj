(ns my-regex.equiv-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.string :as str]
            [my-regex.sim :as sim]))

;; パターンのリテラルは a/b/c のみ（メタ文字を素の文字として出さない）
(def ^:private lit (gen/elements [\a \b \c]))

(declare gen-alt)   ; 相互再帰（atom がグループ内で alt を呼ぶ）

;; 文法の各層に対応した生成器: alt > concat > repeat > atom
;; depth はグループのネスト上限。0 で葉のみになり再帰が止まる。
(defn- gen-atom [depth]
  (if (zero? depth)
    (gen/fmap str lit)
    (gen/frequency
     [[6 (gen/fmap str lit)]                       ; リテラル a/b/c
      [1 (gen/return ".")]                          ; 任意1文字
      [2 (gen/fmap #(str "(" % ")")                 ; グループ（中で再帰）
                   (gen-alt (dec depth)))]])))

(defn- gen-repeat [depth]
  (gen/let [a (gen-atom depth)
            q (gen/elements ["" "*" "+" "?"])]      ; 量化子は高々1個
    (str a q)))

(defn- gen-concat [depth]
  (gen/fmap str/join
            (gen/vector (gen-repeat depth) 1 4)))   ; 1〜4個の連接

(defn- gen-alt [depth]
  (gen/fmap #(str/join "|" %)
            (gen/vector (gen-concat depth) 1 3)))   ; 1〜3個の選択

(def ^:private gen-pattern (gen-alt 3))

;; 入力は a/b/c/d。d はパターンのリテラルに無いので、
;; 「. は d にマッチするがリテラルはしない」ケースも踏む。
(def ^:private gen-input
  (gen/fmap str/join
            (gen/vector (gen/elements [\a \b \c \d]) 0 8)))

(defn- java-matches?
  "java.util.regex での完全一致（参照モデル）。"
  [pattern input]
  (some? (re-matches (re-pattern pattern) input)))

;; ── 等価検査本体 ──────────────────────────
(defspec equivalent-to-java-regex 2000
  (prop/for-all [pattern gen-pattern
                 input   gen-input]
                (= (sim/matches-pattern? pattern input)
                   (java-matches? pattern input))))
