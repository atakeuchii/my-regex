(ns my-regex.nfa-test
  (:require [clojure.test :refer [deftest is]]
            [my-regex.nfa :as nfa]))

(defn- out-targets
  "状態が指す遷移先 id のリスト。"
  [st]
  (case (:type st)
    :match []
    (:char :any :class) [(:out st)]
    :split [(:out st) (:out2 st)]
    :assert [(:out st)]))

(defn- well-formed?
  "穴(nil)が残っておらず、全遷移先が実在する状態を指すか。"
  [{:keys [states start match]}]
  (and (contains? states start)
       (= :match (:type (get states match)))
       (every? (fn [[_ st]]
                 (every? #(and (some? %) (contains? states %))
                         (out-targets st)))
               states)))

(defn- reachable
  "start から辿れる状態 id の集合。"
  [{:keys [states start]}]
  (loop [seen #{} stack [start]]
    (if-let [id (peek stack)]
      (if (seen id)
        (recur seen (pop stack))
        (recur (conj seen id)
               (into (pop stack) (out-targets (get states id)))))
      seen)))

(defn- count-type [nfa t]
  (count (filter #(= t (:type %)) (vals (:states nfa)))))

(def ^:private patterns
  ["a" "ab" "abc" "a|b" "a|b|c" "a*" "a+" "a?"
   "(a)" "(ab)" "(a|b)*" "a(b|c)*d" "" "a|" "a()"])

(deftest all-well-formed
  (doseq [p patterns]
    (is (well-formed? (nfa/from-pattern p)) (str "pattern: " p))))

(deftest match-is-reachable
  (doseq [p patterns]
    (let [n (nfa/from-pattern p)]
      (is (contains? (reachable n) (:match n)) (str "pattern: " p)))))

(deftest char-and-any-counts
  ;; リテラル文字の数だけ :char 状態ができる
  (is (= 3 (count-type (nfa/from-pattern "abc") :char)))
  (is (= 2 (count-type (nfa/from-pattern "a|b") :char)))
  ;; . は :any になる
  (is (= 1 (count-type (nfa/from-pattern "a.c") :any)))
  (is (= 0 (count-type (nfa/from-pattern "a*") :any)))
  ;; グループは状態を増やさない（中身と同型）
  (is (= (count-type (nfa/from-pattern "(ab)") :char)
         (count-type (nfa/from-pattern "ab")   :char))))
