(ns my-regex.sim
  (:require [my-regex.nfa :as nfa]))

(defn- ctx-at
  "位置 pos（0..n）でのコンテキスト。先頭か・末尾か。"
  [pos n]
  {:at-start? (zero? pos) :at-end? (= pos n)})

(defn- assert-ok?
  "ゼロ幅アサーションが現在位置で通れるか。"
  [kind ctx]
  (case kind
    :bol (:at-start? ctx)
    :eol (:at-end? ctx)))

;; (defn- eps-closure
;;   "状態 id 集合 ids から、ε(split)遷移だけで到達できる全状態を集める。
;;    split は両分岐へ展開し、char/any/match はそこで止める（=フロンティア）。
;;    訪問済み seen で二重展開を防ぎ、ε ループでも停止する。"
;;   [states ids ctx]
;;   (loop [seen #{}
;;          stack (vec ids)]
;;     (if-let [id (peek stack)]
;;       (if (seen id)
;;         (recur seen (pop stack))
;;         (let [st (get states id)
;;               seen' (conj seen id)]
;;           (case (:type st)
;;             :split (recur seen'
;;                           (into (pop stack)
;;                                 (remove nil? [(:out st) (:out2 st)])))
;;             :assert (if (assert-ok? (:kind st) ctx)
;;                       (recur seen' (conj (pop stack) (:out st)))
;;                       (recur seen' (pop stack)))
;;             (recur seen' (pop stack)))))
;;       seen)))
(defn- eps-closure
  "ids から ε(split)と、条件を満たす :assert を辿って到達できる状態を集める。
   ctx は現在位置（先頭/末尾）。char/any/class/match で止める。"
  [states ids ctx]
  (loop [seen  #{}
         stack (vec ids)]
    (if-let [id (peek stack)]
      (if (seen id)
        (recur seen (pop stack))
        (let [st    (get states id)
              seen' (conj seen id)]
          (case (:type st)
            :split  (recur seen'
                           (into (pop stack)
                                 (remove nil? [(:out st) (:out2 st)])))
            :assert (if (assert-ok? (:kind st) ctx)
                      (recur seen' (conj (pop stack) (:out st)))  ; 通れる
                      (recur seen' (pop stack)))                  ; 通れない→死
            (recur seen' (pop stack)))))                          ; 消費状態は止める
      seen)))

(defn- step
  "curr で文字 c を消費し、位置 ctx での次の状態集合を返す。"
  [states curr c ctx]
  (let [targets (reduce (fn [acc id]
                          (let [st (get states id)]
                            (if (and (#{:char :any :class} (:type st))
                                     (nfa/accepts? st c))
                              (conj acc (:out st))
                              acc)))
                        #{}
                        curr)]
    (eps-closure states targets ctx)))

(defn matches?
  [nfa s]
  (let [{:keys [states start match]} nfa
        n (count s)]
    (loop [i 0
           frontier (eps-closure states #{start} (ctx-at 0 n))]
      (cond
        (empty? frontier) false
        (>= i n) (contains? frontier match)
        :else (recur (inc i)
                     (step states frontier (nth s i)
                           (ctx-at (inc i) n)))))))
(defn find?
  "s のどこかに nfa がマッチする部分文字列があるか（re-find 相当の真偽）。"
  [nfa s]
  (let [{:keys [states start match]} nfa
        n    (count s)
        seed (fn [pos] (eps-closure states #{start} (ctx-at pos n)))]
    (loop [i 0
           frontier (seed 0)]
      (cond
        (contains? frontier match) true       ; 受理に到達＝どこかでマッチ確定
        (>= i n)                   false
        :else
        (let [advanced (step states frontier (nth s i) (ctx-at (inc i) n))]
          ;; 各位置で「ここから開始」も許す＝先頭の暗黙 .*? に相当
          (recur (inc i) (into advanced (seed (inc i)))))))))

(defn matches-pattern?
  [pattern s]
  (matches? (nfa/from-pattern pattern) s))

(defn find-pattern?
  [pattern s] 
  (find? (nfa/from-pattern pattern) s))
