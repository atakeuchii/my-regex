(ns my-regex.sim
  (:require [my-regex.nfa :as nfa]))

(defn- eps-closure
  "状態 id 集合 ids から、ε(split)遷移だけで到達できる全状態を集める。
   split は両分岐へ展開し、char/any/match はそこで止める（=フロンティア）。
   訪問済み seen で二重展開を防ぎ、ε ループでも停止する。"
  [states ids]
  (loop [seen #{}
         stack (vec ids)]
    (if-let [id (peek stack)]
      (if (seen id)
        (recur seen (pop stack))
        (let [st (get states id)
              seen' (conj seen id)]
          (if (= :split (:type st))
            (recur seen'
                   (into (pop stack)
                         (remove nil? [(:out st) (:out2 st)])))
            (recur seen' (pop stack)))))
      seen)))

(defn- step
  "状態集合 curr で文字 c を1つ消費し、次の状態集合(ε閉包込み)を返す。"
  [states curr c]
  (let [targets (reduce (fn [acc id]
                          (let [st (get states id)]
                            (case (:type st)
                              :char (if (= c (:ch st))
                                      (conj acc (:out st))
                                      acc)
                              :any (conj acc (:out st))
                              acc)))
                        #{}
                        curr)]
    (eps-closure states targets)))

(defn matches?
  [nfa s]
  (let [{:keys [states start match]} nfa
        init (eps-closure states #{start})
        final (reduce (fn [curr c]
                        (let [nxt (step states curr c)]
                          (if (empty? nxt)
                            (reduced nxt)
                            nxt)))
                      init
                      s)]
    (contains? final match)))

(defn matches-pattern?
  [pattern s]
  (matches? (nfa/from-pattern pattern) s))
