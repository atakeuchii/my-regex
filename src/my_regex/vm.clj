(ns my-regex.vm
  (:require [my-regex.compile :as c]
            [my-regex.nfa :as nfa]))

(defn- consumes?
  [i c]
  (and c (nfa/accepts? (assoc i :type (:op i)) c)))

(defn- assert-pass?
  [kind sp n]
  (case kind
    :bol (zero? sp)
    :eol (= sp n)))

(defn- add-thread
  [prog n tlist seen pc saves sp]
  (if (contains? seen pc)
    [tlist seen]
    (let [seen (conj seen pc)
          i (nth prog pc)]
      (case (:op i)
        :jmp (add-thread prog n tlist seen (:x i) saves sp)
        :save (add-thread prog n tlist seen (inc pc) (assoc saves (:slot i) sp) sp)
        :split (let [[tlist seen]
                     (add-thread prog n tlist seen (:x i) saves sp)]
                 (add-thread prog n tlist seen (:y i) saves sp))
        :assert (if (assert-pass? (:kind i) sp n)
                  (add-thread prog n tlist seen (inc pc) saves sp)
                  [tlist seen]) 
        [(conj tlist {:pc pc :saves saves}) seen]))))

(defn run-match
  [prog s]
  (let [n (count s)]
    (loop [sp 0
           clist (first (add-thread prog n [] #{} 0 {} 0))]
      (if (empty? clist)
        nil
        (let [match-saves (when (= sp n)
                            (some (fn [{:keys [pc saves]}]
                                    (when (= :match (:op (nth prog pc))) saves))
                                  clist))]
          (cond
            match-saves match-saves
            (= sp n) nil
            :else (let [c (nth s sp)
                        [nlist _] (reduce (fn [[nlist nseen] {:keys [pc saves]}]
                                            (let [i (nth prog pc)]
                                              (if (and (not= :match (:op i)) (consumes? i c))
                                                (add-thread prog n nlist nseen (inc pc) saves (inc sp))
                                                [nlist nseen])))
                                          [[] #{}]
                                          clist)]
                    (recur (inc sp) nlist))))))))

(defn matches? [prog s] (some? (run-match prog s)))
(defn matches-pattern? [pattern s] (matches? (c/from-pattern pattern) s))

(defn- num-groups
  "プログラムから最大グループ番号を求める。全体マッチ(slot0/1)は除く。"
  [prog]
  (let [max-slot (transduce (comp (filter #(= :save (:op %))) (map :slot))
                            max 1 prog)]
    (quot (dec max-slot) 2)))

(defn- group-strings
  "saves から [全体, g1, g2, ...] を作る。参加しなかったグループは nil。"
  [prog s saves]
  (let [ng (num-groups prog)]
    (mapv (fn [k]
            (let [a (get saves (* 2 k))
                  b (get saves (inc (* 2 k)))]
              (when (and a b) (subs s a b))))
          (range (inc ng)))))

(defn run-find
  "prog を s のどこかにマッチさせ、最左優先の saves を返す。無ければ nil。"
  [prog s]
  (let [n (count s)]
    (loop [sp      0
           clist   []
           cseen   #{}
           matched nil]
      ;; 未マッチなら「この位置から開始」スレッドを最低優先で足す（=先頭の暗黙 .*?）
      (let [[clist cseen] (if matched
                            [clist cseen]
                            (add-thread prog n clist cseen 0 {} sp))]
        (if (empty? clist)
          matched
          (let [c (when (< sp n) (nth s sp))
                [m* nlist nseen]
                (reduce
                 (fn [[m nlist nseen] {:keys [pc saves]}]
                   (cond
                     m [m nlist nseen]                          ; match 後の低優先は cut
                     (= :match (:op (nth prog pc))) [saves nlist nseen]
                     (and c (consumes? (nth prog pc) c))
                     (let [[nl ns] (add-thread prog n nlist nseen
                                               (inc pc) saves (inc sp))]
                       [m nl ns])
                     :else [m nlist nseen]))
                 [nil [] #{}]
                 clist)
                matched' (or m* matched)]              ; 新 match は旧を上書き
            (if (>= sp n)
              matched'
              (recur (inc sp) nlist nseen matched'))))))))

(defn find? [prog s] (some? (run-find prog s)))
(defn find-pattern? [pattern s] (find? (c/from-pattern pattern) s))

(defn match-groups
  "完全一致し [全体マッチ g1 g2 ...] を返す。マッチしなければ nil。"
  [prog s]
  (when-let [saves (run-match prog s)]
    (group-strings prog s saves)))

(defn match-groups-pattern [pattern s]
  (match-groups (c/from-pattern pattern) s))

(defn find-groups [prog s]
  (when-let [saves (run-find prog s)]
    (group-strings prog s saves)))
(defn find-groups-pattern [pattern s]
  (find-groups (c/from-pattern pattern) s))
