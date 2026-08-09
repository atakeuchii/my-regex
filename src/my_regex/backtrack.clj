(ns my-regex.backtrack
  (:require [my-regex.parser :as parser]
            [my-regex.nfa :as nfa]))

;; match-node: node を s の位置 pos からマッチ試行する。
;;   k = 継続。「pos' まで消費した」とき残りを続ける関数 (fn [pos'] -> pos' or nil)。
;;   成功なら最終 pos、失敗なら nil。
(defn- match-node [node s pos k]
  (case (:op node)
    :empty (k pos)
    :char (when (and (< pos (count s)) (= (nth s pos) (:ch node)))
            (k (inc pos)))
    :dot (when (< pos (count s)) (k (inc pos)))
    :class (when (and (< pos (count s))
                      (nfa/accepts? {:type :class
                                     :spec {:neg? (:neg? node) :items (:items node)}}
                                    (nth s pos)))
             (k (inc pos)))
    :concat (let [parts (:parts node)]
              (letfn [(go [ps p]
                        (if (empty? ps)
                          (k p)
                          (match-node (first ps) s p
                                      (fn [p'] (go (rest ps) p')))))]
                (go parts pos)))
    :alt (or (match-node (:left node) s pos k)
             (match-node (:right node) s pos k))
    :group (match-node (:body node) s pos k)
    :star (if (:lazy? node)
            (or (k pos)
                (match-node (:body node) s pos
                            (fn [p'] (when (> p' pos) (match-node node s p' k)))))
            (or (match-node (:body node) s pos
                            (fn [p'] (when (> p' pos) (match-node node s p' k))))
                (k pos)))
    :plus (match-node (:body node) s pos
                      (fn [p'] (match-node {:op :star :body (:body node) :lazy? (:lazy? node)}
                                           s p' k)))
    :opt (if (:lazy? node)
           (or (k pos) (match-node (:body node) s pos k))
           (or (match-node (:body node) s pos k) (k pos)))
    :anchor (case (:kind node)
              :bol (when (zero? pos) (k pos))
              :eol (when (= pos (count s)) (k pos)))))

(defn matches? [pattern s]
  (let [ast (parser/parse pattern)]
    (boolean (match-node ast s 0
                         (fn [pos] (when (= pos (count s)) pos))))))

(defn find? [pattern s]
  (let [ast (parser/parse pattern)]
    (boolean
     (some (fn [start]
             (match-node ast s start (fn [pos] pos)))
           (range (inc (count s)))))))
