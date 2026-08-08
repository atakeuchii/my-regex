(ns my-regex.compile
  (:refer-clojure :exclude [compile])
  (:require [my-regex.parser :as parser]))

;; ── 命令（pc = ベクタの添字）─────────────────
;; {:op :char  :ch c}      入力1文字が c なら消費して pc+1、違えば死
;; {:op :any}              任意1文字を消費して pc+1
;; {:op :class :spec s}    s が入力文字を受理すれば消費して pc+1
;; {:op :split :x a :y b}  スレッドを2本に。まず a、次に b（順序=greedy/lazy）
;; {:op :jmp   :x a}       無条件に a へ
;; {:op :save  :slot n}    現在の入力位置を slot n に記録（消費しない）
;; {:op :assert :kind k}   ゼロ幅アサーション（:bol / :eol）
;; {:op :match}            受理
;;
;; slot 割り当て: 0/1 = 全体マッチ、グループ k = 2k / 2k+1

(defn- new-prog [] (atom []))

(defn- emit!
  [prog instr]
  (let [pc (count @prog)]
    (swap! prog conj instr)
    pc))

(defn- here
  [prog]
  (count @prog))

(defn- set-target!
  [prog pc key target]
  (swap! prog assoc-in [pc key] target))

(defn- compile-node 
  [prog node]
  (case (:op node)
    :char (emit! prog {:op :char :ch (:ch node)})
    :dot (emit! prog {:op :any})
    :class (emit! prog {:op :class :spec {:neg? (:neg? node)
                                          :items (:items node)}})
    :empty nil
    :concat (doseq [p (:parts node)]
              (compile-node prog p))
    :group (let [k (:idx node)]
             (emit! prog {:op :save :slot (* 2 k)})
             (compile-node prog (:body node))
             (emit! prog {:op :save :slot (inc (* 2 k))}))
    :alt (let [s (emit! prog {:op :split :x nil :y nil})
               l1 (here prog)]
           (compile-node prog (:left node))
           (let [j (emit! prog {:op :jmp :x nil})
                 l2 (here prog)]
             (compile-node prog (:right node))
             (let [l3 (here prog)]
               (set-target! prog s :x l1)
               (set-target! prog s :y l2)
               (set-target! prog j :x l3))))
    :star (let [s (emit! prog {:op :split :x nil :y nil})
                l2 (here prog)]
            (compile-node prog (:body node))
            (emit! prog {:op :jmp :x s})
            (let [l3 (here prog)]
              (if (:lazy? node)
                (do (set-target! prog s :x l3)
                    (set-target! prog s :y l2))
                (do (set-target! prog s :x l2)
                    (set-target! prog s :y l3)))))
    :plus (let [l1 (here prog)]
            (compile-node prog (:body node))
            (let [s (emit! prog {:op :split :x nil :y nil})
                  l3 (here prog)]
              (if (:lazy? node)
                (do (set-target! prog s :x l3)
                    (set-target! prog s :y l1))
                (do (set-target! prog s :x l1)
                    (set-target! prog s :y l3)))))
    :opt (let [s (emit! prog {:op :split :x nil :y nil})
               l1 (here prog)]
           (compile-node prog (:body node))
           (let [l2 (here prog)]
             (if (:lazy? node)
               (do (set-target! prog s :x l2) (set-target! prog s :y l1))
               (do (set-target! prog s :x l1) (set-target! prog s :y l2)))))
    :anchor (emit! prog {:op :assert :kind (:kind node)})
    (throw (ex-info "compile: op not implemented yet" {:op (:op node)}))))

(defn compile
  [ast]
  (let [prog (new-prog)]
    (emit! prog {:op :save :slot 0})
    (compile-node prog ast)
    (emit! prog {:op :save :slot 1})
    (emit! prog {:op :match})
    @prog))

(defn from-pattern [s] (compile (parser/parse s)))

(defn disasm
  "プログラムを pc 付きで表示する。"
  [prog]
  (doseq [[pc i] (map-indexed vector prog)]
    (println (format "%3d  %s" pc
                     (case (:op i)
                       :char   (str "char " (:ch i))
                       :any    "any ."
                       :class  (str "class " (pr-str (:spec i)))
                       :split  (str "split " (:x i) ", " (:y i))
                       :jmp    (str "jmp " (:x i))
                       :save   (str "save " (:slot i))
                       :assert (str "assert " (:kind i))
                       :match  "match")))))
