(ns my-regex.nfa
  (:require [my-regex.parser :as parser]))

;; ── 状態の表現 ──────────────────────────────
;; {:type :char :ch c :out id}    文字 c を消費 → out
;; {:type :any :out id}  任意1文字を消費 → out（. 用）
;; {:type :split :out id :out2 id} ε分岐（文字を消費せず両方へ）
;; {:type :match}                 受理
;; :out / :out2 が nil = まだ繋ぎ先未定の「穴(dangling)」。

(defn- new-builder []
  (atom {:states {} :next 0}))

(defn- add-state!
  [b state]
  (let [id (:next @b)]
    (swap! b #(-> % (assoc-in [:states id] state) (update :next inc)))
    id))

(defn- patch!
  [b outs target]
  (doseq [[id slot] outs]
    (swap! b assoc-in [:states id slot] target)))

(defn- compile-node
  [b node]
  (case (:op node)
    :char
    (let [id (add-state! b {:type :char :ch (:ch node) :out nil})]
      {:start id :outs [[id :out]]})

    :dot
    (let [id (add-state! b {:type :any :out nil})]
      {:start id :outs [[id :out]]})

    :empty
    (let [id (add-state! b {:type :split :out nil :out2 nil})]
      {:start id :outs [[id :out] [id :out2]]})
    
    :concat
    (let [[hd & tl] (:parts node)
          f0 (compile-node b hd)]
      (reduce (fn [f1 nd]
                (let [f2 (compile-node b nd)]
                  (patch! b (:outs f1) (:start f2))
                  {:start (:start f1) :outs (:outs f2)}))
              f0
              tl))
    
    :alt
    (let [f1 (compile-node b (:left node))
          f2 (compile-node b (:right node))
          id (add-state! b {:type :split
                            :out (:start f1)
                            :out2 (:start f2)})]
      {:start id :outs (into (:outs f1) (:outs f2))})
    
    :star
    (let [f (compile-node b (:body node))
          id (add-state! b {:type :split :out (:start f) :out2 nil})]
      (patch! b (:outs f) id)
      {:start id :outs [[id :out2]]})
    
    :plus
    (let [f (compile-node b (:body node))
          id (add-state! b {:type :split :out (:start f) :out2 nil})]
      (patch! b (:outs f) id)
      {:start (:start f) :outs [[id :out2]]})
    
    :opt
    (let [f (compile-node b (:body node))
          id (add-state! b {:type :split :out (:start f) :out2 nil})]
      {:start id :outs (conj (:outs f) [id :out2])})
    
    :group
    (compile-node b (:body node))

    :anchor
    (let [id (add-state! b {:type :assert :kind (:kind node) :out nil})]
      {:start id :outs [[id :out]]})

    :class
    (let [id (add-state! b {:type :class
                            :spec {:neg? (:neg? node) :items (:items node)}
                            :out nil})]
      {:start id :outs [[id :out]]})

    (throw (ex-info "compile-node: op not implemented yet"
                    {:op (:op node)}))))

(defn build
  "AST を NFA に変換する。{:states {} :start id :match id} を返す。"
  [ast]
  (let [b (new-builder)
        frag (compile-node b ast)
        m (add-state! b {:type :match})]
    (patch! b (:outs frag) m)
    {:states (:states @b) :start (:start frag) :match m}))

(defn from-pattern
  [s]
  (build (parser/parse s)))

(defn- shorthand-accepts?
  [which ch]
  (let [c (int ch)
        digit? (<= 48 c 57)
        word? (or digit? (<= 65 c 90) (<= 97 c 122) (= ch \_))
        space? (contains? #{\space \tab \newline \return \formfeed} ch)]
    (case which
      :d digit?
      :w word?
      :s space?
      :D (not digit?)
      :W (not word?)
      :S (not space?))))

(defn- item-accepts?
  [item ch]
  (case (:kind item)
    :char (= ch (:ch item))
    :range (<= (int (:lo item)) (int ch) (int (:hi item)))
    :shorthand (shorthand-accepts? (:which item) ch)))

(defn- class-accepts?
  [spec ch]
  (let [hit? (boolean (some #(item-accepts? % ch) (:items spec)))]
    (if (:neg? spec)
      (not hit?)
      hit?)))

(defn accepts?
  [st ch]
  (case (:type st)
    :char (= ch (:ch st))
    :any true
    :class (class-accepts? (:spec st) ch)
    false))

(defn- class-label
  [spec]
  (str (when (:neg? spec) "^")
       (apply str (map (fn [it]
                         (case (:kind it)
                           :char (str (:ch it))
                           :range (str (:lo it) "-" (:hi it))
                           :shorthand (str "\\" (name (:which it)))))
                       (:items spec)))))

(defn ->dot
  "NFA を Graphviz DOT 文字列に変換する。デバッグ用。"
  [{:keys [states start match]}]
  (let [sb (StringBuilder.)
        add #(doto sb (.append %) (.append "\n"))]
    (add "digraph nfa {")
    (add "  rankdir=LR;")
    (add "  node [shape=circle];")
    (add "  start [shape=point];")
    (add (format "  start -> %d;" start))
    (doseq [[id st] (sort-by key states)]
      (if (= :match (:type st))
        (add (format "  %d [shape=doublecircle];" id))
        (add (format "  %d;" id))))

    (doseq [[id st] (sort-by key states)]
      (case (:type st)
        :char  (add (format "  %d -> %d [label=\"%s\"];" id (:out st) (:ch st)))
        :any   (add (format "  %d -> %d [label=\".\"];" id (:out st)))
        :split (do (when (:out st)
                     (add (format "  %d -> %d [label=\"ε\",style=dashed];"
                                  id (:out st))))
                   (when (:out2 st)
                     (add (format "  %d -> %d [label=\"ε\",style=dashed];"
                                  id (:out2 st)))))
        :class (add (format "  %d -> %d [label=\"[%s]\"];"
                            id (:out st) (class-label (:spec st))))
        :assert (add (format "  %d -> %d [label=\"%s\",style=dashed];"
                             id (:out st)
                             (case (:kind st) 
                               :bol "^"
                               :eol "$")))
        :match nil))
    (add "}")
    (str sb)))

(defn spit-dot
  "NFA を DOT ファイルに書き出す。`dot -Tpng f.dot -o f.png` で画像化。"
  [nfa path]
  (spit path (->dot nfa)))
