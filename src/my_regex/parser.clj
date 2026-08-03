(ns my-regex.parser)

(declare parse-regex)

(def ^:private metachars
  "atom の先頭には来られないメタ文字。"
  #{\( \) \| \* \+ \?})
(def ^:private concat-stop
  "concat の終端になる文字。ここに来たら連接を打ち切る。"
  #{\| \)})

(defn- node-char  [ch]       {:op :char  :ch ch})
(defn- node-dot   []         {:op :dot})
(defn- node-concat [parts]   {:op :concat :parts parts})
(defn- node-alt   [l r]      {:op :alt   :left l :right r})
(defn- node-star  [body]     {:op :star  :body body})
(defn- node-plus  [body]     {:op :plus  :body body})
(defn- node-opt   [body]     {:op :opt   :body body})
(defn- node-group [idx body] {:op :group :idx idx :body body})

;; ── 入力カーソル ────────────────────────────
;; s   : 対象の正規表現文字列
;; pos : 次に読む位置(atom)。各パース関数が共有して進める。
(defn- ->cursor [s]
  {:s s :pos (atom 0) :groups (atom 0)})

(defn- peek-ch
  "次の1文字を返す（位置は進めない）。末尾なら nil。" [cur]
  (let [{:keys [s pos]} cur]
    (when (< @pos (count s))
      (nth s @pos))))

(defn- next-ch!
  "次の1文字を返し、位置を1つ進める。末尾なら nil。"
  [cur]
  (let [{:keys [s pos]} cur
        i @pos]
    (when (< i (count s))
      (swap! pos inc)
      (nth s i))))

(defn- eof? [cur]
  (>= @(:pos cur) (count (:s cur))))

(defn- expect!
  "次の1文字が ch であることを確認して消費する。違えばエラー。"
  [cur ch]
  (let [c (next-ch! cur)]
    (when-not (= c ch)
      (throw (ex-info "expected char"
                      {:expected ch :actual c :pos @(:pos cur)})))
    c))

(defn- parse-atom
  "atom := literal | '.' | '(' regex ')'"
  [cur]
  (let [c (peek-ch cur)]
    (cond
      (nil? c)
      (throw (ex-info "unexpected end of input"
                      {:pos @(:pos cur)}))

      (= c \.)
      (do (next-ch! cur)
          (node-dot))

      (= c \()
      (let [idx (swap! (:groups cur) inc)]
        (next-ch! cur)
        (let [body (parse-regex cur)]
          (expect! cur \))
          (node-group idx body)))

      (metachars c)
      (throw (ex-info "unexpected metacharacter"
                      {:char c :pos @(:pos cur)}))

      :else
      (do (next-ch! cur)
          (node-char c)))))

(defn- parse-repeat
  "repeat := atom ('*' | '+' | '?')?"
  [cur]
  (let [a (parse-atom cur)
        c (peek-ch cur)]
    (case c
      \* (do (next-ch! cur) (node-star a))
      \+ (do (next-ch! cur) (node-plus a))
      \? (do (next-ch! cur) (node-opt a))
      a)))

(defn- parse-concat
  "concat := repeat*"
  [cur]
  (loop [parts []]
    (let [c (peek-ch cur)]
      (if (or (nil? c) (concat-stop c))
        (case (count parts)
          0 {:op :empty}
          1 (first parts)
          (node-concat parts))
        (recur (conj parts (parse-repeat cur)))))))

(defn- parse-alt
  "alt := concat ('|' concat)*"
  [cur]
  (loop [acc (parse-concat cur)]
    (if (= (peek-ch cur) \|)
      (do (next-ch! cur)
          (recur (node-alt acc (parse-concat cur))))
      acc)))

(defn- parse-regex
  "regex := alt"
  [cur]
  (parse-alt cur))

(defn parse [s]
  (let [cur (->cursor s)
        ast (parse-regex cur)]
    (when-not (eof? cur)
      (throw (ex-info "unexpected trailing input"
                      {:pos @(:pos cur) :char (peek-ch cur)})))
    ast))
