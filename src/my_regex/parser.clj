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
(defn- node-class [neg? items] {:op :class :neg? neg? :items items})
(defn- node-anchor [kind] {:op :anchor :kind kind})

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

(defn- peek-ch2
  "2文字先を覗く(位置は進めない)"
  [cur]
  (let [{:keys [s pos]} cur
        i (inc @pos)]
    (when (< i (count s))
      (nth s i))))

(defn- digit-char? [c]
  (and c (<= (int \0) (int c) (int \9))))

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

(defn- shorthand-which
  "\\d \\w \\s とその否定を :d :w :s :D :W :S に対応づける。"
  [ch]
  (case ch
    \d :d
    \w :w
    \s :s
    \D :D
    \W :W
    \S :S
    nil))

(defn- parse-escape
  "バックスラッシュの次の1文字を解釈する。
   \\d 等は文字クラスの短縮、それ以外はその文字自体（メタ文字のリテラル化）。"
  [ch]
  (if-let [w (shorthand-which ch)]
    (node-class false [{:kind :shorthand :which w}])
    (node-char ch)))

(defn- read-class-char!
  [cur]
  (let [c (next-ch! cur)]
    (cond
      (nil? c)
      (throw (ex-info "unterminated class" {:pos @(:pos cur)}))
      
      (= c \\)
      (let [e (next-ch! cur)]
        (when (nil? e)
          (throw (ex-info "dangling escape in class" {})))
        (if-let [w (shorthand-which e)]
          {:shorthand w}
          {:char e}))
      
      :else {:char c})))

(defn- parse-class!
  [cur]
  (let [neg? (boolean (when (= (peek-ch cur) \^)
                        (next-ch! cur) 
                        true))]
    (loop [items []]
      (let [c (peek-ch cur)]
        (cond
          (nil? c)
          (throw (ex-info "unterminated class" {:pos @(:pos cur)}))
          
          ;; ']' を消費して閉じる
          (= c \])
          (do (next-ch! cur) 
              (when (empty? items)
                (throw (ex-info "empty class" {:pos @(:pos cur)})))
              (node-class neg? items))
          
          :else
          (let [elem (read-class-char! cur)]
            ;; 単一文字で始まり次が'-'でその次が']'でない -> 範囲
            (if (and (:char elem)
                     (= (peek-ch cur) \-)
                     (not= (peek-ch2 cur) \]))
              (do (next-ch! cur)
                  (let [hi (read-class-char! cur)]
                    (when-not (:char hi)
                      (throw (ex-info "bad range end" {})))
                    (recur (conj items {:kind :range
                                        :lo (:char elem)
                                        :hi (:char hi)}))))
              ;; 範囲でなければ単一要素
              (recur (conj items
                           (if-let [w (:shorthand elem)]
                             {:kind :shorthand :which w}
                             {:kind :char :ch (:char elem)}))))))))))

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

      (= c \\)
      (do (next-ch! cur)
          (let [e (next-ch! cur)]
            (when (nil? e)
              (throw (ex-info "dangling escape" {:pos @(:pos cur)})))
            (parse-escape e)))
      
      (= c \[)
      (do (next-ch! cur)
          (parse-class! cur))
      
      (= c \^)
      (do (next-ch! cur)
          (node-anchor :bol))
      
      (= c \$)
      (do (next-ch! cur)
          (node-anchor :eol))

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

(defn- read-digits!
  [cur]
  (loop [acc []]
    (let [c (peek-ch cur)]
      (if (digit-char? c)
        (do (next-ch! cur)
            (recur (conj acc c)))
        (when (seq acc)
          (parse-long (apply str acc)))))))

(defn- try-parse-braces!
  "'{' の位置で呼ぶ。量化子として読めれば {:min m :max n}（max は nil で∞）を返す。
   読めなければ位置を元に戻して nil（→ '{' は後でリテラル文字になる）。"
  [cur]
  (let [saved @(:pos cur)
        fail! (fn [] (reset! (:pos cur) saved) nil)]
    (next-ch! cur)
    (if-let [lo (read-digits! cur)]
      (case (peek-ch cur)
        \} (do (next-ch! cur) {:min lo :max lo}) ; {n}
        \, (do (next-ch! cur)
               (let [hi (read-digits! cur)]
                 (if (= (peek-ch cur) \})
                   (do (next-ch! cur) {:min lo :max hi})
                   (fail!))))
        (fail!))
      (fail!))))

(defn- expand-repetition
  "atom a を {min,max} 回に脱糖する。max=nil は上限なし。"
  [a mn mx]
  (let [required (repeat mn a)
        optional (if (nil? mx)
                   [(node-star a)]  ; {n,} -> 末尾にa*
                   (repeat (- mx mn) (node-opt a))) ; {n,m} -> a? を(m-n)個
        parts (vec (concat required optional))]
    (case (count parts)
      0 {:op :empty}
      1 (first parts)
      (node-concat parts))))

(defn- parse-repeat
  "repeat := atom ('*' | '+' | '?' | '{' n (',' m?)? '}')?"
  [cur]
  (let [a (parse-atom cur)
        c (peek-ch cur)]
    (case c
      \* (do (next-ch! cur) (node-star a))
      \+ (do (next-ch! cur) (node-plus a))
      \? (do (next-ch! cur) (node-opt a))
      \{ (if-let [{:keys [min max]} (try-parse-braces! cur)]
           (do (when (and max (< max min))
                 (throw (ex-info "bad repetition range {min>max}" {:min min :max max})))
               (expand-repetition a min max))
           a)
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
