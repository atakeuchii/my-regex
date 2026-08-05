(ns my-regex.bench
  (:require [my-regex.sim :as sim]
            [my-regex.nfa :as nfa]))

(defn- time-ms
  "thunk を n 回実行した総ミリ秒を返す（雑な実測）。"
  [n thunk]
  (let [start (System/nanoTime)]
    (dotimes [_ n] (thunk))
    (/ (- (System/nanoTime) start) 1e6)))

(defn linearity
  "同じパターンを、入力長を倍々にして実行時間を測る。
   時間が入力長に比例（倍々）していれば線形の証拠。"
  []
  (let [pat  "(a|b)*c"                     ; NFA を1回だけ構築して使い回す
        nfa  (nfa/from-pattern pat)
        reps 200]
    (println (format "pattern = %s  (reps=%d)" pat reps))
    (println "  len      total(ms)    ms/run    ns/char")
    (doseq [len [1000 2000 4000 8000 16000 32000]]
      (let [input (apply str (repeatedly len #(rand-nth [\a \b])))  ; c を含まない=非マッチ
            total (time-ms reps #(sim/matches? nfa input))
            per   (/ total reps)]
        (println (format "  %-7d  %9.2f  %8.4f  %8.2f"
                         len total per (/ (* per 1e6) len)))))))

(defn -main [& _]
  (linearity))
