(ns my-regex.bench
  (:require [my-regex.sim :as sim]
            [my-regex.nfa :as nfa]
            [my-regex.vm :as vm]
            [my-regex.compile :as c]
            [my-regex.backtrack :as bt]))

(defn- time-ms
  "thunk を n 回実行した総ミリ秒を返す（雑な実測）。"
  [n thunk]
  (let [start (System/nanoTime)]
    (dotimes [_ n] (thunk))
    (/ (- (System/nanoTime) start) 1e6)))

(defn- time-once
  "thunk を1回だけ実行したミリ秒。"
  [thunk]
  (let [t0 (System/nanoTime)] (thunk) (/ (- (System/nanoTime) t0) 1e6)))

(defn- time-avg
  "thunk を reps 回実行した平均ミリ秒。"
  [reps thunk]
  (let [t0 (System/nanoTime)]
    (dotimes [_ reps] (thunk))
    (/ (- (System/nanoTime) t0) 1e6 reps)))

(defn redos-compare
  "同じ ReDoS パターンを3実装で。n を増やすと backtrack だけ爆発する。"
  []
  (let [pat "(a+)+$"
        nfa-c (nfa/from-pattern pat)
        prog (c/from-pattern pat)
        input (fn [n] (str (apply str (repeat n \a)) "!"))]
    (println (format "pattern = %s   input = \"a\"*n + \"!\"（必ず非マッチ）" pat))
    (println "   n    backtrack(ms)      sim(ms)      vm(ms)")
    (doseq [n [5 10 15 18 20 22 24 26]]
      (let [s (input n)
            bt-ms (time-once #(bt/matches? pat s))
            sim-ms (time-avg 500 #(sim/matches? nfa-c s))    ; 平均
            vm-ms  (time-avg 500 #(vm/matches? prog s))]
        (println (format "  %-3d  %14.3f  %11.5f  %10.5f" n bt-ms sim-ms vm-ms))))
    (println "注: n を +2 するごとに backtrack は約4倍。26 で数秒かかることも。")))

(defn automaton-scale
  "オートマトンだけを巨大 n で。backtrack が走れない領域でも線形。"
  []
  (let [pat "(a+)+$"
        nfa-c (nfa/from-pattern pat)
        prog (c/from-pattern pat)
        input (fn [n] (str (apply str (repeat n \a)) "!"))]
    (println (format "\npattern = %s  （オートマトンのみ・大きい n）" pat))
    (println "     n        sim(ms)      vm(ms)")
    (doseq [n [1000 10000 100000]]
      (let [s      (input n)
            sim-ms (time-avg 20 #(sim/matches? nfa-c s))
            vm-ms  (time-avg 20 #(vm/matches? prog s))]
        (println (format "  %-8d  %11.4f  %10.4f" n sim-ms vm-ms))))))

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
  (linearity)
  (redos-compare)
  (automaton-scale))
