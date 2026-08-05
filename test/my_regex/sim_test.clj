(ns my-regex.sim-test
  (:require [clojure.test :refer [deftest is testing]]
            [my-regex.sim :as sim]))

(defn- m? [pattern s] (sim/matches-pattern? pattern s))

(deftest literals-and-dot
  (is (m? "a" "a"))
  (is (not (m? "a" "b")))
  (is (m? "abc" "abc"))
  (is (not (m? "abc" "ab")))       ; 完全一致: 足りない
  (is (not (m? "abc" "abcd")))     ; 完全一致: 余る
  (is (m? "a.c" "abc"))
  (is (m? "a.c" "axc"))
  (is (not (m? "a.c" "ac"))))      ; . は1文字必要

(deftest alternation
  (is (m? "a|b" "a"))
  (is (m? "a|b" "b"))
  (is (not (m? "a|b" "c")))
  (is (m? "cat|dog" "dog"))
  (is (not (m? "cat|dog" "cog"))))

(deftest repetition
  (testing "star: 0回以上"
    (is (m? "a*" ""))
    (is (m? "a*" "a"))
    (is (m? "a*" "aaaa"))
    (is (not (m? "a*" "aab"))))
  (testing "plus: 1回以上"
    (is (not (m? "a+" "")))
    (is (m? "a+" "a"))
    (is (m? "a+" "aaa")))
  (testing "opt: 0/1回"
    (is (m? "a?" ""))
    (is (m? "a?" "a"))
    (is (not (m? "a?" "aa")))))

(deftest groups-and-combos
  (is (m? "(ab)+" "abab"))
  (is (not (m? "(ab)+" "aba")))
  (is (m? "a(b|c)*d" "ad"))        ; (b|c) を0回
  (is (m? "a(b|c)*d" "abccbd"))
  (is (not (m? "a(b|c)*d" "abce")))
  (is (m? "(a|b)*" ""))
  (is (m? "(a|b)*" "abba")))

(deftest empty-and-edge
  (is (m? "" ""))
  (is (not (m? "" "a")))
  (is (m? "a|" "a"))               ; 空選択
  (is (m? "a|" ""))
  (is (m? "a()" "a"))              ; 空グループ
  (is (m? "(a*)*" "aaa")))         ; ε ループでも停止
