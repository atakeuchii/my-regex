(ns my-regex.sim-test
  (:require [clojure.test :refer [deftest is testing]]
            [my-regex.sim :as sim]))

(defn- m? [pattern s] (sim/matches-pattern? pattern s))
(defn- f? [pattern s] (sim/find-pattern? pattern s))

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

(deftest char-classes
  (is (m? "[abc]" "b"))
  (is (not (m? "[abc]" "d")))
  (is (m? "[a-z]+" "hello"))
  (is (not (m? "[a-z]+" "Hello")))       ; 大文字は範囲外
  (is (m? "[^0-9]+" "abc"))
  (is (not (m? "[^0-9]+" "ab3")))        ; 否定クラスは 3 で脱落
  (is (m? "[\\dA-F]+" "9F0A"))           ; クラス内の短縮
  (is (m? "[a-]" "-"))                    ; リテラルの -
  (is (m? "\\s" " "))
  (is (m? "\\S" "a"))
  (is (not (m? "\\S" " "))))

(deftest bounded-quant
  (is (m? "a{3}" "aaa"))
  (is (not (m? "a{3}" "aa")))
  (is (m? "a{2,}" "aaaaa"))
  (is (not (m? "a{2,}" "a")))
  (is (m? "a{1,3}" "aa"))
  (is (not (m? "a{1,3}" "aaaa")))
  (is (m? "a{0}" ""))
  (is (m? "\\d{3}-\\d{4}" "123-4567"))
  (is (not (m? "\\d{3}-\\d{4}" "12-4567"))))

(deftest anchors-and-find
  (is (f? "abc" "xxabcxx"))
  (is (not (f? "^abc" "xxabc")))         ; 先頭でない
  (is (f? "^abc" "abcxx"))
  (is (f? "abc$" "xxabc"))
  (is (not (f? "abc$" "abcxx")))         ; 末尾でない
  (is (f? "^abc$" "abc"))
  (is (not (f? "^abc$" "abcd")))
  (is (f? "\\d+" "id=42"))
  (is (not (f? "z" "abc")))
  ;; matches?(完全一致) と find?(部分一致) の差
  (is (not (m? "abc" "xxabcxx")))
  (is (f? "abc" "xxabcxx")))
