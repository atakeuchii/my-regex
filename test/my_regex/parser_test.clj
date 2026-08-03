(ns my-regex.parser-test
  (:require [clojure.test :refer [deftest is testing]]
            [my-regex.parser :as p]))

(defn- sexp
  "AST を読みやすい S 式に畳む（テスト用）。
   例: (p/parse \"ab*\") => (cat \\a (star \\b))"
  [node]
  (case (:op node)
    :char  (:ch node)
    :dot   'dot
    :empty 'empty
    :star  (list 'star (sexp (:body node)))
    :plus  (list 'plus (sexp (:body node)))
    :opt   (list 'opt  (sexp (:body node)))
    :group (list 'grp (:idx node) (sexp (:body node)))
    :alt   (list 'alt (sexp (:left node)) (sexp (:right node)))
    :concat (cons 'cat (map sexp (:parts node)))))

(defn p [s] (sexp (p/parse s)))

;; ── 基本ノード ──────────────────────────────
(deftest atoms
  (is (= \a (p "a")))
  (is (= 'dot (p ".")))
  (is (= '(star \a) (p "a*")))
  (is (= '(plus \a) (p "a+")))
  (is (= '(opt \a)  (p "a?"))))

;; ── 連接 ────────────────────────────────────
(deftest concatenation
  (is (= '(cat \a \b) (p "ab")))
  (is (= '(cat \a \b \c) (p "abc"))))

;; ── 選択 ────────────────────────────────────
(deftest alternation
  (is (= '(alt \a \b) (p "a|b")))
  (is (= '(alt (alt \a \b) \c) (p "a|b|c"))))

;; ── 優先順位（今日の肝）─────────────────────
(deftest precedence
  (is (= '(cat \a (star \b)) (p "ab*")))
  (is (= '(alt (cat \a \b) \c) (p "ab|c")))
  (is (= '(star (grp 1 (cat \a \b))) (p "(ab)*"))))

;; ── グループと採番 ──────────────────────────
(deftest groups
  (is (= '(grp 1 \a) (p "(a)")))
  (is (= '(grp 1 (grp 2 \a)) (p "((a))")))
  (is (= '(cat (grp 1 \a) (grp 2 \b)) (p "(a)(b)")))
  (is (= '(cat \a (star (grp 1 (alt \b \c))) \d)
         (p "a(b|c)*d"))))

;; ── 空（ε）─────────────────────────────────
(deftest empties
  (is (= 'empty (p "")))
  (is (= '(alt \a empty) (p "a|")))
  (is (= '(alt empty \a) (p "|a")))
  (is (= '(alt empty empty) (p "|")))
  (is (= '(cat \a (grp 1 empty)) (p "a()"))))

;; ── 不正入力は例外 ──────────────────────────
(deftest errors
  (testing "余分な閉じ括弧"
    (is (thrown? clojure.lang.ExceptionInfo (p/parse "a)")))
    (is (thrown? clojure.lang.ExceptionInfo (p/parse ")"))))
  (testing "閉じられない括弧"
    (is (thrown? clojure.lang.ExceptionInfo (p/parse "(a"))))
  (testing "先頭に来られないメタ文字"
    (is (thrown? clojure.lang.ExceptionInfo (p/parse "*a"))))
  (testing "量化子の重ね掛けは今日は不正"
    (is (thrown? clojure.lang.ExceptionInfo (p/parse "a**")))))
