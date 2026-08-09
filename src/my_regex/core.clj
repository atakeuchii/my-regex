(ns my-regex.core
  (:refer-clojure :exclude [compile re-matches re-find])
  (:require [my-regex.compile :as c]
            [my-regex.vm :as vm]))

(defn compile
  [pattern]
  (c/from-pattern pattern))

(defn- ->prog [p]
  (if (vector? p)
    p
    (compile p)))

(defn matches? [p s]
  (vm/matches? (->prog p) s))

(defn find? [p s]
  (vm/find? (->prog p) s))

(defn re-matches [p s]
  (vm/match-groups (->prog p) s))

(defn re-find [p s]
  (vm/find-groups (->prog p) s))

