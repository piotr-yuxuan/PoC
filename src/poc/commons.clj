(ns poc.commons
  "Common symbols which could be used elsewhere, not specific to this
  very project."
  (:use     [clojure.core.logic :exclude [== >= <= > < = !=]])
  (:require [clojure.core.logic.fd :as fd]))

(defn distanceo
  [a b out]
  "Returns the distance between two numbers"
  (conde
   [(fd/- a b out)]
   [(fd/- b a out)]))

(defn str-mutate
  "Insert c in string s at index i with offset o. Offset 0 puts c and
  shift others, offset 1 replaces the character."
  ([s c i o]
   (str (subs s 0 i) c (subs s (+ o i))))
  ([s c i]
   (str-mutate s c i 1)))

(defn str-n
  [& args]
  (reduce #(str % "\n" %2) "" args))

(defn incoll?
  [v c]
  (when-let [[result] (filter #(= % v) c)] result))

(defn seq-history
  "Format a seq as an history"
  [seq]
  (let [f1 #(map vec %)
        f2 #(partition 2 %)]
    (-> seq f2 f1 f2 f1 vec)))

(defn contains-some?
  "Returns a vector with the keys from keys the map contains."
  [map keys]
  (reduce #(if (contains? map %2) (conj (vec %) %2) %) nil keys))

(defn mapify
  "Take a nested vector ns and returns a map. Take care of the structure
  of nv before to use this function."
  [nv]
  (reduce #(let [k (first %2)
                 v (second %2)]
             (assoc % k (if (coll? v)(first v) v)))
          {}
          nv))
