(ns poc.boardobjects
  (:require [clojure.core.logic :only    [== >= <= > < = !=] :as l])
  (:use     [clojure.core.logic :exclude [== >= <= > < = !=]])
  (:require [clojure.core.matrix.operators :as m]
            [clojure.core.logic.arithmetic :as ar]
            [clojure.core.logic.fd :as fd]
            [poc.commons :refer :all]))

;; This file defines what the game of Chess is made of. It should be
;; immutable. Of course it can be change but it would also change the
;; game of Chess itself.

(def boardsize
  "The side of the square board. Take care if you want to define non square board, the logic is not ready for it and you'll have to modify it.

  This is immutable."
  8)

;; Pieces are defined within a set as it seems the most semantically
;; correct data structure for chess pieces.
(def initial-state
  "The initial state of the game of chess.

  This is immutable."
  (let [white #{'(king [5 1]) '(queen [4 1])
                '(rook [1 1]) '(rook [8 1])
                '(bishop [3 1]) '(bishop [6 1])
                '(knight [2 1]) '(knight [7 1])
                '(pawn [1 2]) '(pawn [2 2]) '(pawn [3 2]) '(pawn [4 2])
                '(pawn [5 2]) '(pawn [6 2]) '(pawn [7 2]) '(pawn [8 2])}
        symmetry (fn [boardsize set]
                   (map #(let [[a & b] %
                               [c d] (last b)]
                           (list a [c (- (inc boardsize) d)]))
                        set))]
    {:positions {:white white
                 :black (set (symmetry boardsize white))}
     :history []
     :player :white}))

(defn king
  [[x y] [a b] s]
  (fresh [xa yb sum]
    ;; Here we do basic math with triangle.
    (distanceo x a xa)
    (distanceo y b yb)
    (fd/eq (= sum (+ (* xa xa) (* yb yb))))
    (membero sum (range 1 3))))

(defn queen
  [[x y] [a b] s]
  (conde
   [(l/== x a) (l/!= y b)]
   [(l/== y b) (l/!= x a)]
   [(l/!= x a) (l/!= y b)
    (fresh [d1]
      (distanceo a x d1)
      (distanceo b y d1))]))

(defn rook
  [[x y] [a b] s]
  (conde
   [(l/== x a) (l/!= y b)]
   [(l/== y b) (l/!= x a)]))

(defn bishop
  [[x y] [a b] s]
  (conde
   [(l/!= x a) (l/!= y b)
    (fresh [d1]
      (distanceo a x d1)
      (distanceo b y d1))]))

(defn knight
  [[x y] [a b] s]
  (fresh [d1 d2]
    (fd/in d1 d2 (fd/interval 1 2))
    (distanceo a x d1)
    (distanceo b y d2)
    (fd/+ d1 d2 3)))

(defn pawn
  [[x y] [a b] s]
  (fresh [d1]
    (fd/in d1 (fd/interval 1 2))
    (case (get s :player)
      :black (fd/- y b d1)
      :white (fd/- b y d1))
    (l/== x a)))

;; It would be wonderful if we could use some syntactic sugar to make
;; the code easier to understand. I should use macros for this.
;; Maybe I can use core.logic project for this.
(def straight-cross
  '([(== x a) (!= y b)]
    [(== y b) (!= x a)]))

(def diagonal-cross
  '([(!= x a) (!= y b)
     (fresh [d1]
       (distanceo a x d1)
       (distanceo b y d1))]))

(def icons
  "Nested map for pictograms. Access by type and then by colour.

  This is immutable."
  {king   {:white "♔" :black "♚"}
   queen  {:white "♕" :black "♛"}
   rook   {:white "♖" :black "♜"}
   bishop {:white "♗" :black "♝"}
   knight {:white "♘" :black "♞"}
   pawn   {:white "♙" :black "♟"}})


(def blank-board
  "Returns a checkerboard pattern."
  (reduce (fn [a b] (str a (if (even? (+ b (if (even? (int (/ b boardsize))) 1 0)))
                            "■" "□")))
          ""
          (range (* boardsize boardsize))))

(def special-moves
  "Special moves than can be performed. This immutable."
  [:Castling])
