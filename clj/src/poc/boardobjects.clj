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
  "The side of the square board. Take care if you want to define non
  square board, the logic is not ready for it and you'll have to modify
  it.

  This is immutable."
  8)

;; Pieces are defined within a set as it seems the most semantically
;; correct data structure for chess pieces.
(def initial-state
  "The initial state of the game of chess.

  This is immutable."
  (let [white ['(king [5 1]) '(queen [4 1])
               '(rook [1 1]) '(rook [8 1])
               '(bishop [3 1]) '(bishop [6 1])
               '(knight [2 1]) '(knight [7 1])
               '(pawn [1 2]) '(pawn [2 2]) '(pawn [3 2]) '(pawn [4 2])
               '(pawn [5 2]) '(pawn [6 2]) '(pawn [7 2]) '(pawn [8 2])]
        symmetry (fn [boardsize vect]
                   (map #(let [[a & b] %
                               [c d] (last b)]
                           (list a [c (- (inc boardsize) d)]))
                        vect))]
    {:positions {:white white
                 :black (vec (symmetry boardsize white))}
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

(comment (run* [q]
           (fresh [a b]
             (l/== q [a b])
             (membero a (range 1 9))
             (membero b (range 1 9))
             (bishop [6 1] [a b] nil))))

(defn knight
  [[x y] [a b] s]
  (fresh [d1 d2]
    (fd/in d1 d2 (fd/interval 1 2))
    (distanceo a x d1)
    (distanceo b y d2)
    (fd/+ d1 d2 3)))

(defn- positions-from-state
  "Copy from poc.gamelogic. This is redundant and should be resolved."
  ([state]
   (positions-from-state (:positions state) nil))
  ([state colour]
   (let [positions (:positions state)
         reduced (if (nil? colour)
                   (mapcat second state)
                   (get positions colour))]
     (map last reduced)))
  ([position colour turn]
   nil))

(defn- positions-per-color
  "Copy from poc.gamelogic. This is redundant and should be resolved."
  ([state turn]
   (positions-per-color state))
  ([state]
   (reduce #(assoc % %2 (positions-from-state state %2))
           {} (keys (:positions initial-state)))))

;; I still have to implement move en-passant.
(defn pawn
  [[x y] [a b] s]
  (let [first-move? (= 2 (-> (filter #(= [x y] (last %))
                                     (get-in s
                                             [:positions (:player s)]))
                             first count))
        upper-bound (if first-move? 2 1)
        player (:player s)
        other-player (if (= (:player s) :white) :black :white)
        captures (get (positions-per-color s) other-player)]
    (fresh [delta]
      (membero delta (range 1 (inc upper-bound)))
      (conde [(l/== player :white) (fd/+ y delta b)]
             [(l/== player :black) (fd/+ b delta y)])
      (conde [(membero [a b] captures) ;; move is different for capture
              (l/== delta 1)
              (conde [(ar/< 1 a) (fd/+ x 1 a)]
                     [(ar/< 1 x) (fd/+ a 1 x)])]
             [(l/== x a) ;; basic straight move
              succeed]))))

(comment

  (defn test-func
    [testee]
    (distinct
     (run* [q]
       (fresh [a b]
         (l/== q [a b])
         (membero a (range 1 9))
         (membero b (range 1 9))
         (testee [a b])))))

  (test-func (fn [[a b]] (pawn [1 6]
                              [a b]
                              poc.interface/state-after-12-white)))

  (test-func (fn [[a b]] (pawn [1 6] [a b] poc.interface/state-after-12-white)))
  (get (positions-per-color poc.interface/state-after-12-white) :white)

  (distinct
   (run* [q]
     (fresh [a b]
       (l/== q [a b])
       (membero a (range 1 9))
       (membero b (range 1 9))
       (pawn [2 2] [a b]
             initial-state))))

  (def temp-state {:positions {:white #{'(pawn [3 2] [3 4] [3 5] [3 6])
                                        '(pawn [5 2]) '(pawn [7 2])
                                        '(pawn [2 2])}
                               :black #{'(pawn [2 7]) '(pawn [4 7])
                                        '(pawn [6 7] [6 5] [6 4] [6 3])
                                        '(pawn [7 7])}}
                   :history []
                   :player :white})

  (test-func (fn [[a b]] (pawn [3 6] [a b] temp-state)))

  (let [[x y] [5 2]]
    (run* [q]
      (fresh [u v]
        (membero [u v] (get (positions-per-color temp-state) :black))
        (l/== q [u v])
        (conde [(fd/+ x 1 u)]
               [(fd/+ u 1 x)]))))

 (get (positions-per-color temp-state) :black)

  (distinct
   (run* [q]
     (fresh [a b]
       (l/== q [a b])
       (membero a (range 1 9))
       (membero b (range 1 9))
       (pawn [7 2] [a b]
             temp-state))))

  (run* [q]
           (fresh [a b]
             (l/== q [a b])
             (membero a (range 1 9))
             (membero b (range 1 9))
             (pawn [5 2] [a b]
                   {:positions {:white #{'(pawn [7 2]) '(pawn [6 2]) '(knight [2 1])
                                         '(pawn [4 2]) '(queen [4 1]) '(rook [1 1])
                                         '(knight [7 1]) '(bishop [6 1]) '(bishop [3 1])
                                         '(pawn [2 2]) '(rook [8 1]) '(king [5 1])
                                         '(pawn [8 2]) '(pawn [5 2]) '(pawn [3 2])
                                         '(pawn [1 2])}
                                :black #{'(pawn [4 7]) '(pawn [3 7]) '(pawn [1 7])
                                         '(knight [7 8]) '(knight [2 8]) '(pawn [6 7])
                                         '(bishop [3 8]) '(rook [1 8]) '(pawn [2 7])
                                         '(pawn [5 7]) '(rook [8 8]) '(king [5 8])
                                         '(pawn [7 7]) '(bishop [6 8]) '(pawn [8 7])
                                         '(queen [4 8])}}
                    :history []
                    :player :white}))))

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

(def show-position
  "◉")

(def special-moves
  "Special moves than can be performed. This immutable."
  [:Castling])
