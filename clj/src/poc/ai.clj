;; BEWARE THIS IS CURRENTLY UNDER HEAVY DEVELOPMENT

(ns poc.ai
  (:require [poc.commons :refer :all]
            [poc.boardobjects :refer :all]
            [poc.gamelogic :refer :all]))

(def cost-table
  {king 1000000
   queen 95
   rook 45
   bishop 30
   knight 30})

(def power-table
  {queen 95
   rook 55
   bishop 45
   knight 30
   king 20})

(def promotion-table
  {pawn 30
   knight 25
   bishop 15
   rook 15})

(defn promotion-cost
  [piece]
  (get promotion-table piece 5))

(defn piece-cost
  [piece]
  (get cost-table piece 10))

(defn earning-from-state-position
  [state position]
  (if (= (:player state)
         (colour-from-state-position state position))
    0 ;; won't occur very often by design
    (piece-cost (type-from-state-position state position))))

(piece-cost pawn)

(defn dest-weight
  [state [x y]]
  (let [[x y] [x y]
        values (if (> 4 (/ (count (:history state)) 2))
                 {:3 20 :4 25}
                 {:3 20 :4 20 :5 30 :6 40 :7 50 :8 60})
        y (if (= :white (:player state)) y (- (inc boardsize) 1))]
    (get values (-> y str keyword) 10)))

(defn move-weight
  [state [x y]]
  (get power-table (eval (type-from-state-position state [x y])) 10))

(comment
  (dest-weight {:history (range 5) :player :white} [2 3])
  (dest-weight (assoc initial-state :player :white) [1 3])
  (move-weight initial-state [2 1]))

;; The main data structure is a map with weights as keys and vectors of possible moves as values.

;; For the two first moves we look up in an opening book filled with
;; some stuff from here: https://www.chess.com/openings/. This seems
;; fair: everybody does like this.

;; First, we get all our possible moves and weight them with destination.
;; We add promotion.
;; If castling is possible, we evaluate it and add it.
;; If we are about to be eaten, we consider eating the opponent, protecting, escape or sacrifying.
;; Then, if we can eat, we think about if we'll be protected, how much we can get with this capture.
;; Finally, we take the best move if we have some ties to break or can neither eat or being captured, we rely on destination weights.

;; This relies on some fixed tables. Of course any strategies with such
;; tool is completely superseeded by any modern chess machine. However,
;; it can be fun to optimize those tables with some genetic algorithm:
;; just randomly alter tables (not to get stuck in local optimum) then
;; set up a contest and find the most efficient. It's just a proof of
;; concept.

(rand-nth (let [themap {1 [0 1 2] 2 [1 2 3] 3 [3 4 5] 4 [4 5 6] 7 [7 8 9]}]
            (get themap (apply max (keys themap)))))

(defn add-map-vec
  [m k v & vs]
  (assoc m k (into (get m k) (into [v] vs))))

(into nil [1])

(into [1 2] nil)

(conj [3] 4 nil)

(add-map-vec {1 #{1 2}} 1 3 4 5 6)

(defn possible-attacks
  [state]
  (let [current (:player initial-state)
        opponent (if (= :white current) :black :white)]
    (map (fn [%] {:move %
                 :fought (possible-positions-to-destination (assoc state :player opponent)
                                                            (second %))
                 :earning (type-from-state-position state (second %))})
         (all-possible-moves state))))

(possible-attacks poc.interface/ttt)

(all-possible-moves ttt)
(earning-from-state-position poc.interface/ttt [3 5])

(defn next-move
  [state]
  (let [moves (reduce #(let [[[x y] [a b]] %2
                             dest-w (dest-weight initial-state [a b])
                             move-w (move-weight initial-state [x y])
                             t-weight (reduce + [dest-w move-w])]
                         (add-map-vec % t-weight %2))
                      {}
                      (all-possible-moves initial-state))]
    (identity)))
