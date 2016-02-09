(ns poc.gamelogic
  (:require [clojure.core.logic :only    [== >= <= > < = !=] :as l])
  (:use     [clojure.core.logic :exclude [== >= <= > < = !=]])
  (:require [clojure.core.matrix.operators :as m]
            [clojure.core.logic.arithmetic :as ar]
            [clojure.core.logic.fd :as fd]
            [poc.boardobjects :refer :all]
            [poc.commons :refer :all]))

(defn state->type-colour-position-o
  "Constraints such as colour is the colour of the piece at [x y] in the
  given state. At least one of the following symbol should refer to a
  logical variable: type, colour, position."
  [state type colour position]
  (fresh [tail item temp]
    (membero [colour tail] (vec (:positions state)))
    (membero item tail)
    (firsto item type)
    (lasto position item)))

;; Two versions are offered: add a constraint over an output lvar or get
;; the result from traditionnal programming.
(defn positions-from-state-o
  ([position state]
   (positions-from-state state position))
  ([position state colour]
   (state->type-colour-position-o state (lvar) colour position)))

(defn positions-from-state
  ([state]
   (positions-from-state (:positions state) nil))
  ([state colour]
   (let [positions (:positions state)
         reduced (if (nil? colour)
                   (mapcat second state)
                   (get positions colour))]
     (filter #(-> % nil? not) (map last reduced))))
  ([position colour turn]
   nil))

(defn positions-per-color
  ([state turn]
   (positions-per-color state))
  ([state]
   (reduce #(assoc % %2 (positions-from-state state %2))
           {} (keys (:positions initial-state)))))

(defn type-from-state-position-o
  [type state position]
  (state->type-colour-position-o state type (lvar) position))

(defn type-from-state-position
  [state [x y]]
  (first (run* [q]
           (fresh [a u v]
             (membero a (map #(vector (first %) (last %))
                             (get-in state [:positions (:player state)])))
             (conso q [[u v]] a)
             (l/== x u)
             (l/== y v)))))

(defn colour-from-state-position-o
  [colour state [x y]]
  (state->type-colour-position-o state (lvar) colour [x y]))

(defn colour-from-state-position
  [state [x y]]
  (first (run* [q]
           (fresh [a b c d]
             (membero a (vec (positions-per-color state)))
             (l/== a [b c])
             (l/== d [x y])
             (membero d c)
             (l/== q b)))))

;; Here are the pieces. They are plain functions.
king queen rook bishop knight pawn

(comment
  ;; Algorithme de détection d'obstacles :
  ;; On détermine les positions sans obstacles
  ;; Pour chacune de ces positions on définit une position itermédiaire
  ;; Si cette position intermédiaire peut se trouver sur une case prise
  ;; alors la position principale est invalidée.

  "Sketch to show how we can find positions beyond obstacles. We have
  only one dimension to make it easy to get."
  (let [position 5
        obstacles '(3 6)]
    (distinct (run* [result]
                (membero result (range 1 (inc boardsize)))
                (fresh [u]
                  (conde [(fd/eq (< position result)
                                 (< position u)
                                 (< u result))]
                         [(fd/eq (< result position)
                                 (< result u)
                                 (< u position))])
                  (membero u obstacles))))))

(defn unreachable-positions
  [obstacles [x y] [a b]]
  (let [var-bounding (fn [x? a? u?]
                       (conde [(ar/< x? a?) (ar/< x? u?) (ar/< u? a?)]
                              [(ar/< a? x?) (ar/< a? u?) (ar/< u? x?)]))]
    (fresh [u v]
      (membero u (range 1 (inc boardsize))) (membero v (range 1 (inc boardsize)))
      (conde [(l/== y b)
              (var-bounding x a u)
              (membero [u b] obstacles)]
             [(l/== x a)
              (var-bounding y b v)
              (membero [a v] obstacles)]
             [(and* [(l/!= x a) (l/!= y b)
                     (fresh [_] (distanceo a u _) (distanceo b v _))])
              (var-bounding x a u) (var-bounding y b v)
              (membero [u v] obstacles)]))))

;; Useful as function symbols seem not to be resolved properly in core.logic.
;; Huh, perhaps I should use multimethod to get rid of this twist.
(def function-table
  "core.logic seems to have trouble in handling function symbols."
  (reduce #(assoc % %2 (eval %2))
                   {}
                   '(king queen rook bishop knight pawn)))

;; (defn moves-from-state-position
;;   "If the result is not what you expect, think about checking the
;;   parameters: maybe you tried to move a piece which is not yours or
;;   perhaps it's not your turn."
;;   [state [x y]]
;;   (when type
;;     (distinct
;;      (let [state->o state->type-colour-position-o
;;            obstacles (run* [position]
;;                        (state->o state (lvar) (lvar) position))
;;            friends  (run* [position]
;;                            (state->o state (lvar) (:player state) position)
;;                            (l/!= nil position))]
;;        (run* [?]
;;          (fresh [a b type colour]
;;            ;; Stay inside the board for the sake of fun
;;            (membero a (range 1 (inc boardsize)))
;;            (membero b (range 1 (inc boardsize)))
;;            ;; Get all available moves for that kind of piece
;;            (state->o state type colour [x y])
;;            (project [type]
;;                     (apply (get function-table type) [[x y] [a b] state]))
;;            ;; Unify q with working logic variables
;;            (l/== ? [a b])
;;            ;; Avoid friends
;;            (nafc membero ? friends)
;;            ;; Don't go beyond obstacles
;;            (nafc unreachable-positions obstacles [x y] [a b])))))))

(defn moves-from-state-position
  "If the result is not what you expect, think about checking the
  parameters: maybe you tried to move a piece which is not yours or
  perhaps it's not your turn."
  [state [x y]]
  (let [type (get function-table (type-from-state-position state [x y]))
        obstacles (positions-from-state state)
        current-player (colour-from-state-position state [x y])
        friends (positions-from-state state (:player state))]
    (when type
      (distinct
       (run* [?]
         (fresh [a b]
           ;; Stay inside the board for the sake of fun
           (membero a (range 1 (inc boardsize)))
           (membero b (range 1 (inc boardsize)))
           ;; Get all available moves for that kind of piece
           (type [x y] [a b] state)
           ;; Unify q with working logic variables
           (l/== ? [a b])
           ;; Avoid friends
           (nafc membero ? friends)
           ;; Don't go beyond obstacles
           (nafc unreachable-positions obstacles [x y] [a b])
           ;; Move only your own pieces.
           (l/== (:player state) current-player)))))))

(defmulti castling-from-side-and-player
  "Returns {:rook [[] []] :king [[] []]}. Multimethod is more
  interesting than if forms in this very case as it ensures precise
  parameters or raises an exception otherwise."
  (fn [side player]
    [side player]))

(defmethod castling-from-side-and-player [:QueenSide :white]
  [side player]
  {:rook [[1 1] [4 1]] :king [[5 1] [3 1]]})

(defmethod castling-from-side-and-player [:KingSide :white]
  [side player]
  {:rook [[8 1] [6 1]] :king [[5 1] [7 1]]})

(defmethod castling-from-side-and-player [:QueenSide :black]
  [side player]
  {:rook [[1 8] [4 8]] :king [[5 8] [3 8]]})

(defmethod castling-from-side-and-player [:KingSide :black]
  [side player]
  {:rook [[8 8] [6 8]] :king [[5 8] [7 8]]})

(defn- change-piece-position
  [positions lastp newp]
  (map #(if (= (last %) lastp)
          (concat % [newp])
          %)
       positions))

(defmulti move-piece
  (fn [state arrow] (type arrow)))

(comment (move-piece {:positions {:white #{'(rook [1 1]) '(rook [8 1]) '(king [5 1])}
                                  :black #{'(rook [1 8]) '(rook [8 8]) '(king [5 8])}}
                      :history []
                      :player :white}
                     {:Castling :KingSide}))

(defmethod move-piece (type {})
  [state arrow]
  (let [current-player (:player state)
        old-player-positions (get (:positions state) current-player)
        new-player-positions (reduce #(change-piece-position % (first %2) (second %2))
                                        old-player-positions
                                        (map second (castling-from-side-and-player (:Castling arrow)
                                                                                   current-player)))]
    {:player (if (= :white current-player) :black :white)
     :history (conj (:history state) arrow)
     :positions (assoc (:positions state) current-player new-player-positions)}))

(defmethod move-piece (type [])
  [state arrow]
  (let [[[x1 y1] [x2 y2]] arrow]
    (when-let [legal-move (incoll? [x2 y2]
                                   (moves-from-state-position state [x1 y1]))]
      (let [current-player (:player state)
            other-player (if (= :white current-player) :black :white)
            other-player-positions (change-piece-position (get-in state
                                                                     [:positions other-player])
                                                             [x2 y2] nil)
            new-player-positions (change-piece-position (get-in state
                                                                  [:positions current-player])
                                                          [x1 y1] [x2 y2])]
        {:player (if (= :white current-player) :black :white)
         :history (conj (:history state) [[x1 y1] legal-move])
         :positions (assoc (:positions state)
                      current-player new-player-positions
                      other-player other-player-positions)}))))

;; FIX this is way too long and must be improved
(defn possible-positions-to-destination
  "Can help to parse PGN (see namespace interface). It considers the
  current player as a basic optimisation."
  [state [x y]]
  (run* [q]
    (membero q (positions-from-state state (:player state)))
    (project [q]
             (membero [x y] (moves-from-state-position state q)))))

(defn move-pieces-from-state-through-history
  "Reduction of an history on a given state."
  [state history]
  (reduce move-piece
          state
          history))

;; Not that impressive, but once we can do this we can extend to full history. Just mock version, should be extended to any distance.
(defn one-move-far-state-from-state?
  [depart-state arrival-state]
  (-> (run 1 [constraint-history]
        (fresh [x1? y1?]
          (membero x1? (range boardsize)) (membero y1? (range boardsize))
          (project [x1? y1?]
                   (fresh [x2? y2?]
                     (membero [x2? y2?] (moves-from-state-position depart-state [x1? y1?]))
                     (project
                      [x2? y2?]
                      (l/== arrival-state
                            (move-pieces-from-state-through-history depart-state
                                                                    [[[x1? y1?] [x2? y2?]]])))
                     (l/== constraint-history [[[x1? y1?] [x2? y2?]]])))))
      empty? not))

(defn binary-numbers
  "Returns all possible binary numbers under a certain length.

  This function is useless as is for this mock but it explains how to
  deal with an arbitrary, non hard-coded number of logic variables."
  [length]
  (let [closure (fn [length dimension]
                  ;; This symbol now contains our fresh logic variables.
                  (let [bindings (vec (repeatedly length lvar))
                        goal #(membero % (range dimension))]
                    (run* [q]
                      ;; This apply a goal to every variables.
                      (everyg goal bindings)
                      (l/== q bindings))))]
    ;; Just change 2 for any other dimension if you want.
    (closure length 2)))

(defn logic-history
  "Returns a structured vector of logic variables which suits history structure."
  [n]
  (-> (repeatedly (* 2 2 n) lvar) seq-history))

;; Returns
(declare history-from-state-to-state)
