(ns poc.gamelogic
  (:require [clojure.core.logic :only    [== >= <= > < = !=] :as l])
  (:use     [clojure.core.logic :exclude [== >= <= > < = !=]])
  (:require [clojure.core.matrix.operators :as m]
            [clojure.core.logic.arithmetic :as ar]
            [clojure.core.logic.fd :as fd]
            [poc.boardobjects :refer :all]
            [poc.commons :refer :all]))

;; Here we just consider easy basic general moves. We define a move as a
;; change of position: staying at the same place is not a move. This
;; implies we will need an heuristic to determine whether we have to
;; play or don't use that turn.

;; Here are the pieces. They are plain functions.
king queen rook bishop knight pawn

;; The following function returns the current state of the board. It's
;; given the afore defined map.
(defn positions-from-state
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

(comment
  (count (positions-from-state initial-state :white)))

;; TODO here I should use a bag map to make for it nicely suits. And
;; it's fun! Moreover it would get positions-per-color to be merge into
;; positions-from-bag so it would avoid a function to be defined just to
;; be used once.
;;   "Returns positions from different arguments: 1) All occupied
;;  positions; 2) Positions of the given colour; 3) Positions of the
;;  colour at some previous state CAUTION TODO: the turn selector is
;;  obviously buggy and does nothing interesting. It has to be fixed
;;  before you use it.
;; Obviously the previous state is not implemented yet.

;; Retrieve type and colour from state

(defn positions-per-color
  ([state turn]
   (positions-per-color state))
  ([state]
   (reduce #(assoc % %2 (positions-from-state state %2))
           {} (keys (:positions initial-state)))))

(defn type-from-state-position
  [state [x y]]
  (first (run* [q]
           (fresh [a u v]
             (membero a (map #(vector (first %) (last %))
                             (get-in state [:positions (:player state)])))
             (conso q [[u v]] a)
             (l/== x u)
             (l/== y v)))))

(defn colour-from-state-position
  [state [x y]]
  (first (run* [q]
           (fresh [a b c d]
             (membero a (vec (positions-per-color state)))
             (l/== a [b c])
             (l/== d [x y])
             (membero d c)
             (l/== q b)))))

;; Let's use the broadly-used convention: whites are always at the
;; bottom of the board.

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
                  (membero u (range 1 (inc boardsize)))
                  (conde [(fd/eq (< position result)
                                 (< position u)
                                 (< u result))]
                         [(fd/eq (< result position)
                                 (< result u)
                                 (< u position))])
                  (membero u obstacles))))))

;; This should be rewritten as [state [x y] [a b]] and should avoid
;; using so basic implementation.
(defn unreachable-positions
  "This constraint applies to pieces which move straight. When applied,
  the piece should be not able to move to a position if there is another
  piece straight on its way. Designed to suit any pieces."
  [state [x y]]
  (let [obstacles (positions-from-state state)
        var-bounding (fn [x a u]
                       (conde [(ar/< x a) (ar/< x u) (ar/< u a)]
                              [(ar/< a x) (ar/< a u) (ar/< u x)]))
        mapo (fnc [f l] (map f l))]
    (distinct
     (run* [result]
       (fresh [a b u v]
         ;; can't run alone yet -- why?
         ;;(mapo #(membero % (range 1 (inc boardsize))) [a b u v])
         (membero a (range 1 (inc boardsize))) (membero b (range 1 (inc boardsize)))
         (membero u (range 1 (inc boardsize))) (membero v (range 1 (inc boardsize)))
         (conde [(l/== y b)
                 (var-bounding x a u)
                 (membero [u b] obstacles)]
                [(l/== x a)
                 (var-bounding y b v)
                 (membero [a v] obstacles)]
                [(and* [(l/!= x a) (l/!= y b)
                        (fresh [_] (distanceo a x _) (distanceo b y _))])
                 (var-bounding x a u) (var-bounding y b v)
                 (membero [u v] obstacles)])
         (l/== result [a b]))))))

;; These configurations lead to something very weird.
(comment
  (let [test-state {:history [[[1 1] [2 2]]]
                    :player :white
                    :positions {:white #{['queen [1 1] [2 2]]}
                                :black #{['queen [3 4]]}}}]
    (unreachable-positions test-state [2 2]))
  (let [test-state {:history [[[1 1] [2 2]]]
                    :player :white
                    :positions {:white #{['queen [1 1] [2 2]]}
                                :black #{['queen [3 2]]}}}]
    (unreachable-positions test-state [2 2])
    ;;(positions-from-state test-state)
    ))

;; Useful as function symbols seem not to be resolved properly in core.logic.
(def function-table
  "core.logic seems to have trouble in handling function symbols."
  (reduce #(assoc % %2 (eval %2))
                   {}
                   '(king queen rook bishop knight pawn)))

(defn moves-from-state-position
  "If the result is not what you expect, think about checking the
  parameters: maybe you tried to move a piece which is not yours or
  perhaps it's not your turn."
  [state [x y]]
  (when-let [type (get function-table (type-from-state-position state [x y]))]
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
         (nafc membero ? (positions-from-state state (:player state)))
         ;; Don't go beyond obstacles
         (nafc membero ? (unreachable-positions state [x y]))
         ;; Move only your own pieces.
         (l/== (:player state) (colour-from-state-position state [x y])))))))

;; This function can now be called:
(comment
  (moves-from-state-position initial-state [2 2]))

;; Before we used constraint programming but from here we turn into
;; classical functionnal programming. I should try to stick to
;; constraint programming here as it seems it would be easier to use
;; core.logic further.

(comment
  (let [test-state {:history [[[1 1] [2 2]]]
                    :player :white
                    :positions {:white #{['queen [1 1] [2 2]] ['queen [8 1]]
                                         ['queen [7 3]] ['queen [7 4]]}
                                :black #{['queen [8 4]]}}}]
    (reduce #(move-piece % (first %2) (second %2))
            test-state
            [[[2 2] [2 5]]
             [[8 4] [8 8]]
             [[2 5] [4 5]]])))

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
  (println (str "SPECIAL: " arrow))
  (let [current-player (:player state)
        old-player-positions (get (:positions state) current-player)
        new-player-positions (set (reduce #(change-piece-position % (first %2) (second %2))
                                          old-player-positions
                                          (map second (castling-from-side-and-player (:Castling arrow)
                                                                                     current-player))))]
    {:player (if (= :white current-player) :black :white)
     :history (conj (:history state) arrow)
     :positions (assoc (:positions state) current-player new-player-positions)}))

(defmethod move-piece (type [])
  [state arrow]
  (println "NORMAL MOVE")
  (let [[[x1 y1] [x2 y2]] arrow]
    (when-let [legal-move (incoll? [x2 y2]
                                   (moves-from-state-position state [x1 y1]))]
      (let [current-player (:player state)
            other-player (if (= :white current-player) :black :white)
            other-player-positions (change-piece-position (get-in state
                                                                  [:positions other-player])
                                                          [x2 y2] nil)
            new-player-positions (set (change-piece-position (get-in state
                                                                     [:positions current-player])
                                                             [x1 y1] [x2 y2]))]
        {:player (if (= :white current-player) :black :white)
         :history (conj (:history state) [[x1 y1] legal-move])
         :positions (assoc (:positions state)
                      current-player new-player-positions
                      other-player other-player-positions)}))))

(comment
  (move-piece {:positions {:white #{'(rook [1 1]) '(rook [8 1]) '(king [5 1])}
                           :black #{'(rook [1 8]) '(rook [8 8]) '(king [5 8])}}
               :history []
               :player :white}
              {:Castling :KingSide})
  (move-piece {:positions {:white #{'(rook [1 1]) '(rook [8 1]) '(king [5 1])}
                           :black #{'(rook [1 8]) '(rook [8 8]) '(king [5 8])}}
               :history []
               :player :white}
              [[1 1] [1 8]]))

(defn possible-positions-to-destination
  "Can help to parse PGN (see namespace interface). It considers the
  current player as a basic optimisation."
  [state [x y]]
  (run* [q]
    (membero q (positions-from-state state (:player state)))
    (project [q]
             (membero [x y] (moves-from-state-position state q)))))

(comment
  (possible-positions-to-destination initial-state [1 3]))

;; OK, so it's sound rather good now.
;;
;; We can add some functions for checking history consistency given an
;; initial state; some constraint programming to find which histories
;; can be used from a state to reach another one; some constraint
;; programming to check whether a final state is reachable in less than
;; n moves, given an initial state.
;;
;; We could also dive into artificial intelligence and define some
;; strategies.

(defn move-pieces-from-state-through-history
  "Reduction of an history on a given state."
  [state history]
  (reduce move-piece
          state
          history))

(comment
  (move-pieces-from-state-through-history
              {:player :black
               :history [[[8 1] [8 8]]]
               :positions {:white #{'(rook [8 1] [8 8]) '(rook [1 1]) '(king [5 1])}
                           :black #{'(rook [1 8]) '(rook [8 8] nil) '(king [5 8])}}}
              []))

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

(comment
  (one-move-far-state-from-state? initial-state
                                  (move-piece initial-state [2 1] [2 3])))

(comment
  (let [test-state {:history [[[1 1] [2 2]]]
                    :player :white
                    :positions {:white #{['queen [1 1] [2 2]] ['queen [8 1]]
                                         ['queen [7 3]] ['queen [7 4]]}
                                :black #{['queen [8 4]]}}}]
    (one-move-far-state-from-state? initial-state
                                    test-state)))

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

(comment
  ;; Something a bit vain: compute all the $2^{12*n}$ possibility for n
  ;; moves. Anway, now we can refine it to find all allowed moves from
  ;; given state.
  ;; This makes my JVM crashes because of OutOfMemoryError GC overhead
  ;; limit exceeded. LOL
  (-> (do
        (let [b (logic-history 2)]
          (run* [q]
            (everyg #(membero % (range boardsize)) (vec (flatten b)))
            (l/== q b))))
      count println))

;; Returns
(declare history-from-state-to-state)
