(ns poc.general-test
  (:require  [clojure.test :refer :all]))

(let [my-state {:positions {:white ['(queen [5 5] [1 2] nil)
                                    '(rook [2 2])
                                    '(pawn [3 2] nil)
                                    '(pawn [4 2])]
                            :black ['(king [1 7] nil)
                                    '(bishop [2 7])
                                    '(pawn [3 7])
                                    '(pawn [4 7])]}
                :history []
                :player :white}
      type (lvar)
      colour (lvar)
      position (lvar)
      chosen position]
  (distinct (run* [q]
              (state->type-colour-position-o my-state type colour position)
              (l/!= nil q)
              (l/== q chosen))))

(comment
  (test-func (fn [[a b]] (unreachable-positions (positions-from-state initial-state)
                                               [1 1] [a b])))

  (let [state {:positions {:white #{'(queen [4 1])}
                           :black #{'(pawn [3 1]) '(pawn [4 2]) '(pawn [5 1])}}
               :history []
               :player :white}]
    (= (set (test-func (fn [[a b]] (unreachable-positions (positions-from-state state)
                                                         [4 1] [a b]))))
       (set '(;; horizontal
              [1 1] [2 1] [6 1] [7 1] [8 1]
              ;; vertical
              [4 3] [4 4] [4 5] [4 6] [4 7] [4 8]))))
  (let [state {:positions {:white #{'(queen [4 1])
                                    '(pawn [3 1]) '(pawn [4 2]) '(pawn [5 1])}
                           :black #{}}
               :history []
               :player :white}]
    (= (set (test-func (fn [[a b]] (unreachable-positions (positions-from-state state)
                                                         [4 1] [a b]))))
       (set '(;; horizontal
              [1 1] [2 1] [6 1] [7 1] [8 1]
              ;; vertical
              [4 3] [4 4] [4 5] [4 6] [4 7] [4 8]))))
  (let [state {:positions {:black #{'(queen [4 1])}
                           :white #{'(pawn [3 1]) '(pawn [4 2]) '(pawn [5 1])}}
               :history []
               :player :white}]
    (= (set (test-func (fn [[a b]] (unreachable-positions (positions-from-state state)
                                                         [4 1] [a b]))))
       (set '(;; horizontal
              [1 1] [2 1] [6 1] [7 1] [8 1]
              ;; vertical
              [4 3] [4 4] [4 5] [4 6] [4 7] [4 8]))))
  (let [state {:positions {:black #{'(queen [4 1])
                                    '(pawn [3 1]) '(pawn [4 2]) '(pawn [5 1])}
                           :white #{}}
               :history []
               :player :white}]
    (= (set (test-func (fn [[a b]] (unreachable-positions (positions-from-state state)
                                                         [4 1] [a b]))))
       (set '(;; horizontal
              [1 1] [2 1] [6 1] [7 1] [8 1]
              ;; vertical
              [4 3] [4 4] [4 5] [4 6] [4 7] [4 8])))))

(comment
  (defn test-func
    [testee]
    (distinct
     (run* [q]
       (fresh [a b]
         (l/== q [a b])
         (membero a (range 1 (inc boardsize)))
         (membero b (range 1 (inc boardsize)))
         (testee [a b])))))

  (distinct
   (run* [q]
     (fresh [a b]
       (l/== q [a b])
       (membero a (range 1 (inc boardsize)))
       (membero b (range 1 (inc boardsize)))
       (unreachable-positions initial-state [4 1] [a b]))))

  (test-func (fn [[a b]] (unreachable-positions (positions-from-state initial-state)
                                               [1 1] [a b]))))

;; This function can now be called:
(let [test-state {:history [[[1 1] [2 2]]]
                  :player :white
                  :positions {:white #{['queen [1 1] [2 2]]}
                              :black #{['queen [3 2]]}}}]
  (moves-from-state-position test-state [2 2]))


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

(comment
  (possible-positions-to-destination initial-state [1 3]))


(comment
  (move-pieces-from-state-through-history
   {:player :black
    :history [[[8 1] [8 8]]]
    :positions {:white #{'(rook [8 1] [8 8]) '(rook [1 1]) '(king [5 1])}
                :black #{'(rook [1 8]) '(rook [8 8] nil) '(king [5 8])}}}
   []))

(comment
  (one-move-far-state-from-state? initial-state
                                  (move-piece initial-state [[2 1] [2 3]])))

(comment
  (let [test-state {:history [[[1 1] [2 2]]]
                    :player :white
                    :positions {:white #{['queen [1 1] [2 2]] ['queen [8 1]]
                                         ['queen [7 3]] ['queen [7 4]]}
                                :black #{['queen [8 4]]}}}]
    (one-move-far-state-from-state? initial-state
                                    test-state)))


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

(display-state-on-board (pgn-parser pgn-sample))

(display-state-on-board (move-pieces-from-state-through-history
                         {:positions {:white #{'(rook [1 1])
                                               '(rook [8 1])
                                               '(king [5 1])}
                                      :black #{'(rook [1 8])
                                               '(rook [8 8])
                                               '(king [5 8])}}
                          :history []
                          :player :white}
                         [[[8 1] [8 8]]
                          {:Castling :QueenSide}
                          {:Castling :QueenSide}]))

(def fake-header "Fake header to ease test. It is syntactly correct."
  "[Event \"event\"]
[Site \"site\"]
[Date \"2015.02.14\"]
[Round \"0\"]
[White \"white\"]
[Black \"black\"]
[Result \"1/2-1/2\"]")

(def sample-1
  (str-n fake-header
         "1.e4 c5 2.c3 d5 3.exd5 Qxd5 4.d4 Nf6 5.Nf3 Bg4 6.Be2 e6 7.h3
Bh5 8.O-O Nc6 9.Be3 cxd4 10.cxd4 Bb4 11.a3 Ba5 12.Nc3 Qd6 13.Nb5 Qe7
14.Ne5 Bxe2 15.Qxe2 O-O 16.Rac1 Rac8 17.Bg5 Bb6 18.Bxf6 gxf6 19.Nc4 Rfd8
20.Nxb6 axb6 21.Rfd1 f5 22.Qe3 Qf6 23.d5 Rxd5 24.Rxd5 exd5 25.b3 Kh8
26.Qxb6 Rg8 27.Qc5 d4 28.Nd6 f4 29.Nxb7 Ne5 30.Qd5 f3 31.g3 Nd3 32.Rc7
Re8 33.Nd6 Re1+ 34.Kh2 Nxf2 35.Nxf7+ Kg7 36.Ng5 Kh6 37.Rxh7+ 1-0"))

(def sample-2
  (str-n fake-header
         "1.e4 c5 2.c3 d5 3.exd5 Qxd5 4.d4 Nf6 5.Nf3 Bg4 6.Be2 e6
7.O-O Nc6 8.Be3 cxd4 9.cxd4 Bb4 10.a3 Ba5 11.Nc3 Qd6 12.Ne5 Bxe2 13.Qxe2
Bxc3 14.bxc3 Nxe5 15.Bf4 Nf3+ 16.Qxf3 Qd5 17.Qd3 Rc8 18.Rfc1 Qc4 19.Qxc4
Rxc4 20.Rcb1 b6 21.Bb8 Ra4 22.Rb4 Ra5 23.Rc4 O-O 24.Bd6 Ra8 25.Rc6 b5
26.Kf1 Ra4 27.Rb1 a6 28.Ke2 h5 29.Kd3 Rd8 30.Be7 Rd7 31.Bxf6 gxf6 32.Rb3
Kg7 33.Ke3 e5 34.g3 exd4+ 35.cxd4 Re7+ 36.Kf3 Rd7 37.Rd3 Raxd4 38.Rxd4
Rxd4 39.Rxa6 b4 1/2-1/2"))

(def sample-3
  (str-n fake-header
         "1.Nf3 d5 2.d4 c6 3.c4 e6 4.Nbd2 Nf6 5.e3 Nbd7 6.Bd3 Bd6 7.e4
dxe4 8.Nxe4 Nxe4 9.Bxe4 O-O 10.O-O h6 11.Bc2 e5 12.Re1 exd4 13.Qxd4 Bc5
14.Qc3 a5 15.a3 Nf6 16.Be3 Bxe3 17.Rxe3 Bg4 18.Ne5 Re8 19.Rae1 Be6 20.f4
Qc8 21.h3 b5 22.f5 Bxc4 23.Nxc4 bxc4 24.Rxe8+ Nxe8 25.Re4 Nf6 26.Rxc4
Nd5 27.Qe5 Qd7 28.Rg4 f6 29.Qd4 Kh7 30.Re4 Rd8 31.Kh1 Qc7 32.Qf2 Qb8
33.Ba4 c5 34.Bc6 c4 35.Rxc4 Nb4 36.Bf3 Nd3 37.Qh4 Qxb2 38.Qg3 Qxa3
39.Rc7 Qf8 40.Ra7 Ne5 41.Rxa5 Qf7 42.Rxe5 fxe5 43.Qxe5 Re8 44.Qf4 Qf6
45.Bh5 Rf8 46.Bg6+ Kh8 47.Qc7 Qd4 48.Kh2 Ra8 49.Bh5 Qf6 50.Bg6 Rg8 1/2-1/2"))

(def sample-4
  (str-n fake-header
         "1.Nf3 d5 2.d4 c6 3.c4 e6 4.Nbd2 Nf6 5.e3 c5 6.b3 Nc6 7.Bb2
cxd4 8.exd4 Be7 9.Rc1 O-O 10.Bd3 Bd7 11.O-O Nh5 12.Re1 Nf4 13.Bb1 Bd6
14.g3 Ng6 15.Ne5 Rc8 16.Nxd7 Qxd7 17.Nf3 Bb4 18.Re3 Rfd8 19.h4 Nge7
20.a3 Ba5 21.b4 Bc7 22.c5 Re8 23.Qd3 g6 24.Re2 Nf5 25.Bc3 h5 26.b5 Nce7
27.Bd2 Kg7 28.a4 Ra8 29.a5 a6 30.b6 Bb8 31.Bc2 Nc6 32.Ba4 Re7 33.Bc3 Ne5
34.dxe5 Qxa4 35.Nd4 Nxd4 36.Qxd4 Qd7 37.Bd2 Re8 38.Bg5 Rc8 39.Bf6+ Kh7
40.c6 bxc6 41.Qc5 Kh6 42.Rb2 Qb7 43.Rb4 1-0"))

(display-state-on-board (pgn-parser sample-4))
