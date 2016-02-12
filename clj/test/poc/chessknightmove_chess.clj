(ns poc.chessknightmove-chess
  (:require [poc.chessknightmove-chess :refer :all]
            [poc.pieces :refer :all]
            [clojure.test :refer :all]))

(let [test-state {:history []
                   :player :white
                   :positions {:white #{'(king [2 2]) '(king [8 1])
                                        '(king [4 4]) '(king [4 5])}
                               :black #{'(king [5 5])}}}]
  [(is (= (set (moves-from-state-position test-state [2 2]))
            #{[1 1] [2 1] [3 1] [1 2] [3 2] [1 3] [2 3] [3 3]})
         "Complete free move")
   (is (= (set (moves-from-state-position test-state [8 1]))
            #{[7 1] [7 2] [8 2]})
         "Check borders")
   (is (= (set (moves-from-state-position test-state [4 4]))
            #{[3 3] [4 3] [5 3] [3 4] [5 4] [3 5] [5 5]})
         "Avoid friends and eat ennemy")
   (is true
         "Not to go beyond obstacles in not relevant here")])

(let [test-state {:history []
                  :player :white
                  :positions {:white #{'(queen [2 2]) '(queen [8 1])
                                       '(queen [7 3]) '(queen [7 4])}
                              :black #{'(queen [8 4])}}}]
  [(is (= (set (moves-from-state-position test-state [2 2]))
            #{[1 1] [3 3] [4 4] [5 5] [6 6] [7 7] [8 8]
              [1 3] [3 1]
              [2 1] [2 3] [2 4] [2 5] [2 6] [2 7] [2 8]
              [1 2] [3 2] [4 2] [5 2] [6 2] [7 2] [8 2]})
         "Complete free move")
   (is (= (set (moves-from-state-position test-state [7 3]))
            #{[7 1] [7 2] [7 5] [7 6] [7 7] [7 8]
              [1 3] [2 3] [3 3] [4 3] [5 3] [6 3] [8 3]
              [5 1] [6 2] [8 4]
              [8 2] [6 4] [5 5] [4 6] [3 7] [2 8]})
         "Avoid friends and eat ennemy")])

(let [test-state {:history []
                  :player :white
                  :positions {:white #{'(pawn [2 2]) '(pawn [8 1])
                                       '(pawn [7 3]) '(pawn [7 4])}
                              :black #{'(pawn [8 4])}}}]
  (is (= (set (moves-from-state-position test-state [2 2]))
           #{[2 3] [2 4]})
        "Complete free move")
  (is false
        "En passant"))
