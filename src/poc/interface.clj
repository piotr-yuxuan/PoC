(ns poc.interface
  "Namespace for interface. It contains persers to construct state from
  exterior chess data, functions to generate a board out of a state and
  few side-effect functions to display the board.

  Two parsers are available here
  It contains a parser for a custom game notation, very close to
  Portable Game Notation grammar. However, each move of the movetext
  contains the initial and final positions of the piece. That's to say,
  contrarily to the example shown in Wikipedia, the string '1. e4 e5'
  means: for first move, the white piece in e4 is moved to e5.

  See further explanation on
  https://en.wikipedia.org/wiki/Portable_Game_Notation.

  Beware: this is a naive grammar which doesn't handle all the
  guidelines, just what seems the most important. The whole
  specification can be found on
  http://www6.chessclub.com/help/PGN-spec.

  For the official PGN use inference, I should first correct all bugs in
  the logic then I will be able to implement it why a custom
  parser (more specifically, by upgrading the translator and reducer)."
  (:require [poc.commons :refer :all]
            [poc.boardobjects :refer :all]
            [poc.gamelogic :refer :all])
  (:require [instaparse.core :as i]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ■ 9632 ■ 25A0 BLACK SQUARE                               ;;
;; □ 9633 □ 25A1 WHITE SQUARE                               ;;
;; ▢ 9634 ▢ 25A2 WHITE SQUARE WITH ROUNDED CORNERS          ;;
;; ▣ 9635 ▣ 25A3 WHITE SQUARE CONTAINING BLACK SMALL SQUARE ;;
;; ▤ 9636 ▤ 25A4 SQUARE WITH HORIZONTAL FILL                ;;
;; ▥ 9637 ▥ 25A5 SQUARE WITH VERTICAL FILL                  ;;
;; ▦ 9638 ▦ 25A6 SQUARE WITH ORTHOGONAL CROSSHATCH FILL     ;;
;; ▧ 9639 ▧ 25A7 SQUARE WITH UPPER LEFT TO LOWER RIGHT FILL ;;
;; ▨ 9640 ▨ 25A8 SQUARE WITH UPPER RIGHT TO LOWER LEFT FILL ;;
;; ▩ 9641 ▩ 25A9 SQUARE WITH DIAGONAL CROSSHATCH FILL       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn format-state
  [state]
  (mapcat (fn [[colour rest]]
            (vec (map (fn [[type & positions]]
                        [colour type (last positions)])
                      (filter #(not (nil? (last %))) rest))))
          (:positions state)))

(comment
  (format-state {:player :black
                 :history [[[8 1] [8 8]]]
                 :positions {:white #{'(rook [8 1] [8 8])
                                      '(rook [1 1])
                                      '(king [5 1])}
                             :black #{'(rook [8 8] nil)
                                      '(rook [1 8])
                                      '(king [5 8])}}}))

(defn board-from-state
  "Returns the string for the board. This string can be partitionned
then display as is."
  ([state]
   (board-from-state state blank-board))
  ([state board]
    (reduce (fn [board [colour type [x y]]]
              (str-mutate board
                          (get-in icons [(eval type) colour])
                          (+ (dec x) (* boardsize (- boardsize y)))))
            board
            (format-state state))))

(defn display-state-on-board
  "Display state over a board. You can change the default blank board."
  ([state]
   (display-state-on-board state blank-board))
  ([state board]
   "Side-effect function. Display a human-readable board."
   (println "-----------------")
   (reduce (fn [a b] (println b))
           board
           (partition boardsize
                      (board-from-state state board)))))

(let [history [[[2 2] [2 5]] ;; white
               [[8 4] [8 2]] ;; black
               [[2 5] [4 5]] ;; white, and so on…
               [[8 2] [1 2]]]
      depart-state {:history [[[1 1] [2 2]]]
                    :player :white
                    :positions {:white #{['queen [1 1] [2 2]] ['queen [8 1]]
                                         ['queen [7 3]] ['queen [7 4]]}
                                :black #{['queen [8 4]]}}}]
  (print "Depart ")
  (display-state-on-board depart-state)
  (println "Arrival with custom board ")
  (display-state-on-board (move-pieces-from-state-through-history depart-state
                                                                  history)
                          ;; You can change the defautl blank board
                          (str-mutate blank-board "?" 29 1)))

(comment
  "Don't work, don't know why yet."
  (let [test-state {:history [[[1 1] [2 2]]]
                    :player :white
                    :positions {:white #{['queen [1 1] [2 2]] ['queen [8 1]]
                                         ['queen [7 3]] ['queen [7 4]]}
                                :black #{['queen [8 4]]}}}]
    (map
     (fn [history]
       (println history)
       (display-state-on-board (reduce #(move-piece % (first %2) (second %2))
                                         test-state
                                         history)))
     (reduce #(vec (conj % (vec (concat (last %) [%2])))) [] [[[2 2] [2 5]]
                                                              [[8 4] [8 8]]
                                                              [[2 5] [4 5]]]))))

(def whitespace (i/parser "whitespace = #'\\s+'"))

(def common-definitions
  (str-n
   "Tags = MandatoryTags SupplementaryTag*"
   "<MandatoryTags> = Event Site Date Round White Black Result"
   "Event = <'['> <'Event'> Content <']'>"
   "Site = <'['> <'Site'> Content <']'>"
   "Date = <'['> <'Date'> DateContent <']'>"
   "Round = <'['> <'Round'> RoundContent <']'>"
   "White = <'['> <'White'> Name <']'>"
   "Black = <'['> <'Black'> Name <']'>"
   "Result = <'['> <'Result'> ResultContent <']'>"
   "<SupplementaryTag> = Annotator | PlyCount | TimeControl | Time | Termination | Mode | FEN"
   "Annotator = <'['> <'Annotator'> Name <']'>"
   "PlyCount = <'['> <'PlyCount'> Content <']'>" ;; ply (sic.)
   "TimeControl = <'['> <'TimeControl'> Content <']'>"
   "Time = <'['> <'Time'> TimeContent <']'>"
   "Termination = <'['> <'Termination'> TerminationContent <']'>"
   "Mode = <'['> <'Mode'> ModeContent <']'>"
   "FEN = <'['> <'FEN'> Content <']'>"

   (str "<Content> = <'\"'> CC <'\"'>")
   (str "<DateContent> = <'\"'>" (print-str #"[0-9?]{4}.[0-9?]{1,2}.[0-9?]{1,2}") "<'\"'>")
   (str "<RoundContent> = <'\"'>" (print-str #"[0-9]*[0-9?]{0,1}") "<'\"'>")
   (str "<Name> = <'\"'> CC* <'\"'>") ;; should be lastName, firstName
   "<ResultContent> = '\"1-0\"' | '\"0-1\"' | '\"1/2-1/2\"' | '\"*\"'"
   "<ModeContent> = <'\"'> ('OTB' | 'ICS') <'\"'>"
   "<TerminationContent> = <'\"'> ('abandoned' | 'adjudication' | 'death' | 'emergency' | 'normal' | 'rules infraction' | 'time forfeit' | 'unterminated') <'\"'>"
   (str "<TimeContent> = <'\"'>" (print-str #"[0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}") "<'\"'>")

   "<BracketComment> = <'{'> Comment <'}'>"
   "<LineComment> = <';'> Comment"
   "Comment = CC*"

   "<Position> = CD"
   (str "<CC> = #'[a-zA-Z0-9,-.;|\\/!?() ]*'")
   (str "<CD> = #'[a-zA-Z0-9,-.;|!?()+=]+'")
   "Movetext = Move* ?[EndResult]"
   "EndResult = '1-0' | '0-1' | '1/2-1/2'"))

(def custom-sample
  ;; This sample has no castling inside. Moreover every move has two
  ;; positions: from and to.
  ;; This sample is for the custom notation language. It doesn't fit it
  ;; yet so don't trust it! However more complex to parse, the real PGN
  ;; would be a better choice so just focus on it.
  "[Event \"F/S Return Match\"]
[Site \"Belgrade, Serbia Yugoslavia|JUG\"]
[Date \"1992.11.04\"]
[Round \"29\"]
[White \"Fischer, Robert J.\"]
[Black \"Spassky, Boris V.\"]
[Result \"1/2-1/2\"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 {This opening is called the Ruy Lopez.}
4. Ba4 Nf6 6. Re1 b5 7. Bb3 d6 8. h3 Nb8  10. d4 Nbd7
11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7 14. Bg5 b4 15. Nb1 h6 16. Bh4 c5 17. dxe5
Nxe4 18. Bxe7 Qxe7 19. exd6 Qf6 20. Nbd2 Nxd6 21. Nc4 Nxc4 22. Bxc4 Nb6
23. Ne5 Rae8 24. Bxf7+ Rxf7 25. Nxf7 Rxe1+ 26. Qxe1 Kxf7 27. Qe3 Qg5 28. Qxg5
hxg5 29. b3 Ke6 30. a3 Kd6 31. axb4 cxb4 32. Ra5 Nd5 33. f3 Bc8 34. Kf2 Bf5
35. Ra7 g6 36. Ra6+ Kc5 37. Ke1 Nf4 38. g3 Nxh3 39. Kd2 Kb5 40. Rd6 Kc5 41. Ra6
Nf2 42. g4 Bd3 1/2-1/2")

(def custom-grammar
  (str-n "CST = Tags Movetext"
         common-definitions
         "EndResult = '1-0' | '0-1' | '1/2-1/2'"
         "Move = <Number> <'.'|'...'> (From To) <BracketComment*> ?[<LineComment>]"
         "<BracketComment> = <'{'> Comment <'}'>"
         "<LineComment> = <';'> Comment"
         "Comment = CC*"
         "From = Position"
         "To = Position"
         "Final = Position"
         (str "Number =" (print-str #"[0-9]+"))))

(def custom-tokeniser
  "This tokeniser overwrite some definitions in order to ignore what isn't relevant for us."
  (i/parser custom-grammar
            :auto-whitespace whitespace))

(custom-tokeniser custom-sample)

(def pgn-sample
  "[Event \"F/S Return Match\"]
[Site \"Belgrade, Serbia Yugoslavia|JUG\"]
[Date \"1992.11.04\"]
[Round \"29\"]
[White \"Fischer, Robert J.\"]
[Black \"Spassky, Boris V.\"]
[Result \"1/2-1/2\"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 {This opening is called the Ruy Lopez.}
4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O 9. h3 Nb8  10. d4 Nbd7
11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7 14. Bg5 b4 15. Nb1 h6 16. Bh4 c5 17. dxe5
Nxe4 18. Bxe7 Qxe7 19. exd6 Qf6 20. Nbd2 Nxd6 21. Nc4 Nxc4 22. Bxc4 Nb6
23. Ne5 Rae8 24. Bxf7+ Rxf7 25. Nxf7 Rxe1+ 26. Qxe1 Kxf7 27. Qe3 Qg5 28. Qxg5
hxg5 29. b3 Ke6 30. a3 Kd6 31. axb4 cxb4 32. Ra5 Nd5 33. f3 Bc8 34. Kf2 Bf5
35. Ra7 g6 36. Ra6+ Kc5 37. Ke1 Nf4 38. g3 Nxh3 39. Kd2 Kb5 40. Rd6 Kc5 41. Ra6
Nf2 42. g4 Bd3 43. Re6 1/2-1/2")

(def pgn-grammar
  (str-n "PGN = Tags Movetext"
         common-definitions
         "Movetext = Move* ?[<EndResult>]"
         "<Move> = <Number> ((<'.'> (WhiteMove | (WhiteMove BlackMove))) | (<'...'> BlackMove)) <BracketComment*> ?[<LineComment>]"
         "<WhiteMove> = Token"
         "<BlackMove> = Token"
         "Token = Position"
         (str "Number =" (print-str #"[0-9]+"))))

(def pgn-tokeniser
  (i/parser pgn-grammar
            :auto-whitespace whitespace))

(comment
  (pgn-tokeniser pgn-sample))

(defn struct-presenter
  "Present the data in a map.

  Technical insight: kwp for keyword path, ns for nested vector, k for
  keyword, &v for rest vectors, mergef for merge function which
  determines whether map should be merged."
  [kwp ns]
  (let [[k & v] ns
        mergef (if (= k kwp) identity #(apply merge %))]
    (if (and (vector? ns) (not (nil? v)) (keyword? k))
      (assoc {} k (mergef (map #(struct-presenter kwp %) v)))
      ns)))

(comment
  (struct-presenter :Movetext (pgn-tokeniser pgn-sample))
  (struct-presenter :Movetext (custom-tokeniser custom-sample)))

(defn post-struct-presenter
  "Present the data in a map. Take the output of struct-presenter and
  shaped it into a custom structure.

  Technical insight: nkw for nested key word. fkw for flat keyword. nm
  for nested map."
  [nkw getter nm]
  (let [lkw (last nkw)
        nkw (-> nkw reverse rest reverse)]
    (map #(-> (get % lkw) getter) (get-in nm nkw))))

(comment
  (post-struct-presenter [:CST :Movetext :Move]
                         (comp vec vals)
                         (struct-presenter :Movetext (custom-tokeniser custom-sample)))
  (post-struct-presenter [:PGN :Movetext :Token]
                         identity
                         (struct-presenter :Movetext (pgn-tokeniser pgn-sample))))

(def custom-move-grammar
  "Grammar to parse information for each move."
  (str-n "<M> = (<I*> L <I*> F <I*>) | ''" ;; Letter and Figure
         (str "I = " (print-str #"[a-zA-Z0-9,-.;|!?()+=]"))
         (str "L = " (print-str #"[a-z]"))
         (str "F = " (print-str #"[1-8]"))))

(def pgn-token-grammar
  "Grammar to parse information for each move. It's a loosy one and
  accepts more than it should."
  (str-n "<M> = Castling | (Piece* File* Capture* L F Promotion* <I*>)"
         ;; letter abbreviations. Pawn is usually given empty string as
         ;; abbreviation.
         "Castling = QueenSide | KingSide"
         "QueenSide = <KingSide '-O'>"
         "KingSide = <'O-O'>"
         "Promotion = <'='> Abbrev"
         "<Abbrev> = Abbreviations "
         "Piece = Abbreviations "
         (str "<Abbreviations> = " (print-str #"[KQRBNP]"))
         (str "File = " (print-str #"[a-h]"))
         (str "F = " (print-str #"[1-8]"))
         (str "I = " (print-str #"[!#?()+=]"))
         (str "L = " (print-str #"[a-h]"))
         "Capture = <'x'>"))

(defn abbr-to-symbol
  [abbr]
  (get {"K" 'king
        "Q" 'queen
        "R" 'rook
        "B" 'bishop
        "N" 'knight
        "P" 'pawn} abbr))

(comment
  ((i/parser pgn-token-grammar) "Kexa1?")
  ((i/parser pgn-token-grammar) "Pe8=Q#")
  ((i/parser pgn-token-grammar) "O-O"))

(defn custom-translator
  "Use a grammar to map (translate) algebraic notation into a vector."
  [grammar input]
  (map #(map (i/parser grammar
                       :auto-whitespace whitespace)
             %)
       input))

(defn pgn-translator
  "Use a grammar to map (translate) algebraic notation into a vector."
  [grammar input]
  (map #((i/parser grammar
                   :auto-whitespace whitespace) %)
       input))

(comment
  (custom-translator custom-move-grammar
                     (post-struct-presenter [:CST :Movetext :Move]
                                            (comp vec vals)
                                            (struct-presenter :Movetext (custom-tokeniser custom-sample))))
  (pgn-translator pgn-token-grammar
                  (post-struct-presenter [:PGN :Movetext :Token]
                                         identity
                                         (struct-presenter :Movetext (pgn-tokeniser pgn-sample)))))

(defn mock-reducer
  "This is only a mock one and should be written again to be extended as
  it works only when there is no castling, no promotions and when each
  move has two nested positions. It's downgraded version of what could
  be done but that level of detail would lead in endless cogitation."
  [state input]
  (seq-history (map #(case (first %)
                       :L (inc (- (int (-> % second first)) (int \a)))
                       :F (-> % second Integer.) %)
                    (partition 2 (flatten input)))))

(defn ambiguity-solver
  "Returns a not ambiguous departure position. Don't look at hints such
  as file or piece once only one result is found."
  [state move choices]
  (let [file (if (contains? move :File)
               (fn [choices] (filter #(= (first %)
                                        (-> (:File move) Integer.))
                                    choices))
                identity)
        piece (if (contains? move :Piece)
                (fn [choices] (filter #(= (abbr-to-symbol (:Piece move))
                                         (type-from-state-position state %))
                                     choices))
                identity)]
    (reduce #(if (= 1 (count %)) % (apply %2 [%]))
            choices
            [file piece])))

(defn atom-reducer
  [state move]
  (let [get-dest (fn [L F] (let [L (inc (- (-> L first int) (int \a)))
                                F (-> F Integer.)]
                            [L F]))]
    (let [movemap (mapify move)
          destination (if (every? movemap [:L :F])
                        (get-dest (:L movemap) (:F movemap))
                        nil)]
      (if-let [special (contains-some? move special-moves)]
        (if destination
          (assoc (dissoc movemap :L :F) :position destination)
          movemap) ;; prettify structure?
        (let [choices (possible-positions-to-destination state destination)
              departure (first (ambiguity-solver state move choices))]
          [departure destination])))))

(comment
  (atom-reducer initial-state
                (first (map (fn [arg] (reduce #(assoc % (first %2) (second %2)) {} arg))
                            '(([:Castling :Queen] [:L "e"] [:F "4"])))))
  ;; =>
  [[5 2] [5 4]])

(defn pgn-reducer
  [state input]
  (let [inputmap (map (fn [arg] (reduce #(assoc % (first %2) (second %2)) {} arg))
                      input)]
    (reduce #(let [arrow (atom-reducer % %2)
                   [from to] (if (vector? arrow) arrow (:position arrow))]
               (println (str "Move: " (type-from-state-position % from)
                             " moving from " from " to " to "."))
               (move-piece % arrow))
            state
            inputmap)))

(defn parser-factory
  "A string is taken as input by the tokeniser which returns nested
  vectors of tokens. The presenter returns a more convenient data
  structure. The translator change the position tokens to something
  understandable by the program."
  [tokeniser presenter translator reducer]
  (fn [string] (-> string
                  tokeniser
                  presenter
                  translator
                  reducer)))

(def custom-parser
  "This parses to an history, not a state. However you can use
  move-pieces-from-state-through-history in the reducer to get a state
  as a result :-)"
  (let [tokeniser custom-tokeniser
        presenter (fn [input]
                    (let [upper :Movetext
                          get-in-kw [:CST :Movetext :Move]
                          main (partial struct-presenter upper)
                          post (partial post-struct-presenter get-in-kw (comp vec vals))]
                      (-> input
                          main
                          post)))
        translator (partial custom-translator custom-move-grammar)
        reducer (partial mock-reducer initial-state)]
    (parser-factory tokeniser
                    presenter
                    translator
                    reducer)))

(comment
 (custom-parser custom-sample))

(def pgn-parser
  "If I also parse the FEN notation I will be able to start from any
  board (with enhanced reducer parameter) but this seems a bit too far
  as now."
  (let [tokeniser pgn-tokeniser
        presenter (fn [input]
                    (let [upper :Movetext
                          get-in-kw [:PGN :Movetext :Token]
                          main (partial struct-presenter upper)
                          post (partial post-struct-presenter get-in-kw identity)]
                      (-> input
                          main
                          post)))
        translator (partial pgn-translator pgn-token-grammar)
        reducer (let [special-moves [:Castling]]
                  (partial pgn-reducer initial-state))]
    (parser-factory tokeniser
                    presenter
                    translator
                    reducer)))

(comment
  (pgn-parser pgn-sample))

(display-state-on-board  {:positions {:white #{'(rook [1 1]) '(rook [8 1]) '(king [5 1])}
                                     :black #{'(rook [1 8]) '(rook [8 8]) '(king [5 8])}}
                         :history []
                         :player :white})

(display-state-on-board (move-pieces-from-state-through-history
                         {:positions {:white #{'(rook [1 1]) '(rook [8 1]) '(king [5 1])}
                                      :black #{'(rook [1 8]) '(rook [8 8]) '(king [5 8])}}
                          :history []
                          :player :white}
                         [[[8 1] [8 8]]
                          {:Castling :QueenSide}
                          {:Castling :QueenSide}]))
