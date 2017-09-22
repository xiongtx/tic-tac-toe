(ns tic-tac-toe.core
  (:require [clojure.string :as str])
  (:import java.lang.NumberFormatException)
  (:gen-class))

(def empty-board
  "Empty tic-tac-toe board."
  [[nil nil nil]
   [nil nil nil]
   [nil nil nil]])

(defn print-board
  "Print the board."
  [board]
  (println)
  (doseq [row board]
    (println (mapv #(if (keyword? %) (name %) "_") row))))

(defn piece
  "Piece on board at (row, col)."
  [board row col]
  (-> board
      (nth row)
      (nth col)))

(defn add-piece
  "Return board with piece added at (row, col)."
  [board player row col]
  {:pre [(#{:x :o} player)
         (<= 0 row 2)
         (<= 0 col 2)
         (nil? (piece board row col))]
   :post [(= player (piece % row col))]}
  (assoc-in board [row col] player))

(defn victory?
  "Check board's victory condition."
  [board player]
  (letfn [(win? [pieces]
            (every? #(= player %) pieces))]
    (or (win? (first board))
        (win? (second board))
        (win? (last board))
        (win? (map first board))
        (win? (map second board))
        (win? (map last board))
        (win? (map-indexed (fn [i x] (nth x i)) board))
        (win? (map-indexed (fn [i x] (nth x (- 2 i))) board)))))

(defn player-name
  "Return player's name from player keyword."
  [player]
  (str/upper-case (name player)))

(defn player-turn
  "Prompt player for (row, col) to place piece on board."
  [board player]

  (print-board board)
  (println "Player" (player-name player) ": ")

  (let [valid-indices #{"0" "1" "2"}
        [row-str col-str] (-> (read-line)
                              (str/split #"\s+"))]
    (if (or (not (valid-indices row-str))
            (not (valid-indices col-str)))

      (do
        (println "Invalid position:" row-str col-str)
        (recur board player))

      (let [[row col] (map #(Integer/parseInt %) [row-str col-str])]
        (if (some? (piece board row col))
          (do
            (println "Cannot add piece:" row col "is occupied")
            (recur board player))
          (add-piece board player row col))))))

(defn show-tie
  "Show that the game is a tie."
  [board]
  (print-board board)
  (println "It's a tie"))

(defn show-win
  "Show that a player won."
  [board player]
  (print-board board)
  (println "Player" (player-name player) "won!"))

(defn -main
  "Play tic-tac-toe at the command line."
  [& _]
  (loop [turn 0
         board empty-board]
    (if (= turn 9)
      (show-tie board)
      (let [player (if (even? turn) :x :o)
            other-player (if (= :x player) :o :x)]
        (if (victory? board other-player)
          (show-win board other-player)
          (recur (inc turn)
                 (player-turn board player)))))))
