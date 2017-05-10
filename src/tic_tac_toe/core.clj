(ns tic-tac-toe.core
(:require [clojure.set :as set]))

(defn initial_board
  [x_cells o_cells]
  {:x (set x_cells) :o (set o_cells)})

(defn mark [board cell turn]
    (assoc board turn (conj (turn board) cell)))

(defn check_for_draw? [board]
  (= (+ (count (:x board)) (count (:o board))) 9))

(def win-sets [#{1 2 3} #{4 5 6} #{7 8 9} #{1 4 7} #{2 5 8} #{3 6 9} #{1 5 9} #{3 5 7}])
   
(defn empty_cells [board]
  (set/difference (apply sorted-set (range 1 10)) (:x board) (:o board)))

(defn next_level_board [board sym]
  (map #(mark board % sym) (empty_cells board)))


(defn opponent [sym]
    (cond
      (= sym :x) :o
      (= sym :o) :x ))

(defn game_tree [board sym]
  {:root board :children (map #(game_tree % (opponent sym)) (next_level_board board sym))})

(defn evaluate [board]
  (cond
    (some #(every? (:x board) %) win-sets) 10
    (some #(every? (:o board) %) win-sets) -10
    :else 0))

(defn symbol_at?
  [board cell]
  (cond
    (contains? (:x board) cell) " x "
    (contains? (:o board) cell) " o "
    :else "   "))

(defn show_row
  [row]
  (str "| " (clojure.string/join " | " row) " |"))

(defn print_board [board]
  (let [cells (map #(symbol_at? board %) (apply sorted-set (range 1 10)))
        rows (partition 3 cells)]
   (println (clojure.string/join "\n" (for [row rows] (show_row row))))))

(defn player_move1 [board]
  (print_board board)
  (println "player-1 move: ")
  (mark board  (read) :x))

(defn player_move2 [board]
  (print_board board)
  (println "Player-2 move: ")
  (mark board  (read) :o))

(defn play
  [board]
  (let [play1 (player_move1 board)]
    (cond
      (some #(every? (:x play1) %) win-sets) (println "Player 1 won")
      (check_for_draw? play1) (println "Draw")
      :else (let [play2 (player_move2 play1)]
              (cond
                (some #(every? (:o play2) %) win-sets) (println "Player 2 won")
                (check_for_draw? play2) (println "Draw")
                :else (play play2))))))

(defn -main []
  (let [initial_board {:x (set ()) :o (set ()) }]
    (play initial_board)))
