(ns tic-tac-toe.core
(:require [clojure.set :as set]))

(defn initial_board
  "initial board"
  [x_cells o_cells]
  {:x (set x_cells) :o (set o_cells)})

(defn mark [board cell turn]
    (assoc board turn (conj (turn board) cell)))

(defn check_for_draw? [board]
  (= (+ (count (:x board)) (count (:o board))) 9))

(def win-sets [#{1 2 3} #{4 5 6} #{7 8 9} #{1 4 7} #{2 5 8} #{3 6 9} #{1 5 9} #{3 5 7}])

(defn winner? [board turn]
  (some #(every? (turn board) %) win-sets))

(defn empty_cells [board]
  (set/difference (apply sorted-set (range 1 10)) (:x board) (:o board)))

(defn next_level_board [board sym]
  (if-not (winner? board sym)
    (map #(mark board % sym) (empty_cells board))))

(defn evaluate [board]
  (cond
    (some #(every? (:x board) %) win-sets) 10
    (some #(every? (:o board) %) win-sets) -10
    :else 0))


(defn opponent [sym]
    (cond
      (= sym :x) :o
      (= sym :o) :x ))

(defn game_tree [board sym]
  {:root board :children (map #(game_tree % (opponent sym)) (next_level_board board sym))})


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
    (println (clojure.string/join "\n" (for [row rows] (show_row row))))
    (println )))

(defn player_move1 [board]
  (print_board board)
  (println "player-1 move: ")
  (mark board  (read) :x))

(defn index-of-first-occurence [item coll]
  (count (take-while (partial not= item) coll)))

(defn second-level-move [boards turn]
  (let [opponent-boards (map next_level_board boards (opponent turn))]
    (opponent-boards)))

(defn random_move [board turn]
  (mark board (rand-nth (apply list (empty_cells board))) turn))

(defn best-move [board turn]
  (let [next-boards (next_level_board board turn)
        scores (map evaluate next-boards)
        next-position (index-of-first-occurence -10 scores)]
    (if (= next-position (count scores))
      (random_move board turn)
      (assoc board turn ((nth next-boards next-position) turn)))))


(defn print-terminal-state [board sym]
  (print_board board)
  (case sym
    :x (println "player 1 won!")
    :o (println "player 2 won!")
    "default" (println "draw!")))


(defn play
  [board]
  (let [play1 (best-move board :x)]
    (cond
      (winner? play1 :x) (print-terminal-state play1 :x)
      (check_for_draw? play1) (println "Draw")
      :else (let [play2 (best-move play1 :o)]
              (cond
                (winner? play2 :o) (print-terminal-state play2 :o)
                (check_for_draw? play2) (println "Draw")
                :else (play play2))))))

(defn -main []
  (let [initial_board {:x (set ()) :o (set ()) }]
    (play initial_board)))
