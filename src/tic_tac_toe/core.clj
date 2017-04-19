(ns tic-tac-toe.core)

(defn initial_board
  [x_cells o_cells]
  {:x (set x_cells) :o (set o_cells)})

(defn mark [board cell turn]
    (assoc board turn (conj (turn board) cell)))

(defn draw? [board]
  (= (+ (count (:x board)) (count (:o board))) 9))


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
      (draw? play1) (println "Draw")
      :else (let [play2 (player_move2 play1)]
              (cond
                (draw? play2) (println "Draw")
                :else (play play2))))))

(defn -main []
  (let [initial_board {:x (set ()) :o (set ()) }]
  (play initial_board)))


