(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board coord]
  (into #{} (get board (first coord))))

(defn col-values [board coord]
  (into #{} (nth (apply map list board) (second coord))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board coord]
  (let [x (- (first coord) (mod (first coord) 3))
        y (- (second coord) (mod (second coord) 3))]
  (set (for [a (range 3)
             b (range 3)]
         (value-at board [(+ a x) (+ b y)])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) 
    #{}
    (set/difference all-values 
                    (row-values board coord) 
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (every? true? 
          (for [x [0 1 2 3 4 5 6 7 8]
                y [0 1 2 3 4 5 6 7 8]]
            (has-value? board [x y]))))

(defn rows [board]
  (for [x [0 1 2 3 4 5 6 7 8]]
    (row-values board [x x])))

(defn valid-rows? [board]
   (every? true? (map #(= all-values %) (rows board))))

(defn cols [board]
  (for [x [0 1 2 3 4 5 6 7 8]]
    (col-values board [x x])))

(defn valid-cols? [board]
   (every? true? (map #(= all-values %) (cols board))))

(defn blocks [board]
  (for [x (range 0 9 3)
        y (range 0 9 3)]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (every? true? (map #(= all-values %) (blocks board))))

(defn valid-solution? [board]
 (every? true? (map #(% board) [valid-blocks? valid-cols? valid-rows?])))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-points [board]
  (for [x [0 1 2 3 4 5 6 7 8]
        y [0 1 2 3 4 5 6 7 8]
        :let [result [x y]]
        :when (not(has-value? board [x y]))]
    result))

(defn find-empty-point [board]
  (first (find-empty-points board)))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [head (find-empty-point board)]
      (for [option (valid-values-for board head)
            solution (solve (set-value-at board head option))]
        solution))))


