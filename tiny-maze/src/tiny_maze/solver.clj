(ns tiny-maze.solver)


(defn neighbors
  "I can't take credit for this function. I ran across it while studying
  collection types in the book 'The Joy of Clojure' written by Micheal Fogus
  and Chris Houser. This function finds possible neighbors to a matrix
  position."
  ([size yx] (neighbors [[-1 0] [1 0] [0 1] [0 -1]] size yx))
  ([directions size yx]
   (filter (fn [new-yx]
             (every? #(< -1 % size) new-yx))
           (map #(vec (map + yx %)) directions))))

(defn solve-maze
  "This function first finds the possible neighbors to the current location
  and then checks each neighbor as a possible next location (must have
  a 0 or :E resident).
  If a 0 resides at the next location, the function is called with an updated
  maze (:x at current location) and the next location.
  If an :E resides at then next location, the final maze is returned with an
  :x at both the current and next locations."
  ([maze] (solve-maze maze [0 0]))
  ([maze curr-loc]
    (let [ns (neighbors (count maze) curr-loc)
          next-loc (first (filter #(#{0 :E} (get-in maze %)) ns))]
      (if (= :E (get-in maze next-loc))
        (assoc-in (assoc-in maze curr-loc :x) next-loc :x)
        (solve-maze (assoc-in maze curr-loc :x) next-loc)))))