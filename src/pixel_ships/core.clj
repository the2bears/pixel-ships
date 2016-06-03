(ns pixel-ships.core)

(def ship-size 12)
(def always-solid [{:x 5, :y 2} {:x 5, :y 3} {:x 5, :y 4} {:x 5, :y 5} {:x 5, :y 9}])
(def hull-possibles [{:x 4, :y 1} {:x 5, :y 1} {:x 4, :y 2} {:x 3, :y 3} {:x 4, :y 3} {:x 3, :y 4} {:x 4, :y 4} {:x 2, :y 5} {:x 3, :y 5}
                     {:x 4, :y 5} {:x 1, :y 6} {:x 2, :y 6} {:x 3, :y 6} {:x 1, :y 7} {:x 2, :y 7} {:x 3, :y 7} {:x 1, :y 8} {:x 2, :y 8}
                     {:x 3, :y 8} {:x 1, :y 9} {:x 2, :y 9} {:x 3, :y 9} {:x 4, :y 9} {:x 3, :y 10} {:x 4, :y 10} {:x 5, :y 10}])

(def cockpit-possibles [{:x 4, :y 6} {:x 5, :y 6} {:x 4, :y 7} {:x 5, :y 7} {:x 4, :y 8} {:x 5, :y 8}])

(defn add-hull [cells]
  (let [seed (:seed cells)
        hull-count (count hull-possibles)
        checker (fn [result i mask]
                   (cond (>= i hull-count) result
                         (not= 0 (bit-and seed mask))
                           (recur (conj result (nth hull-possibles i)) (inc i) (bit-shift-left mask 1))
                         :else (recur result (inc i) (bit-shift-left mask 1))))]
    (assoc-in cells [:pixels :hull] (checker [] 0 1))))

(defn add-cockpit [cells]
  (let [seed (:seed cells)
        cockpit-count (count cockpit-possibles)
        checker (fn [solid-cells cockpit-cells i mask]
                   (cond (>= i cockpit-count)
                           (let [pixels (:pixels cells)]
                                 (assoc cells :pixels (merge pixels (assoc {} :cockpit (into [] cockpit-cells)) (assoc {} :solid solid-cells))))
                         (not= 0 (bit-and seed mask))
                           (recur (conj solid-cells (nth cockpit-possibles i)) cockpit-cells (inc i) (bit-shift-left mask 1))
                         :else (recur solid-cells (conj cockpit-cells (nth cockpit-possibles i)) (inc i) (bit-shift-left mask 1))))]
    (checker ((comp :solid :pixels) cells) ((comp :cockpit :pixels) cells) 0 (bit-shift-left 1 26))))

(defn wrap-with-solids [cells]
  (let [hull-cells (into [] ((comp :hull :pixels) cells))
        all-cells (into [] (reduce #(concat %1 %2) (vals (:pixels cells))))
        checker (fn [result x y]
                  (cond (>= x (/ ship-size 2)) (recur result 0 (inc y))
                        (>= y ship-size) (assoc-in cells [:pixels :solid] (into [] (distinct result)))
                        (and (or (.contains hull-cells {:x x :y (inc y)} )
                                 (.contains hull-cells {:x x :y (dec y)} )
                                 (.contains hull-cells {:x (inc x) :y y} )
                                 (.contains hull-cells {:x (dec x) :y y} ))
                             (not (.contains all-cells {:x x :y y})))
                          (recur (conj result {:x x :y y}) (inc x) y)
                        :else (recur result (inc x) y)))]
    (checker ((comp :solid :pixels) cells) 0 0)))

(defn mirror-ship [cells]
  (let [pixels (:pixels cells)
        mirror-list (fn [m]
                      (into [] (concat m (map (fn[m] {:x (- (dec ship-size) (:x m)) :y (:y m)}) m))))]
    (assoc cells :pixels (-> (assoc pixels :hull (mirror-list (:hull pixels )))
                             (assoc :cockpit (mirror-list (:cockpit pixels )))
                             (assoc :solid (mirror-list (:solid pixels )))))))

(defn generate-pixel-ship [seed]
  (-> (assoc-in {} [:pixels :solid] always-solid)
      (assoc :seed seed)
      (add-hull)
      (add-cockpit)
      (wrap-with-solids)
      (mirror-ship)
      ))

(def ship (generate-pixel-ship Integer/MAX_VALUE))
(count ((comp :solid :pixels) ship))
ship
