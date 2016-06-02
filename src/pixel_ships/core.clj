(ns pixel-ships.core)

(def ship-size 12)
(def always-solid {:pixels {:solid '([5 2] [5 3] [5 4] [5 5] [5 9])}})

(def hull-possibles '([4 1] [5 1] [4 2] [3 3] [4 3] [3 4] [4 4] [2 5] [3 5] [4 5] [1 6] [2 6] [3 6] [1 7] [2 7] [3 7] [1 8] [2 8] [3 8] [1 9] [2 9] [3 9] [4 9] [3 10] [4 10] [5 10]))

(def cockpit-possibles '([4 6] [5 6] [4 7] [5 7] [4 8] [5 8]))

(defn add-hull [cells]
  (let [seed (:seed cells)
        hull-count (count hull-possibles)
        checker (fn [result i mask]
                   (cond (>= i hull-count) result
                         (not= 0 (bit-and seed mask))
                           ;(recur (conj result (vector (nth hull-cols i) (nth hull-rows i))) (inc i) (bit-shift-left mask 1))
                           (recur (conj result (vector (first (nth hull-possibles i)) (second (nth hull-possibles i)))) (inc i) (bit-shift-left mask 1))
                         :else (recur result (inc i) (bit-shift-left mask 1))))]
    (assoc-in cells [:pixels :hull] (checker () 0 1))))

(defn add-cockpit [cells]
  (let [seed (:seed cells)
        cockpit-count (count cockpit-possibles)
        checker (fn [solid-cells cockpit-cells i mask]
                   (cond (>= i cockpit-count)
                           (let [pixels (:pixels cells)]
                                 (assoc cells :pixels (merge pixels (assoc {} :cockpit cockpit-cells) (assoc {} :solid solid-cells))))
                         (not= 0 (bit-and seed mask))
                           (recur (conj solid-cells (vector (first (nth cockpit-possibles i)) (second (nth cockpit-possibles i)))) cockpit-cells (inc i) (bit-shift-left mask 1))
                         :else (recur solid-cells (conj cockpit-cells (vector (first (nth cockpit-possibles i)) (second (nth cockpit-possibles i)))) (inc i) (bit-shift-left mask 1))))]
    (checker ((comp :solid :pixels) cells) ((comp :cockpit :pixels) cells) 0 (bit-shift-left 1 26))))

(defn wrap-with-solids [cells]
  (let [hull-cells (into [] ((comp :hull :pixels) cells))
        all-cells (into [] (reduce #(concat %1 %2) (vals (:pixels cells))))
        checker (fn [result x y]
                  (cond (>= x (/ ship-size 2)) (recur result 0 (inc y))
                        (>= y ship-size) (assoc-in cells [:pixels :solid] (distinct result))
                        (and (or (.contains hull-cells (vector x (inc y) ))
                                 (.contains hull-cells (vector x (dec y) ))
                                 (.contains hull-cells (vector (inc x) y ))
                                 (.contains hull-cells (vector (dec x) y )))
                             (not (.contains all-cells (vector x y))))
                          (recur (conj result (vector x y)) (inc x) y)
                        :else (recur result (inc x) y)))]
    (checker ((comp :solid :pixels) cells) 0 0)))

(defn mirror-ship [cells]
  (let [pixels (:pixels cells)
        mirror-list (fn [m]
                      (concat m (map (fn[[a b]] (vector (- (dec ship-size) a) b)) m)))]
    (assoc cells :pixels (-> (assoc pixels :hull (mirror-list (:hull pixels )))
                             (assoc :cockpit (mirror-list (:cockpit pixels )))
                             (assoc :solid (mirror-list (:solid pixels )))))))

(defn generate-pixel-ship [seed]
  (-> (assoc always-solid :seed seed)
      (add-hull)
      (add-cockpit)
      (wrap-with-solids)
      (mirror-ship)
      ))

(def ship (generate-pixel-ship Integer/MAX_VALUE))
(count ((comp :solid :pixels) ship))
ship

