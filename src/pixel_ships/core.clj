(ns pixel-ships.core)

(def n 12)
(def pixels-per-cell 3)
(def always-solid {:pixels {:solid '([5 2] [5 3] [5 4] [5 5] [5 9])}})

(def hull-cols '(4 5 4 3 4 3 4 2 3 4 1 2 3 1 2 3 1 2 3 1 2 3 4  3  4  5))
(def hull-rows '(1 1 2 3 3 4 4 5 5 5 6 6 6 7 7 7 8 8 8 9 9 9 9 10 10 10))
(def hull-count (count hull-cols))
(def hull (map list hull-cols hull-rows))

(def cockpit-cols '(4 5 4 5 4 5))
(def cockpit-rows '(6 6 7 7 8 8))
(def cockpit (map list cockpit-cols cockpit-rows))
(def cockpit-count (count cockpit-cols))


(defn add-hull [cells]
  (let [seed (:seed cells)
        checker (fn [result i mask]
                   (cond (>= i hull-count) result
                         (not= 0 (bit-and seed mask))
                           (recur (conj result (vector (nth hull-cols i) (nth hull-rows i))) (inc i) (bit-shift-left mask 1))
                         :else (recur result (inc i) (bit-shift-left mask 1))))]
    (assoc-in cells [:pixels :hull] (checker () 0 1))))

(defn add-cockpit [cells]
  (let [seed (:seed cells)
        checker (fn [solid-cells cockpit-cells i mask]
                   (cond (>= i cockpit-count)
                           (let [pixels (:pixels cells)]
                                 (assoc cells :pixels (merge pixels (assoc {} :cockpit cockpit-cells) (assoc {} :solid solid-cells))))
                         (not= 0 (bit-and seed mask))
                           (recur (conj solid-cells (vector (nth cockpit-cols i) (nth cockpit-rows i))) cockpit-cells (inc i) (bit-shift-left mask 1))
                         :else (recur solid-cells (conj cockpit-cells (vector (nth cockpit-cols i) (nth cockpit-rows i))) (inc i) (bit-shift-left mask 1))))]
    (checker ((comp :solid :pixels) cells) ((comp :cockpit :pixels) cells) 0 (bit-shift-left 1 26))))

(defn wrap-with-solids [cells]
  (let [hull-cells (into [] ((comp :hull :pixels) cells))
        all-cells (into [] (reduce #(concat %1 %2) (vals (:pixels cells))))
        checker (fn [result x y]
                  (cond (>= x (/ n 2)) (recur result 0 (inc y))
                        (>= y n) (assoc-in cells [:pixels :solid] (distinct result))
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
                      (concat m (map (fn[[a b]] (vector (- (dec n) a) b)) m)))]
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


