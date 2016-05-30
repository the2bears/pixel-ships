(ns pixel-ships.core)

(declare color-from-coords)

(def n 12)
(def pixels-per-cell 3)
(def always-solid '((5 2) (5 3) (5 4) (5 5) (5 9)))
(def always-solid-tagged (map #(list (first %) (second %) :solid) always-solid))

(def hull-cols '(4 5 4 3 4 3 4 2 3 4 1 2 3 1 2 3 1 2 3 1 2 3 4  3  4  5))
(def hull-rows '(1 1 2 3 3 4 4 5 5 5 6 6 6 7 7 7 8 8 8 9 9 9 9 10 10 10))
(def hull-count (count hull-cols))
(def hull (map list hull-cols hull-rows))
(defn index-of-hull [x y]
  (.indexOf hull [x y]))

(defn is-hull? [x y]
  (if (< (index-of-hull x y) 0) false
    true))



(def cockpit-cols '(4 5 4 5 4 5))
(def cockpit-rows '(6 6 7 7 8 8))
(def cockpit (map list cockpit-cols cockpit-rows))
(def cockpit-count (count cockpit-cols))

(defn add-hull [cells seed]
  (letfn [(checker [result i mask]
                   (cond (>= i hull-count) result
                         (not= 0 (bit-and seed mask))
                           (recur (conj result (list (nth hull-cols i) (nth hull-rows i) :hull)) (inc i) (bit-shift-left mask 1))
                         :else (recur result (inc i) (bit-shift-left mask 1))))]
    (checker cells 0 1)))

(defn add-cockpit [cells seed]
  (let [checker (fn [result i mask]
                   (cond (>= i cockpit-count) result
                         (not= 0 (bit-and seed mask))
                           (recur (conj result (list (nth cockpit-cols i) (nth cockpit-rows i) :solid)) (inc i) (bit-shift-left mask 1))
                         :else (recur (conj result (list (nth cockpit-cols i) (nth cockpit-rows i) :cockpit)) (inc i) (bit-shift-left mask 1))))]
    (checker cells 0 (bit-shift-left 1 26))))

(defn wrap-with-solids [cells]
  (let [cells-no-tag (map #(list (first %) (second %)) cells)
        checker (fn [result x y]
                  (cond (>= x (/ n 2)) (recur result 0 (inc y))
                        (>= y n) result
                        (and (or (.contains cells (list x (inc y) :hull))
                                 (.contains cells (list x (dec y) :hull))
                                 (.contains cells (list (inc x) y :hull))
                                 (.contains cells (list (dec x) y :hull)))
                             (not (.contains cells-no-tag (list x y))))
                          (recur (conj result (list x y :solid)) (inc x) y)
                        :else (recur result (inc x) y)))]
    (checker cells 0 0)))

(defn mirror-ship [cells]
  (distinct (concat (map #(list (nth % 0)  (nth % 1) (nth % 2)) cells) (map #(list (- (dec n) (nth % 0)) (nth % 1) (nth % 2)) cells))))

(defn generate-pixel-ship [seed]
  (-> (add-hull always-solid-tagged seed)
      (add-cockpit seed)
      (wrap-with-solids)
      (mirror-ship)))

(generate-pixel-ship 198234)
