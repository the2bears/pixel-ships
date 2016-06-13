(ns pixel-ships.core
  (:require [pixel-ships.bollinger :as bollinger]))

(defn add-hull [cells]
  (let [seed (:seed cells)
        hull-possibles ((comp :hull :model) cells)
        hull-count (count hull-possibles)
        checker (fn [result i mask]
                   (cond (>= i hull-count) result
                         (not= 0 (bit-and seed mask))
                           (recur (conj result (nth hull-possibles i)) (inc i) (bit-shift-left mask 1))
                         :else (recur result (inc i) (bit-shift-left mask 1))))]
    (assoc-in cells [:pixels :hull] (checker [] 0 1))))

(defn add-cockpit [cells]
  (let [seed (:seed cells)
        cockpit-possibles ((comp :cockpit :model) cells)
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
        ship-size (:ship-size cells)
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
        ship-size (:ship-size cells)
        mirror-list (fn [m]
                      (into [] (concat m (map (fn[m] {:x (- (dec ship-size) (:x m)) :y (:y m)}) m))))]
    (assoc cells :pixels (-> (assoc pixels :hull (mirror-list (:hull pixels )))
                             (assoc :cockpit (mirror-list (:cockpit pixels )))
                             (assoc :solid (mirror-list (:solid pixels )))))))

(defn create-pixel-ship [model]
  (-> model
      (assoc-in [:pixels :solid] ((comp :solid :model) model))
      (add-hull)
      (add-cockpit)
      (wrap-with-solids)
      (mirror-ship)
      ))

(defn color-pixel-ship [pixel-ship]
  (let [color-tag (fn[ship tag]
                    (assoc-in ship [:pixels tag]
                              (into [] (map (fn[{:keys [x y]}] {:x x :y y  :color (bollinger/color-from-coords bollinger/color-scheme (:seed pixel-ship) x y tag)})
                                            ((comp tag :pixels) ship)))))]
    (reduce #(color-tag %1 %2) pixel-ship (keys (:pixels pixel-ship)))))
