(ns pixel-ships.bollinger)

(def model
  {:name :bollinger
   :seed Integer/MAX_VALUE
   :ship-size 12
   :model {:solid [{:x 5, :y 2} {:x 5, :y 3} {:x 5, :y 4} {:x 5, :y 5} {:x 5, :y 9}],
           :cockpit [{:x 4, :y 6} {:x 5, :y 6} {:x 4, :y 7} {:x 5, :y 7} {:x 4, :y 8} {:x 5, :y 8}],
           :hull [{:x 4, :y 1} {:x 5, :y 1} {:x 4, :y 2} {:x 3, :y 3} {:x 4, :y 3} {:x 3, :y 4} {:x 4, :y 4} {:x 2, :y 5} {:x 3, :y 5}
                  {:x 4, :y 5} {:x 1, :y 6} {:x 2, :y 6} {:x 3, :y 6} {:x 1, :y 7} {:x 2, :y 7} {:x 3, :y 7} {:x 1, :y 8} {:x 2, :y 8}
                  {:x 3, :y 8} {:x 1, :y 9} {:x 2, :y 9} {:x 3, :y 9} {:x 4, :y 9} {:x 3, :y 10} {:x 4, :y 10} {:x 5, :y 10}]}})


(def color-scheme
  {:name :bollinger
   :sat-mid 0.5
   :sat-delta 0.12
   :sat-multipliers '[ -2 -1  0 1 0 -1 0 1 2 1 0 -1]
   :bright-mid 0.5
   :bright-delta 0.12
   :bright-multipliers '[-3 -2 -1 0 1  2 3 3 2 1 0 -1 -2 -3]
   :solid-color {:color {:h 0.0 :s 0.0 :v 0.0}}
   :hues-fn (fn[seed]
              (vector (/ (bit-and seed 0xff) 255.0)
                      (/ (bit-shift-right (bit-and seed 0xff00) 8) 255.0)
                      (/ (bit-shift-right (bit-and seed 0xff0000) 16) 255.0)
                      (/ (bit-shift-right (bit-and seed 0xff000000) 24) 255.0))
              )})

(defn color-from-coords [scheme seed x y tag]
  (let [hues ((:hues-fn scheme) seed)
        saturations (map  (fn [x] (+ (* x (:sat-delta scheme)) (:sat-mid scheme))) (:sat-multipliers scheme))
        brightnesses (map  (fn [x] (+ (* x (:bright-delta scheme)) (:bright-mid scheme))) (:bright-multipliers scheme))
        v-mod (if (= tag :cockpit) 0.1 0)
        h (cond (= tag :cockpit) (first hues)
                (< y 6) (nth hues 1)
                (< y 9) (nth hues 2)
                :else (nth hues 3))
        s (nth saturations y)
        v (+ (nth brightnesses x) v-mod)]
    (if (= tag :solid) (:solid-color scheme)
    {:color {:h h :s s :v v}})))
