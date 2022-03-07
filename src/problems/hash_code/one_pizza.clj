(ns problems.hash-code.one-pizza
  (:require [clojure.string :as cs]))

;; ========== ONE PIZZA ==========

(def data-one-pizza
  (let [read-data (fn [dir]
                    (->> (read-string (str \" (slurp (str "resources/one-pizza/" dir ".txt")) \"))
                         cs/split-lines
                         rest
                         (map #(re-seq #"\w+" %))
                         (mapv #(vec (drop 1 %)))))]
    {:a (read-data "a_an_example.in")
     :b (read-data "b_basic.in")
     :c (read-data "c_coarse.in")
     :d (read-data "d_difficult.in")
     :e (read-data "e_elaborate.in")}))

(defn split-data
  "Function to split even or odd vector indexes"
  [data flag]
  (->> (for [i (range (count data))]
         (let [split-data (if (flag i) (get data i) [])]
           split-data))
       flatten))

(def new-data
  (->> (for [type (keys data-one-pizza)]
         (let [even (split-data (type data-one-pizza) even?)
               odd  (split-data (type data-one-pizza) odd?)]
           [even odd]))
       (map (fn [xs] (apply #(distinct (remove (set %2) (vec %1))) xs)))
       (map #(vector (count %) %))
       (map flatten)
       (mapv vec)
       (map #(cs/join " " %))
       (zipmap [:a :b :c :d :e])))

;; inline execute
(comment
  (spit "output/one-pizza/a_an_exmple.out.txt" (:a new-data))
  (spit "output/one-pizza/b_basic.out.txt" (:b new-data))
  (spit "output/one-pizza/c_coarse.out.txt" (:c new-data))
  (spit "output/one-pizza/d_difficult.out.txt" (:d new-data))
  (spit "output/one-pizza/e_elaborate.out.txt" (:e new-data))
  )