(ns problems.fore-progen.easy)

(defn sum-of-square-of-digits [xs]
  (count
   (filter true? (keep
                  (fn [x]
                    (->> (let [kuadrat (fn [a] (* a a))
                               xx      (map read-string (re-seq #"\d" (str x)))]
                           (reduce + (map kuadrat xx)))
                         (< x)))
                  xs))))