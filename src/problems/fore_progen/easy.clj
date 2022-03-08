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

(defn least-common-multiple [xs]
  (let [x    (apply max xs)
        item (remove #{x} xs)]
    (loop [ref x]
      (if (= (mapv #(rem ref %) item)
             (mapv #(* 0 %) item))
        ref
        (recur (+ ref x))))))