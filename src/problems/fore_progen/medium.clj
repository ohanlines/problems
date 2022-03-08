(ns problems.fore-progen.medium)

(defn happy-numbers [x]
  (let [f1 (fn [a] (* a a))
        f2 (fn [s] (map read-string (re-seq #"\d" (str s))))]
    (loop [res x]
      (cond
        (= res 1) true
        (<= x 3)  false
        :else     (recur (reduce + (map f1 (f2 res))))))))