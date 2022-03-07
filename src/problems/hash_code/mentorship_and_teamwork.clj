(ns problems.hash-code.mentorship-and-teamwork)

;; belom selese

(def all-data
  (let [read-data (fn [dir]
                    (->> (read-string (str \" (slurp (str "resources/mentorship-and-teamwork/" dir ".in.txt")) \"))
                         cs/split-lines
                         (#(vector (mapv read-string (cs/split (first %) #" "))
                                   (vec (rest %))))))]
    {:a (read-data "a_an_example")
     :b (read-data "b_better_start_small")
     :c (read-data "c_collaboration")
     :d (read-data "d_dense_schedule")
     :e (read-data "e_exceptional_skills")
     :f (read-data "f_find_great_mentors")}))

(defn contrib-pairing [data]
  (let [a     (get-in data [0 0])
        b     (get-in data [0 1])
        xs    (get data 1)
        takes (fn [s] (let [input  (first s)
                            items  (rest s)
                            split  (clojure.string/split input #" ")
                            names  (butlast split)
                            skills (read-string (last split))]
                        [names (take skills items) (inc skills)]))]
    (loop [res [] lst xs]
      (if (empty? lst)
        res
        (recur (conj res (butlast (takes lst)))
               (drop (last (takes lst)) lst))))))