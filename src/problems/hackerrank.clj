(ns problems.hackerrank)

;; === SIMPLE ARRAY SUM ===
;; input [1 2 3] -> 6
(defn simpleArraySum
  [ar]
  (reduce + ar))

;; === COMPARE THE TRIPLETS ===
;; input [1 2 3] & [3 2 1] -> [1 1]
;; input [100 89 1] & [23 82 40] -> [2 1]

(defn compareTriplets
  [a b]
  (let [mix (map #(apply compare %) (partition 2 (interleave a b)))]
    [(if-let [nums (filter pos? mix)] (count nums) 0)
     (if-let [nums (filter neg? mix)] (count nums) 0)]))

;; === A VERY BIG SUM ===
(defn aVeryBigSum
  [ar]
  (reduce +' ar))

;; === DIAGONAL DIFFERENCE ===
;; input n and xs [3 1 2 3 4 5 6 7 8 9]
;; -> 3 means 3x3 matrix
;; 1 2 3
;; 4 5 6
;; 9 8 9 -> out |(1+5+9) - (3+5+9)| = 2 (absolute value)

(defn diag
  [a]
  (->> (loop [lst [] xs a n 0]
         (if (empty? xs)
           lst
           (recur (conj lst ((first xs) n)) (rest xs) (inc n))))
       (reduce +')))

(defn diagonalDifference
  [ar]
  (let [[x & xs] ar
        mtx      (map vec (partition x xs))
        res      (- (diag mtx) (diag (map vec (map reverse mtx))))]
    (if (neg? res) (* res -1) res)))

;; ==
(defn line [i, j, n, string]
  (if (< (+ i j) (- n 1))
    (str string " ")
    (str string "#")))

(defn row [i, j, n, string]
  (if (>= j (- n 1))
    (str (line i j n string) (System/lineSeparator))
    (row i (+ j 1) n (line i j n string))))

(defn column [i, n, string]
  (if (>= i n)
    string
    (column (+ i 1) n (row i 0 n string))))

(defn staircase [n]
  (column 0 n ""))

;; === STAIRCASE ===

;; === MINI-MAX SUM ===
;; input xs [1 2 3 4 5], generate xs to 5 other vector
;; remove every possible index on the new vector
;; -> [[2 3 4 5] [1 3 4 5] [1 2 4 5] [1 2 3 5] [1 2 3 4]]
;; then find max and min after sum all the vector
(defn miniMaxArr
  [a]
  (for [x (range 5)]
    (let [xs a]
      (- (reduce + xs) (get xs x)))))

(defn miniMaxSum
  [arr]
  (let [sum (miniMaxArr arr)]
    [(apply min sum) (apply max sum)]))

;; === BIRTHDAY CAKE CANDLES ===
;; count max numbers inside a vector
(defn birthdayCakeCandles
  [candles]
  (let [maxv (apply max candles)]
    (count (filter #(= % maxv) candles))))

;; === TIME CONVERSIONS ===
;; convert 12-hour time format into 24-hour time format
(defn timeConversions
  [s]
  (let [ori  (vec (re-seq #"[0-9]+|[A-Z]+" s))
        hour (Integer/parseInt (ori 0))]
    (clojure.string/join ":"
                         (vector
                           (cond
                             (and (= (ori 3) "AM") (= hour 12)) "00"
                             (and (= (ori 3) "PM") (< hour 12)) (+ hour 12)
                             :else (if (< hour 10) (str "0" hour) hour))
                           (ori 1)
                           (ori 2)))))

;; === GRADING STUDENTS ===
;; counting a difference between grade and the next multiple of 5
;; if the difference value is less than 3, than round the grade to the next multiple of 5
;; if the value of grade is less than 38, no rounding occurs
(defn gradingStudents
  [grades]
  (map (fn [grades]
         (let [fold (loop [a grades]
                      (if (zero? (rem a 5))
                        a
                        (recur (inc a))))]
           (cond
             (< grades 38)          grades
             (<  (- fold grades) 3) fold
             (>= (- fold grades) 3) grades)))
       grades))

;; === APPLES AND ORANGES ===
;; given a home range of s to t, tree position at a and b
;; each tree drop a fruit in a certain range
;; count a fruit while drop inside a home range
(defn countApplesAndOranges
  [s t a b apples oranges]
  (let [diff  (fn [x xs] (map #(+ x %) xs))
        diffa (diff a apples)
        diffb (diff b oranges)
        home  (fn [val] (if (and (>= val s) (<= val t)) 1 -1))
        res   [(count (filter pos? (map home diffa)))
               (count (filter pos? (map home diffb)))]]
    (run! println res)))

;; === PLUS MINUS ===
;; count total of positive, negative and zero value in array
;; find the rasio of each value with 6 place after decimal
(defn plusMinus
  [arr]
  (->> (let [form     (fn [x] (format "%.6f" (float x)))
             filt     (fn [f] (count (filter f arr)))
             tot-arr  (count arr)
             tot-pos  (filt pos?)
             tot-neg  (filt neg?)
             tot-zero (filt zero?)]
         [(form (/ tot-pos tot-arr))
          (form (/ tot-neg tot-arr))
          (form (/ tot-zero tot-arr))])
       (run! println)))

;; === NUMBER LINE JUMPS ===
;; x1 x2 starting position, v1 v2 jump distance
;; if x1 could meet at a same location with x2 retun YES, otherwise NO
(defn kangaroo
  [x1 v1 x2 v2]
  (loop [xa x1 xb x2 n 1]
    (cond
      (= xa xb) "YES"
      (= n 10000) "NO"
      :else (recur (+ xa v1) (+ xb v2) (inc n)))))

;; === BREAKING THE RECORDS ===
;; stored all score in 2 arrays, 1 for min score, 1 for max score
;; point + 1 for a if max scored change, +1 for b if min scored change
(defn breakingRecords
  [scores]
  (let [highest (atom [(first scores)])
        lowest  (atom [(first scores)])]
    (loop [a   0
           b   0
           lst scores]
      (if (empty? lst)
        [a b]
        (recur (if (> (first lst) (last @highest))
                 (do (swap! highest conj (first lst))     (inc a))
                 (do (swap! highest conj (last @highest)) (+ a 0)))
               (if (< (first lst) (last @lowest))
                 (do (swap! lowest  conj (first lst))     (inc b))
                 (do (swap! lowest  conj (last @lowest))  (+ b 0)))
               (rest lst))))))

;; === DIVISIBLE SUM PAIRS ===
;; giver array (ar) and n for total integer in array ar
;; make all possible pair and then sum the pair
;; count if total sum are divisible with the given var k
(defn divisibleSumPairs
  [n k ar]
  (->> (let [items (map #(vector % %2) (range n) ar)]
         (for [a items
               b items
               :when (< (first b) (first a))]
           [(last a) (last b)]))
       (map #(apply + %))
       (filter #(zero? (mod % k)))
       count))

;; === MIGRATORY BIRDS ===
;;
(defn migratoryBirds
  [arr]
  (let [xs (last (sort (group-by last (frequencies arr))))]
    (get-in xs [1 0 0])))

;; === BILL DIVISION ===
;; bill contains a total bill vector (2 people, Anna and Elsa)
;; k is an index which Anna didnt eat
;; b is the amount of money that Elsa charged Anna for her share of the bill
(defn bonAppetite
  [bill k b]
  (let [all  (map-indexed #(vector % %2) bill)
        Eats (remove #(= k (first %)) all)
        totl (/ (reduce +' (map last Eats)) 2)]
    (if (zero? (- b totl))
      "Bon Appetit"
      (- b totl))))

;; === SALES BY MATCH ===
;; given an array of integer ar
;; find total pair with the same integer
(defn sockMerchant
  [n ar]
  (let [totl (frequencies ar)
        pair (vals totl)]
    (reduce +' (map #(quot % 2) pair))))

;; === DRAWING BOOK ===
;; find minimum number of turning book pages into specific page (from the front of the book / back of the book)
;; page 1 always on the right side of the book
;; ex: [ [1] [2 3] [4 5] [6 7] [8] ]
(defn iter1
  [a b]
  (loop [init (first a) xs (rest a) n 0]
    (if (some #(= % b) init)
      n
      (recur (first xs) (rest xs) (inc n)))))

(defn pageCount
  [n p]
  (let [xs (concat ['(1)] (partition-all 2 (range 2 (inc n))))]
    (min (iter1 xs p) (iter1 (reverse xs) p))))

;; === CAMEL CASE ===
;; given an input s of camel case string format
;; count total word of s
;; ex: saveChangesInTheEditor ==> 5

(defn camelcase
  [s]
  (inc (count (re-seq #"[A-Z]" s))))

;; === STRONG PASSWORD ===
;; given input n and password
;; at least password have a length of 6 and contains 1 upper, normal, number, and special character

(defn minimumNumber
  [n password]
  (if (<= n 3)
    (- 6 n)
    (let [char1  (set "abcdefghijklmnopqrstuvwxyz")
          char2  (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
          number (set "0123456789")
          sym    (set "!@#$%^&*()-+")]
      (->> (map #(some % password) [char1 char2 number sym])
           (remove nil?)
           (count)
           (- 4)
           (max (- 6 n))))))

;; fn to rotate alphabet
(defn rotate-alp [x n]
  (let [alp  (map str "abcdefghijklmnopqrstuvwxyz")
        part (partition-by #(= x %) alp)
        comb (vec (concat (list x) (last part) (first part)))]
    (get comb (cond
                (zero? (rem n 26)) 0
                (< n 26)           n
                :else (rem n 26)))))

;;
;; soal traveloka
(defn loop-decrement [xs]
  (->> (let [new-xs (vec (conj (reverse (into '() xs)) 0))]
         (loop [res [] xs new-xs]
           (if (= 1 (count xs))
             res
             (recur (conj res (vector (first xs) (second xs)))
                    (rest xs)))))
       (mapv #(* -1 (apply - %)))))

(defn slowest-key-press [key-times]
  (let [alp                    (mapv str "abcdefghijklmnopqrstuvwxyz")
        key-press              (mapv first key-times)
        times                  (mapv second key-times)
        time-interval          (loop-decrement times)
        largest-interval-index (->> time-interval
                                    (map-indexed vector)
                                    (sort-by #(second %))
                                    (last)
                                    (first))
        key-press-alp          (->> largest-interval-index
                                    (get key-press)
                                    (get alp))]
    key-press-alp))
