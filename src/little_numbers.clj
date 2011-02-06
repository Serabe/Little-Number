(ns little-numbers)

(defn to4 [x]
  (let [s (count x)
        a (- 4 s)]
    (str (apply str (repeat a "0")) x)))

(defn has-repeated-cipher?
  [x]
  (not (= 4 (count (set x)))))

(defn- filter-fn
  [x]
  (filter #(not (has-repeated-cipher? %)) x))

(def *seq* (ref (filter-fn (map #(to4 (str %)) (range 123 9999)))))

(defn get-dead
  [number guess]
  (loop [[n & ns] number
         [g & gs] guess
         acc      0]
    (if (empty? ns)
      (if (= n g) (inc acc) acc)
      (recur ns gs (if (= n g) (inc acc) acc)))))

(defn- get-hurt
  ([number guess]
     (count (filter #(some (fn [x] (= x %)) number) guess)))
  ([number guess dead]
     (- (get-hurt number guess) dead)))

(defn get-score
  [number guess]
  (let [dead (get-dead number guess)]
    {:dead dead
     :hurt (get-hurt number guess dead)}))

(defn satisfy-guess
  [number guess score]
  (let [sc (get-score guess number)
        dn (:dead sc)
        hn (:hurt sc)
        dg (:dead score)
        hg (:hurt score)]
    (and (= dn dg)
         (= hn hg))))

(defn filter-guess
  [guess score]
  (fn [x]
    (filter #(satisfy-guess % guess score) x)))

(defn add-guess
  ([guess score]
     (dosync
      (alter *seq* (filter-guess guess score))
      (first @*seq*)))
  ([guess dead hurt]
     (add-guess guess {:dead dead :hurt hurt})))
