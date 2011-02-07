(ns little-numbers)

(defn to4 [x]
  (let [s (str x)]
    (str (apply str (repeat (- 4 (count s)) "0")) s)))

(defn has-repeated-cipher?
  [x]
  (not (= 4 (count (set x)))))

(defn- filter-fn
  [x]
  (filter #(not (has-repeated-cipher? %)) x))

(def possible-values (filter #(not (has-repeated-cipher? %)) (map to4 (range 123 9877))))

(def *seq* (ref []))

(defn reset-game []
  (dosync
   (ref-set *seq* [])))

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
  [number guess {dg :dead hg :hurt}]
  (let [{dn :dead hn :hurt} (get-score guess number)]
    (and (= dn dg)
         (= hn hg))))

(defn next-guess []
  (if (empty? @*seq*)
    {:guess "0123" :count 5040}
    (loop [[{guess :guess score :score} & xs] @*seq*
           possible (filter #(satisfy-guess % guess score) possible-values)]
      (let [filtered (filter #(satisfy-guess % guess score) possible)]
        (if (empty? xs)
          {:guess (first filtered) :count (count filtered)}
          (recur xs filtered))))))

(defn add-guess-to-ref
  [guess score]
  (fn [last-val]
    (conj last-val {:guess guess
                    :score score})))

(defn add-guess
  ([guess score]
     (dosync
      (alter *seq* conj {:guess guess :score score})
      (next-guess)))
  ([guess dead hurt]
     (add-guess guess {:dead dead :hurt hurt})))

(defn guesses-needed-for [number]
  (reset-game)
  (loop [{guess :guess count :count} (next-guess)
         acc   1]
    (if (= 1 count)
      acc
      (recur (add-guess guess (get-score number guess)) (inc acc)))))
