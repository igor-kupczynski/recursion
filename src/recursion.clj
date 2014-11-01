(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and
   (not (empty? coll))
   (empty? (rest coll))))

(defn sing-or-empty? [coll]
  (or (empty? coll) (singleton? coll)))

(defn my-last [coll]
  (if (sing-or-empty? coll)
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (sing-or-empty? a-seq)
    (first a-seq)
    (max (first a-seq)
         (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) '()
      (if (pred? (first a-seq))
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        :else '()))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        :else (and (= (first a-seq) (first b-seq))
                   (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (let [first-1 (first seq-1)
        first-2 (first seq-2)]
    (if (and (not (nil? first-1)) (not (nil? first-2)))
      (cons (f first-1 first-2) (my-map f (rest seq-1) (rest seq-2)))
      '())))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) '()
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0) '()
      (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) (cons '() '())
      (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn inn-rotations [left right]
  (if (empty? left)
    '()
    (cons
     (concat left right)
     (inn-rotations (rest left) (concat right [(first left)])))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (inn-rotations a-seq '())))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper
     (update-in freqs [(first a-seq)] (fnil inc 0))
     (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[token times] (first a-map)]
      (concat
       (repeat times token)
       (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons
     (first coll)
     (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [pivot (int  (/ (count a-seq) 2))]
    [(my-take pivot a-seq) (my-drop pivot a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond (and (nil? a) (nil? b)) '()
          (nil? a) b-seq
          (nil? b) a-seq
          (< a b) (cons a (seq-merge (rest a-seq) b-seq))
          :else (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (cond (empty? a-seq) '()
        (singleton? a-seq) a-seq
        :else (let [[a b] (halve a-seq)]
                (seq-merge
                 (merge-sort a)
                 (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

