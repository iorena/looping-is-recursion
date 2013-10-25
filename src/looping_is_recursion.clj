(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [x n]
                 (if (zero? n)
                   x
                  (recur (* x base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
 (let [helper (fn [a-seq acc]
                (if (<= acc 1)
                 (first a-seq)
                 (recur (rest a-seq) (dec acc))))]
   (helper a-seq (count a-seq))))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2))
    true
   (or (empty? seq1) (empty? seq2))
    false
   (= (first seq1) (first seq2))
    (recur (rest seq1) (rest seq2))
   :else
    false))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         seq a-seq]
    (cond
     (empty? seq)
      nil
     (pred (first seq))
      acc
     :else
      (let [new-acc (+ acc 1)]
        (recur new-acc (rest seq))))))

(defn avg [a-seq]
  (loop [acc 0
         sum 0
         seq a-seq]
    (if (empty? seq)
      (/ sum acc)
      (let [nacc (+ acc 1)
            nsum (+ sum (first seq))
            nseq (rest seq)]
        (recur nacc nsum nseq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [loop-seq a-seq
         res-set #{}]
    (if (empty? loop-seq)
      res-set
      (recur (rest loop-seq) (toggle res-set (first loop-seq))))))

(defn fast-fibo [n]
  (if (<= n 1)
    n
    (loop [acc 1
           f-n 1
           f-n-1 0]
      (if (== acc n)
        f-n
        (recur (inc acc) (+ f-n f-n-1) f-n)))))

(defn cut-at-repetition [a-seq]
  (loop [b-seq '()
         n-seq a-seq]
    (cond
     (empty? n-seq)
      (reverse b-seq)
     (some #(= (first n-seq) %) b-seq)
      (reverse b-seq)
     :else
      (recur (conj b-seq (first n-seq)) (rest n-seq)))))




