(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp orig]
                 (if (= 1 exp)
                   base
                  (recur (* base orig) (dec exp) base)))]
    (if(= 0 exp)
    1
    (helper base exp base))))

(defn last-element [a-seq]
  (let [helper (fn [a-seq]
                 (if (empty? (rest a-seq))
                   (first a-seq)
                  (recur (rest a-seq))))]

    (helper a-seq)))

(defn seq2= [a-seq b-seq]
     ;(not (and (
    (not(and (not-any? nil? a-seq) (not-any? nil? b-seq))))

(defn seq= [seq1 seq2]

   (let [helper (fn [seq1 seq2]
                  (cond
                  (not(and (not-any? nil? seq1) (not-any? nil? seq2))) false
                  (and (empty? seq1) (empty? seq2)) true
                   (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
                   :else false
                   ))]

    (helper seq1 seq2)))


(defn find-first-index [pred a-seq]
  (loop [lkm 0
         n-seq a-seq
         pred? pred]
   (if (empty? n-seq)
     nil
    (if (pred? (first n-seq))
      lkm
      (recur (inc lkm) (rest n-seq) pred?)))))


(defn avg [a-seq]
  (loop [acc 0
         sum 0
         n-seq a-seq]
    (if (empty? n-seq)
      (/ sum acc)
      (recur (inc acc) (+ sum (first n-seq))(rest n-seq)))))

  (defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)
  )
  )

  (defn loopy-factorial [down-from]
  (loop [acc 1
         n down-from]
    (if (zero? n)
      acc
      (recur (* acc n) (dec n)))))



  (defn new-toggle [a-set n-seq]
 (if (contains? a-set (first n-seq))
                 (disj a-set (first n-seq))
                 (conj a-set (first n-seq))
               )
  )

  (defn toggle-test [n-seq]
     (new-toggle #{} n-seq)
    )


(defn parity [a-seq]
  (loop [ a-set #{}
         n-seq a-seq]
    (if (empty? n-seq)
      a-set
      (recur (new-toggle a-set n-seq)(rest n-seq)))))

(defn fib [n]
  (cond
  (= n 0 ) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))
  ))


(defn fast-fibo [n]
  (loop [f-n-1 -1
         f-n 0
         nn n
         sum 0]
    (if (= n sum)
      (* -1 f-n)
      (recur f-n (+ f-n-1 f-n) nn (+ sum 1)))))

(defn testi [a-seq]
  (loop [a-set #{}
         n-seq []
         t-seq a-seq]
    (if (or (> (count n-seq) (count a-set))(empty? t-seq))
        (rest (seq n-seq))
        (recur (conj a-set (first t-seq)) (conj n-seq (first t-seq))(rest t-seq))
      )))



(defn cut-at-repetition [a-seq]
  (loop [a-set #{}
         n-seq []
         t-seq a-seq]
    (if (> (count n-seq) (count a-set))
            (reverse(rest(reverse n-seq)))

      (if(empty? t-seq)
        (seq n-seq)
        (recur (conj a-set (first t-seq)) (conj n-seq (first t-seq))(rest t-seq))
      ))))


