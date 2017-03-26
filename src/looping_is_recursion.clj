(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc b e]
		(if (= 0 e)
			acc
			(recur (* acc b) b (dec e))))]
	(helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc s]
		(if (empty? s)
			acc
			(recur (first s) (rest s))))]
	(helper nil a-seq )))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc s1 s2]
		(cond
		 (and (empty? s1) (empty? s2))
			acc
		 (or (empty? s1) (empty? s2))
			false
		 (not (= (first s1) (first s2)))
			false
		 :else (recur acc (rest s1) (rest s2))))]
	(helper true seq1 seq2 )))

(defn find-first-index [pred a-seq]
	(loop [acc 0
	       p pred
	       seq1 a-seq]
	    (if (empty? seq1)
		nil
		(if (p (first seq1))
		  acc
		  (recur (inc acc) p (rest seq1))))))

(defn avg [a-seq]
	(loop [acc 0
               n 0
               seq1 a-seq]
	  (cond
		(and (empty? seq1) (= n 0))
		acc
		(empty? seq1)
		(/ acc n)
		:else (recur (+ acc (first seq1)) (inc n) (rest seq1)))))

(defn parity [a-seq]
	(loop [pars #{}
	       seq1 a-seq]
	      (if (empty? seq1)
		pars
		(if (contains? pars (first seq1))
		  (recur (disj pars (first seq1)) (rest seq1))
		  (recur (conj pars (first seq1)) (rest seq1))))))

(defn fast-fibo [n]
	(loop [counter n
	       nup 1 
	       ndown 0]
	       (if (= counter 0)
		ndown
      	       (recur (- counter 1) (+ nup ndown) nup))))
		

(defn cut-at-repetition [a-seq]
	(loop [set-holder #{}
	       seq-return []  	
	       seq1 a-seq]
	      (if (empty? seq1)
		seq-return
	        (if (contains? set-holder (first seq1))
		  seq-return
		  (recur (conj set-holder (first seq1)) (conj seq-return (first seq1)) (rest seq1))))))

