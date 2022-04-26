(ns ps4-dana-luna)

;;Preliminaries
(def vocabulary '(call me ishmael))
(def theta1 (list (/ 1 2 ) (/ 1 4 ) (/ 1 4 )))
(def theta2 (list (/ 1 4 ) (/ 1 2 ) (/ 1 4 )))
(def thetas (list theta1 theta2))
(def theta-prior (list (/ 1 2) (/ 1 2)))(ns ps4-dana-luna)

;;Preliminaries
(def vocabulary '(call me ishmael))
(def theta1 (list (/ 1 2 ) (/ 1 4 ) (/ 1 4 )))
(def theta2 (list (/ 1 4 ) (/ 1 2 ) (/ 1 4 )))
(def thetas (list theta1 theta2))
(def theta-prior (list (/ 1 2) (/ 1 2)))

(defn score-categorical [outcome outcomes params]
      (if (empty? params)
        (print "no matching outcome")
        (if (= outcome (first outcomes))
          (first params)
          (score-categorical outcome (rest outcomes) (rest params)))))

(defn list-foldr [f base lst]
      (if (empty? lst)
        base
        (f (first lst)
           (list-foldr f base (rest lst)))))

(defn log2 [n]
      (/ (Math/log n) (Math/log 2)))

(defn score-BOW-sentence [sen probabilities]
      (list-foldr
        (fn [word rest-score]
            (+ (log2 (score-categorical word vocabulary probabilities))
               rest-score))
        0
        sen))

(defn score-corpus [corpus probabilities]
      (list-foldr
        (fn [sen rst]
            (+ (score-BOW-sentence sen probabilities) rst))
        0
        corpus))

(defn logsumexp [log-vals]
      (let [mx (apply max log-vals)]
           (+ mx
              (log2
                (apply +
                       (map (fn [z] (Math/pow 2 z))
                            (map (fn [x] (- x mx)) log-vals)))))))

(def my-corpus '((call me)
                 (call ishmael)))

(defn product [l] (apply * l))

;;Problem 1: define `theta-corpus-joint`
(defn theta-corpus-joint [theta corpus theta-probs]
      (log2 (* (Math/pow 2 (score-corpus corpus theta)) (score-categorical theta thetas theta-probs))))

;;Problem 2: define `compute-marginal`
(defn compute-marginal [corpus theta-probs]
      (logsumexp (map (fn [t] (theta-corpus-joint t corpus theta-probs)) thetas)
                 ))

;;Problem 3: define `compute-conditional-prob`
(defn compute-conditional-prob [theta corpus theta-probs]
      (log2 (/ (Math/pow 2 (theta-corpus-joint theta corpus theta-probs)) (Math/pow 2(compute-marginal corpus theta-probs)) )
            ))

;;Problem 4: define `compute-conditional-dist`
(defn compute-conditional-dist [corpus theta-probs]
      (map (fn [t] (compute-conditional-prob t corpus theta-probs)) thetas)
      )

;;Problem 6: define `compute-posterior-predictive`
(defn compute-posterior-predictive [observed-corpus new-corpus theta-probs]
      (let [conditional-dist (map (fn [p] (Math/pow 2 p)) (compute-conditional-dist observed-corpus theta-probs))]
           (compute-marginal new-corpus conditional-dist)))

;Problem 7: define `sample-BOW-corpus`
(defn normalize [params]
      (let [sum (apply + params)]
           (map (fn [x] (/ x sum)) params)))

(defn flip [weight]
      (if (< (rand 1) weight)
        true
        false))

(defn sample-categorical [outcomes params]
      (if (flip (first params))
        (first outcomes)
        (sample-categorical (rest outcomes)
                            (normalize (rest params)))))

(defn sample-BOW-sentence [len probabilities]
      (if (= len 0)
        '()
        (cons (sample-categorical vocabulary probabilities)
              (sample-BOW-sentence (- len 1) probabilities))))

(defn sample-BOW-corpus [theta sent-len corpus-len]
      (if (= corpus-len 0) '()
                           (cons (sample-BOW-sentence sent-len theta) (sample-BOW-corpus theta sent-len (- corpus-len 1)))))

;;Problem 8
(defn sample-theta-corpus [sent-len corpus-len theta-probs]
      (let [theta (sample-categorical thetas theta-probs)]
           (list theta (sample-BOW-corpus theta sent-len corpus-len))))


;;Problem 9: define `estimate-corpus-marginal`
(defn get-theta [theta-corpus]
      (first theta-corpus))

(defn get-corpus [theta-corpus]
      (first (rest theta-corpus)))

(defn sample-thetas-corpora [sample-size sent-len corpus-len theta-probs]
      (repeatedly sample-size (fn [] (sample-theta-corpus sent-len corpus-len theta-probs))))

(defn estimate-corpus-marginal [corpus sample-size sent-len corpus-len theta-probs]
      (let [corp (map (fn [theta-corpus] (get-corpus theta-corpus))
                      (sample-thetas-corpora sample-size sent-len corpus-len theta-probs))]
           (float (* (/ 1 sample-size) (reduce + (map (fn [c] (if (= c corpus) 1 0)) corp))))))

;;Problem 11: define `rejection-sampler`
(defn get-count [obs observation-list count]
      (if (empty? observation-list)
        count
        (if (= obs (first observation-list))
          (get-count obs (rest observation-list) (+ 1 count))
          (get-count obs (rest observation-list) count))))

(defn get-counts [outcomes observation-list]
      (let [count-obs (fn [obs] (get-count obs observation-list 0))]
           (map count-obs outcomes)))

(defn rejection-sampler [theta observed-corpus sample-size sent-len corpus-len theta-probs]
      (let [removed (remove (fn [c] (not= (get-corpus c) observed-corpus)) (sample-thetas-corpora sample-size sent-len corpus-len theta-probs))]
           (if (empty? removed) nil (float (/ (get-count theta (get-theta removed) 0) (count removed))))))















(defn score-categorical [outcome outcomes params]
      (if (empty? params)
        (print "no matching outcome")
        (if (= outcome (first outcomes))
          (first params)
          (score-categorical outcome (rest outcomes) (rest params)))))

(defn list-foldr [f base lst]
      (if (empty? lst)
        base
        (f (first lst)
           (list-foldr f base (rest lst)))))

(defn log2 [n]
      (/ (Math/log n) (Math/log 2)))

(defn score-BOW-sentence [sen probabilities]
      (list-foldr
        (fn [word rest-score]
            (+ (log2 (score-categorical word vocabulary probabilities))
               rest-score))
        0
        sen))

(defn score-corpus [corpus probabilities]
      (list-foldr
        (fn [sen rst]
            (+ (score-BOW-sentence sen probabilities) rst))
        0
        corpus))

(defn logsumexp [log-vals]
      (let [mx (apply max log-vals)]
           (+ mx
              (log2
                (apply +
                       (map (fn [z] (Math/pow 2 z))
                            (map (fn [x] (- x mx)) log-vals)))))))

(def my-corpus '((call me)
                 (call ishmael)))

(defn product [l] (apply * l))

;;Problem 1: define `theta-corpus-joint`
(defn theta-corpus-joint [theta corpus theta-probs]
      (log2 (* (Math/pow 2 (score-corpus corpus theta)) (score-categorical theta thetas theta-probs))))

;;Problem 2: define `compute-marginal`
(defn compute-marginal [corpus theta-probs]
      (logsumexp (map (fn [t] (theta-corpus-joint t corpus theta-probs)) thetas)
))

;;Problem 3: define `compute-conditional-prob`
(defn compute-conditional-prob [theta corpus theta-probs]
      (log2 (/ (Math/pow 2 (theta-corpus-joint theta corpus theta-probs)) (Math/pow 2(compute-marginal corpus theta-probs)) )
))

;;Problem 4: define `compute-conditional-dist`
(defn compute-conditional-dist [corpus theta-probs]
      (map (fn [t] (compute-conditional-prob t corpus theta-probs)) thetas)
)

;;Problem 6: define `compute-posterior-predictive`
(defn compute-posterior-predictive [observed-corpus new-corpus theta-probs]
  (let [conditional-dist (map (fn [p] (Math/pow 2 p)) (compute-conditional-dist observed-corpus theta-probs))]
    (compute-marginal new-corpus conditional-dist)))

;Problem 7: define `sample-BOW-corpus`
(defn normalize [params]
      (let [sum (apply + params)]
           (map (fn [x] (/ x sum)) params)))

(defn flip [weight]
      (if (< (rand 1) weight)
        true
        false))

(defn sample-categorical [outcomes params]
      (if (flip (first params))
        (first outcomes)
        (sample-categorical (rest outcomes)
                            (normalize (rest params)))))

(defn sample-BOW-sentence [len probabilities]
      (if (= len 0)
        '()
        (cons (sample-categorical vocabulary probabilities)
              (sample-BOW-sentence (- len 1) probabilities))))

(defn sample-BOW-corpus [theta sent-len corpus-len]
      (if (= corpus-len 0) '()
      (cons (sample-BOW-sentence sent-len theta) (sample-BOW-corpus theta sent-len (- corpus-len 1)))))

;;Problem 8
(defn sample-theta-corpus [sent-len corpus-len theta-probs]
   (let [theta (sample-categorical thetas theta-probs)]
   (list theta (sample-BOW-corpus theta sent-len corpus-len))))


;;Problem 9: define `estimate-corpus-marginal`
(defn get-theta [theta-corpus]
      (first theta-corpus))

(defn get-corpus [theta-corpus]
      (first (rest theta-corpus)))

(defn sample-thetas-corpora [sample-size sent-len corpus-len theta-probs]
      (repeatedly sample-size (fn [] (sample-theta-corpus sent-len corpus-len theta-probs))))

(defn estimate-corpus-marginal [corpus sample-size sent-len corpus-len theta-probs]
      (let [corp (map (fn [theta-corpus] (get-corpus theta-corpus))
      (sample-thetas-corpora sample-size sent-len corpus-len theta-probs))]
           (float (* (/ 1 sample-size) (reduce + (map (fn [c] (if (= c corpus) 1 0)) corp))))))

;;Problem 11: define `rejection-sampler`
(defn get-count [obs observation-list count]
      (if (empty? observation-list)
        count
        (if (= obs (first observation-list))
          (get-count obs (rest observation-list) (+ 1 count))
          (get-count obs (rest observation-list) count))))

(defn get-counts [outcomes observation-list]
      (let [count-obs (fn [obs] (get-count obs observation-list 0))]
           (map count-obs outcomes)))

(defn rejection-sampler [theta observed-corpus sample-size sent-len corpus-len theta-probs]
      (let [removed (remove (fn [c] (not= (get-corpus c) observed-corpus)) (sample-thetas-corpora sample-size sent-len corpus-len theta-probs))]
      (if (empty? removed) nil (float (/ (get-count theta (get-theta removed) 0) (count removed))))))













