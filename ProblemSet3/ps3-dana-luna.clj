(ns ps3-dana-luna)

(comment "Make sure that you follow the instructions carefully
										and use the right procedure names, inputs, and outputs")

;; Problem 1: define a function `get-vocabulary`
(def moby-word-tokens '(CALL me Ishmael . Some years ago never mind
                             how long precisely having little or no money in my purse , and
                             nothing particular to interest me on shore , I thought I would
                             sail about a little and see the watery part of the world .  It is
                             a way I have of driving off the spleen , and regulating the
                             circulation . Whenever I find myself growing grim about the mouth
                             whenever it is a damp , drizzly November in my soul whenever I
                             find myself involuntarily pausing before coffin warehouses , and
                             bringing up the rear of every funeral I meet and especially
                             whenever my hypos get such an upper hand of me , that it requires
                             a strong moral principle to prevent me from deliberately stepping
                             into the street , and methodically knocking people's hats off
                             then , I account it high time to get to sea as soon as I can .
                             This is my substitute for pistol and ball . With a philosophical
                             flourish Cato throws himself upon his sword I quietly take to the
                             ship .  There is nothing surprising in this . If they but knew it
                             , almost all men in their degree , some time or other , cherish
                             very nearly the same feelings toward the ocean with me .))

;see if w is in l
(defn member-of-list? [w l]
      (if (empty? l)
        false
        (if (= w (first l))
          true
          (member-of-list? w (rest l)))))

(defn get-vocabulary [word-tokens vocab]
  (if (empty? word-tokens)
      vocab
      (if (member-of-list? (first word-tokens) vocab)
          (get-vocabulary (rest word-tokens) vocab)
          (get-vocabulary (rest word-tokens) (cons (first word-tokens) vocab)))))

(def moby-vocab (get-vocabulary moby-word-tokens '()))

;; Problem 02: define a function `get-count-of-word`
(defn get-count-of-word [w word-tokens count]
      (if (empty? word-tokens)
      count
      (if (= (first word-tokens) w)
        (get-count-of-word w (rest word-tokens) (+ count 1))
        (get-count-of-word w (rest word-tokens) count))
))

;; Problem 03: define a variable `moby-word-frequencies`
(defn get-word-counts [vocab word-tokens]
      (let [count-word (fn [w]
                           (get-count-of-word w word-tokens 0))]
           (map count-word vocab)))

(def moby-word-frequencies (get-word-counts moby-vocab moby-word-tokens))

;; Problem 04: write a function `sample-uniform-BOW-sentence`
(defn flip [p]
      (if (< (rand 1) p)
        true
        false))

(defn normalize [params]
      (let [sum (apply + params)]
           (map (fn [x] (/ x sum)) params)))

(defn sample-categorical [outcomes params]
      (if (flip (first params))
        (first outcomes)
        (sample-categorical (rest outcomes) (normalize (rest params)))))

(defn create-uniform-distribution [outcomes]
      (let [num-outcomes (count outcomes)]
           (map (fn [x] (/ 1 num-outcomes))
                outcomes)))

(defn sample-uniform-BOW-sentence [n vocab]
      (if (or (= n 0) (= vocab '()))
        '()
        (cons (sample-categorical vocab (create-uniform-distribution vocab))
        (sample-uniform-BOW-sentence (- n 1) vocab))))

;; Problem 5: Define a function `compute-uniform-BOW-prob`
(defn subset-of? [sentence vocab]
      (if (= false (member-of-list? (first sentence) vocab))
      false
      (if (= (count sentence) 1)  true (subset-of? (rest sentence) vocab))))

(defn compute-uniform-BOW-prob [vocab sentence]
      (if (= false (subset-of? (get-vocabulary sentence vocab) vocab))
        0 (reduce * (repeat (count sentence) (/ 1 (count vocab))))))

;; Problem 6
(def sentence-generated (sample-uniform-BOW-sentence 3 moby-vocab))
(def sentence-generated-prob (compute-uniform-BOW-prob moby-vocab sentence-generated))

;; Problem 7: Define a variable `moby-word-probabilities`
(defn sample-BOW-sentence [len vocabulary probabilities]
      (if (= len 0)
        '()
        (cons (sample-categorical vocabulary probabilities)
              (sample-BOW-sentence (- len 1) vocabulary probabilities))))

(def moby-word-probabilities (normalize (map (fn [x] (/ x (count moby-vocab))) moby-word-frequencies)))

;; Problem 9: Define a function lookup-probability
(defn lookup-probability [w outcomes probs]
      (if (= false (member-of-list? w outcomes)) 0.0
      (if (= w (first outcomes)) (first probs) (lookup-probability w (rest outcomes) (rest probs)))))

(defn product [l] (apply * l))

;; Problem 10: Define a function compute-BOW-prob
(defn compute-BOW-prob [sentence vocabulary probabilities]
  (product (map (fn [w] (lookup-probability w vocabulary probabilities)) sentence)))

;; Another recursive way to define the function without map
(defn compute-BOW-prob2 [sentence vocabulary probabilities]
      (if (empty? sentence) 1.0
      (* (lookup-probability (first sentence) vocabulary probabilities)
      (compute-BOW-prob (rest sentence) vocabulary probabilities))))

