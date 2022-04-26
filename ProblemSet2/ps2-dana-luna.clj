;; Problem 1: define a function `absval`
(defn absval [x] (Math/sqrt (* x x)))

;; Problem 2: fix the functions `take-square` and `sum-of-squares`
(defn take-square [x] (* x x))
(defn sum-of-squares [x y](+ (take-square x) (take-square y)))

;; Problem 3: define expressions `exp-13-1`, `exp-13-2`, `exp-13-3`, and `exp-13-4`
(def exp-13-1 '(* 1 13))
(def exp-13-2 '(/ 26 2))
(def exp-13-3 '(- 100 87))
(def exp-13-4 '(Math/sqrt 169))

;; Problem 4: define a function `third`
(defn third [l] (first (rest (rest l))))

;; Problem 5:
;; [let's import define `sqrt` and `abs` from Java.lang.Math for the example]
(defn sqrt [x] (Math/sqrt x))
(defn abs [x] (Math/abs x))

;; define a function `compose`
(defn compose [f g] (fn [x] (f (g x))) )

;; Problem 6: define a function `first-two`
(defn first-two [l] (cons (first l) (list (first (rest l)))))

;; Problem 7: define a function `remove-second`
(defn remove-second [l] (cons (first l) (rest (rest l))))

;; Problem 8: define a function `add-to-end`
(defn add-to-end [lst x] (reverse (cons x (reverse lst))))

;; Problem 9: define a function `reverse`
;;           (Note that this funciton will overwrite a built in function.
;;            This is okay.)
(defn reverse [lst] (if (= 1 (count lst)) lst (add-to-end (reverse (rest lst)) (first lst))))

;; Problem 10: define a function `count-to-1`
(defn count-to-1 [n] (if (= 1 n) (list 1) (cons n (count-to-1 (- n 1)))))

;; Problem 11: define a function `count-to-n`
(defn count-to-n [n] (if (= 1 n) (list 1) (add-to-end (count-to-n (- n 1)) n)))

;; Problem 12: define a function `get-max`
(defn get-max [lst] (if (= 1 (count lst)) (first lst) (if (> (first lst) (get-max (rest lst))) (first lst) (get-max (rest lst)) )))

;; Problem 13: define a function `greater-than-five?`
(defn greater-than-five? [lst] (if (= 1 (count lst)) (list (> (first lst) 5)) (cons (> (first lst) 5) (greater-than-five? (rest lst)))) )

;; Problem 14: define a function `concat-three`
(defn concat-three [x y z] (if (empty? x) (if (empty? y) z (cons (first y) (concat-three x (rest y) z))) (cons (first x) (concat-three (rest x) y z))))

;; Problem 15: define a function `sequence-to-power`
(defn sequence-to-power [x n] (if (= n 0) 1 (if (= n 1) x (concat x (sequence-to-power x (- n 1))))))

;; Problem 16: define a function `in-L-star?`
(def L '(a))

(defn prefix? [pr str]
      (cond
        (> (count pr) (count str)) false
        (empty? pr) true
        (= (first pr) (first str)) (prefix? (rest pr) (rest str))
        :else false))

(defn in-L-star? [str]
      (if (empty? str)
        true
        (if (prefix? str L) (in-L-star? (rest (rest str)))
                            false)))
