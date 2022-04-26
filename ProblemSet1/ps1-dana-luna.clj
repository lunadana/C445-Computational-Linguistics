;; Problem 1
;; Define a variable `year`
(def year 2021)

;; Problem 5
;; Define a function `add-up`
(def add-up (fn [x y] (+ x y)))

;; Problem 6
;; Define a function `is-it-four?`
;;   (Note: Don't forget the question mark in the name!)
(def is-it-four? (fn [x] (= x 4)))

;; Problem 7
; add your code in the indicated space below,
; (so that the symbol `problem-7` evaluates to `true`)
(def problem-7 ;<-- do not edit this
  (= (quote    ;<-- do not edit this
       platypus
       ) 'platypus) ;<-- do not edit this
  )              ;<-- do not edit this

;; Problem 8
;; Define a function `func` and an expression `expr`
(def func (fn [x y] (+ x y)))
(def expr '(1 2))

;; Problem 9
;; Define a function `both-same-type?`
;;   (Note: Don't forget the question mark in the name!)
(def both-same-type? (fn [x y] (= (type x) (type y))))

;; Problem 10
;; Define a function `list-longer-than?`
;;   (Note: Don't forget the question mark in the name!)
(def list-longer-than? (fn [x y] (< x (count y))))

;; Problem 11
;; Define a function `dot-product`
(defn dot-product [x y] (apply + (map + x y)))

;; Problem 12
;; Define a function `swap-arg-order`
(defn swap-arg-order [fun] (fn [x y] (fun y x)))

;; Problem 13
;; Define a higher order function `g`
(defn g [fun] (fun 10))
