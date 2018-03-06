(ns csc344clojure.core
  (:gen-class))

;==================================
;
;  Author     : Andrew Vadnais
;  Course     : CSC 344
;  Date       : 03/04/2018
;  Assignment : https://danielschlegel.org/wp/teaching/csc344-spring-2018/csc344-assignment-2/
;
;==================================


;; vector multiplication
(defn mult
  [x]

  (let [num (second x)]

    (if (= num '1)
      (nth x 2 x)
      (if (= (nth x 2) '1)
        num
        (if (= num '0)
          '0
          (if (= (nth x 2 x) '0)
            '0
            (if (integer? num)
              (if (integer? (nth x 2 x))
                (* num (nth x 2 x))
                (if (= num '-1)
                  (list '- (nth x 2 x))
                  (if (= (nth x 2 x) '-1)
                    (list '- num)
                    x)))
              (if (= num '-1)
                (list '- (nth x 2 x))
                (if (= (nth x 2 x) '-1)
                  (list '- num)
                  x))))))))
  )



;; vector subtraction
(defn sub
  [x]

  (if (= (first (second x)) '-)
    (second (second x))
    x)
  )



;; vector addition
(defn add
  [x]

  (let [num (second x)]

    (if (= num '0)
      (nth x 2 x)
      (if (= (nth x 2 x) '0)
        num
        (if (integer? num)
          (if (integer? (nth x 2 x))
            (+ num (nth x 2 x))
            x)
          x))))
  )


;; must declare before referencing
(declare simplify)


;; uses simplify to store results in vector
(defn transform
  [x y]

  (let [x1 (first x)
        x2 (second x)
        y1 (first y)
        y2 (second y)]

    (vector
      (simplify
        (list '+ (simplify (list '* (first x1) y1))
              (simplify (list '* (second x1) y2)))
        )
      (simplify
        (list '+ (simplify (list '* (first x2) y1))
              (simplify (list '* (second x2) y2))))))
  )


;; does arithmetic
(defn simplify
  [x]

  (let [op (first x)
        num (second x)]

    (cond
      (= op 'transform) (if (= (first (nth x 2 x)) 'transform)
                          (transform num (simplify (nth x 2 x)))
                          (transform num (nth x 2 x)))
      (= op '*) (mult x)
      (= (first x) '+) (add x)
      (= op '-) (sub x)
      :else x))
  )




;; binds values to vector
(defn bind-values
  [m l]

  (map (fn [i]
         (cond
           (seq? i) (bind-values m i)
           (vector? i) (vec (bind-values m i))
           :default (m i i)))
       l)
  )



(defn evalexp
  [exp bindings]

  (simplify (bind-values bindings exp))
  )


(def p1 '(transform [[a 3] [0 0]] [x y]))
(def p2 '(transform [[1 0] [0 (+ x 3)]] [(* x 2) y]))
(def p3 '(transform [[0 0] [1 1]]
                    (transform [[2 0] [0 2]]
                               (transform [[-1 0] [0 -1]] [x 2]))))



;(evalexp '(transform [[a 3] [4 5]] [3 4]) '{a 5, y 2})
;(evalexp p3 '{x 5, y 2})
;(evalexp p1 '{a 5, y 2})

;(transform [['a 2] [3 4]] [5 6])