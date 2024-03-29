(require '[clojure.test :refer [deftest is run-tests]])

; 1.- !
(defn !
  "Takes a positive integer n as its argument and returns its 
  corresponding factorial"
  [n]
  (if (zero? n)
    1
    (*' n (! (dec n)))))

; 1.- !
(deftest test-!
  (is (= 1
         (! 0)))
  (is (= 120
         (! 5)))
  (is (= '(1 1 2 6 24 120 720 5040 40320 362880 3628800)
         (map ! (range 11))))
  (is (= 15511210043330985984000000N
        (! 25)))
  (is (= 815915283247897734345611269596115894272000000000N
         (! 40))))

; 2.- duplicate
(defn duplicate
       "Takes a list lst as its argument and returns a new list 
       with each element of lst duplicated."
       [lst]
       (mapcat #(list % %) lst))

(deftest test-duplicate
       (is (= '(1 1 2 2 3 3 4 4 5 5)
              (duplicate '(1 2 3 4 5))))
       (is (= ()
              (duplicate ())))
       (is (= '(a a)
              (duplicate '(a))))
       (is (= '(a a b b c c d d e e f f g g h h)
              (duplicate '(a b c d e f g h)))))

(defn pow
       "Takes two arguments as input: a number a and a positive 
       integer b. It returns the result of computing a raised to 
       the power b."
       [a b]
       (reduce *'
         (repeat b a)))

(deftest test-pow
     (is (= 1 (pow 0 0)))
     (is (= 0 (pow 0 1)))
     (is (= 1 (pow 5 0)))
     (is (= 5 (pow 5 1)))
     (is (= 125 (pow 5 3)))
     (is (= 25 (pow -5 2)))
     (is (= -125 (pow -5 3)))
     (is (= 1024 (pow 2 10)))
     (is (= 525.21875 (pow 3.5 5)))
     (is (= 129746337890625 (pow 15 12)))
     (is (= 3909821048582988049 (pow 7 22))))

(run-tests)