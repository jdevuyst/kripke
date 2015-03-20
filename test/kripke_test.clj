(ns kripke-test
  (:require [clojure.test :refer :all]
            [kripke :refer :all]
            [fletching.macros :refer :all]))

(deftest core
  (testing "disjunction, alt, alt*, disjunction?"
    (is (= (disjunction (range 1 5))
           (alt 1 2 3 (inc 3))
           (alt* 1 2 3 (inc 3))))
    (is (disjunction? (disjunction (range 1 5))))
    (is (disjunction? (alt 1 2 3 (inc 3))))
    (is (disjunction? (alt* 1 2 3 (inc 3)))))
  (testing "project"
    (is (= [[2 1] {[[[9 8 7 6]] 5 4 3] [10]}]
           (-> [(alt 1 2) {(alt* 3 4 5 [(disjunction [6 7 8 9])]) (alt 10)}]
               (project reverse))))
    (is (= [1 [3 4]]
           (project (alt 1 :two (alt 3 4 :five))
                    (partial filter (complement keyword?)))))
    (is (= 4
           (project (alt 1 2 (alt 3 4))
                    (partial apply max)))))
  (testing "expand"
    (is (= (set (for [x [1 2] y [3 4]] [x y]))
           (set (expand [(alt 1 2) (alt 3 4)]))))
    (is (= #{1 2 3 4}
           (set (expand (alt 1 2 3 4)))
           (set (expand (alt 1 2 (alt 3 4)))))))
  (testing "summarize"
    (is (= (-> '([1 2 [3] {[:a] :b}]
                 [1 2 [[4]] {:c [:d]}])
               summarize
               first)
           [1 2 [(alt 3 [4])] {(alt [:a] :c) (alt :b [:d])}]))
    (is (= '([1 2 [3] {[:a] :b}]
             [1 2 [[4]] {:c [:d]}])
           (apply expand (summarize '([1 2 [3] {[:a] :b}]
                                      [1 2 [[4]] {:c [:d]}])))))))
