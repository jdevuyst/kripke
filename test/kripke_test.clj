(ns kripke-test
  (:require [clojure.test :refer :all]
            [kripke :refer :all]
            [fletching.macros :refer :all]))

(deftest core
  (testing "model"
    (is (= (list [1 [2 {3 [4 5]}]]
                 [1 [-2 {3 [-4 5]}]])
           (model [1 [:a {:b [:c 5]}]]
                  [{:a 2 :b 3 :c 4}
                   {:a -2 :b 3 :c -4}]))))
  (testing "abstract"
    (testing "alt"
      (is (= (list [1 4] [1 5] [2 4] [2 5] [3 4] [3 5])
             (for [x [1 2 3] y [4 5]] [x y])
             (make [(alt 1 2 3) (alt 4 5)])))
      (is (= (list 1 2 3 4)
             (make (alt 1 2 3 4))
             (make (alt 1 2 (alt 3 4)))))
      (is (= (list 1 2)
             (make (alt 1 2 (alt)))))
      (is (empty? (make [{(alt) 1}]))))
    (testing "table"
      (is (= (list [1 3] [2 4])
             (make [(table :a 1 2) (table :a 3 4)])))
      (is (= (list [1 3] [2 4])
             (make [(table :a 1 2) (table :a 3 4)])))
      (is (= (list 1 2 3)
             (make (alt 1 2 3))
             (make (table :a 1 2 3))))
      (is (= (list [1 'a] [1 'b] [2 'a] [2 'b])
             (make [(alt 1 2) (alt 'a 'b)])
             (make [(table :a 1 2) (table :b 'a 'b)]))))
    (testing "load/store"
      (is (= (list [1 2] [1 3])
             (make [(store [:a (alt 2 3)] 1)
                    (load [a :a] a)]))))
    (testing "explore"
      (is (= #{{1 2} {3 4} {1 4} {3 2}}
             (->> [{1 2} {3 4}] explore (apply model) set)))
      (is (= (list [1 2 3] [1 2 4] [1 2 10] [1 0 3] [1 0 4] [1 0 10])
             (apply model (explore [[1 2 3] [1 2 4] [1 0 10]])))))
    (testing "summarize"
      (is (= [{1 2} {3 4}]
             (->> [{1 2} {3 4}] summarize (apply model)))))))