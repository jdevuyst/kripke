(ns kripke-test
  (:require [clojure.test :refer :all]
            [kripke :refer :all]
            [fletching.macros :refer :all]))

(deftest core
  (testing "prune"
    (is (= [[1 2 'a 'b] #{{'a 1 'b 2}
                          {'a 6 'b 7}}]
           (prune [1 2 'a 'b] [{'a 1 'b 2 'c 3 'd 4}
                               {'a 1 'b 2 'e 5}
                               {'a 6 'b 7 'f 8}]))))
  (testing "model"
    (is (= (list [1 [2 {3 [4 5]}]]
                 [1 [-2 {3 [-4 5]}]])
           (model [1 [:a {:b [:c 5]}]]
                  [{:a 2 :b 3 :c 4}
                   {:a -2 :b 3 :c -4}]))))
  (testing "abstract/alt"
    (is (= (->> (abstract [(alt 1 2 3) (alt 4 5)])
                (apply model))
           (for [x [1 2 3] y [4 5]] [x y])
           (list [1 4] [1 5] [2 4] [2 5] [3 4] [3 5])))
    (is (= #{1 2 3 4}
           (set (apply model (abstract (alt 1 2 3 4))))
           (set (apply model (abstract (alt 1 2 (alt 3 4))))))))
  (testing "explore"
    (is (= #{{1 2} {3 4} {1 4} {3 2}}
           (->> [{1 2} {3 4}] explore (apply model) set))))
  (testing "summarize"
    (is (= [{1 2} {3 4}]
           (->> [{1 2} {3 4}] summarize (apply model))))))