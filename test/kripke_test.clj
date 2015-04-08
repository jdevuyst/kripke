(ns kripke-test
  (:require [clojure.test :refer :all]
            [kripke :refer :all]
            [fletching.macros :refer :all]))

;; Experimental

(defmacro with [[vname choice] & body]
  {:pre [(symbol? vname)]}
  `(let [choice# ~choice]
     (assert (choice? choice#))
     (choicefn [smap# symb#]
               (for [~vname (map symb# (choice# smap# symb#))]
                 (assoc smap# symb# (do ~@body))))))

(defn const [v moniker]
  {:pre [(keyword? moniker)]}
  (choicefn [smap symb]
            (condp = (get smap moniker ::not-found)
              v [(assoc smap symb v)]
              ::not-found [(assoc smap symb v, moniker v)]
              nil)))

(defmacro eavesdrop [spy-name init-val expr]
  `(let [!acc# (-> ~init-val transient atom)
         ~spy-name (fn [x#]
                     (swap! !acc# conj! x#)
                     x#)]
     [~expr (persistent! @!acc#)]))

(defmacro gather [& args]
  `(second (eavesdrop ~@args)))

(require '[clojure.walk :as w])

(defn prune [frame smaps]
  (let [all-keys (set (mapcat keys smaps))
        diff (->> (gather yield []
                          (w/postwalk yield frame))
                  (reduce disj! (transient all-keys))
                  persistent!)]
    [frame (->> smaps
                (*map* #(persistent! (reduce dissoc! (transient %) diff)))
                set)]))

(def diff #'kripke/diff)

(defn explore [coll]
  {:pre [(coll? coll) (seq coll)]}
  (abstract (*fold* (partial diff alt)
                    coll)))

(deftest experimental
  (testing "with"
    (is (= '(-1 -2 -3)
           (make (with [x (alt 1 2 3)] (- x))))))
  (testing "const"
    (is (= '(1) (make (const 1 :x))))
    (is (= '([1 1]) (make [(const 1 :x) (const 1 :x)])))
    (is (empty? (make [(const 1 :x) (const 2 :x)]))))
  (testing "prune"
    (is (= [[1 2 'a 'b] #{{'a 1 'b 2}
                          {'a 6 'b 7}}]
           (prune [1 2 'a 'b] [{'a 1 'b 2 'c 3 'd 4}
                               {'a 1 'b 2 'e 5}
                               {'a 6 'b 7 'f 8}]))))
  (testing "explore"
    (is (= #{{1 2} {3 4} {1 4} {3 2}}
           (->> [{1 2} {3 4}] explore (apply model) set)))
    (is (= '([1 2 3] [1 2 4] [1 2 10] [1 0 3] [1 0 4] [1 0 10])
           (apply model (explore [[1 2 3] [1 2 4] [1 0 10]]))))))

;; Core Tests

(deftest core
  (testing "model"
    (is (= (list [1 [2 {3 [4 5]}]]
                 [1 [-2 {3 [-4 5]}]])
           (model [1 [:a {:b [:c 5]}]]
                  [{:a 2 :b 3 :c 4}
                   {:a -2 :b 3 :c -4}]))))
  (testing "abstract/make"
    (testing "alt"
      (is (= '([1 4] [1 5] [2 4] [2 5] [3 4] [3 5])
             (for [x [1 2 3] y [4 5]] [x y])
             (make [(alt 1 2 3) (alt 4 5)])))
      (is (= '(1 2 3 4)
             (make (alt 1 2 3 4))
             (make (alt 1 2 (alt 3 4)))))
      (is (= '(1 2)
             (make (alt 1 2 (alt)))))
      (is (= '(1 2 [3] [4])
             (make (alt 1 2 [(alt 3 4)]))))
      (is (empty? (make [{(alt) 1}]))))
    (testing "tab"
      (is (= '([1 3] [2 4])
             (make [(tab :a 1 2) (tab :a 3 4)])))
      (is (= '([1 3] [2 4])
             (make [(tab :a 1 2) (tab :a 3 4)])))
      (is (= '(1 2 3)
             (make (alt 1 2 3))
             (make (tab :a 1 2 3))))
      (is (= '([1 a] [1 b] [2 a] [2 b])
             (make [(alt 1 2) (alt 'a 'b)])
             (make [(tab :a 1 2) (tab :b 'a 'b)]))))
    (testing "store/retr"
      (is (= '([(nil) (1) (-2)] [(nil) (1) (-3)])
             (make [(list (alt (retr [a :a] a)))
                    (list (alt (store [:a (alt 2 3)] 1)))
                    (list (alt (retr [a :a] (- a))))])))))
  (testing "summarize"
    (is (= [{1 2} {3 4}]
           (->> [{1 2} {3 4}] summarize (apply model))))))

;; README tests

(defn normalize-symbols [form]
  (let [!i (atom 0)]
    (w/postwalk #(if (symbol? %)
                   (symbol (str 'symbol__ (swap! !i inc)))
                   %)
                form)))

(defn ≅ [& args]
  (= (map normalize-symbols args)))

(deftest readme
  (let [prototype {:key (alt 1 2 3)}
        frame+smaps (abstract prototype)]
    (is (-> prototype vals first fn?))
    (is (≅ '[{:key abstract__11051}
             ({abstract__11051 1} {abstract__11051 2} {abstract__11051 3})]
           frame+smaps))
    (is (= (apply model frame+smaps)
           '({:key 1} {:key 2} {:key 3})))
    (is (= (let [[frame smaps] frame+smaps
                 new-frame (clojure.set/map-invert frame)]
             (model new-frame smaps))
           '({1 :key} {2 :key} {3 :key})))
    (is (= (make (alt 1 2 [(alt 3 4 (alt 5 6) (alt))]))
           '(1 2 [3] [4] [5] [6])))
    (is (empty? (make #{[(alt)]})))
    (is (= (make {(tab :t 1 2 3) (tab :t 'a 'b 'c)})
           '({1 a} {2 b} {3 c})))
    (is (= (make {(tab :x 1 2 3) (tab :x 'a 'b 'c)
                  (tab :y "Aardvark" "Zebra") (tab :y :wine :beer)})
           '({1 a "Aardvark" :wine}
             {1 a "Zebra" :beer}
             {2 b "Aardvark" :wine}
             {2 b "Zebra" :beer}
             {3 c "Aardvark" :wine}
             {3 c "Zebra" :beer})))
    (is (= (make [(alt (store [:a (alt 1 2 3 4)] :even))
                  (alt (retr [a :a] (if (even? a) a (alt))))])
           '([:even 2] [:even 4])))
    (let [form '([1 2 [3 #{4}]] [:a 2 [3 (4)]])]
      (is (≅ (summarize form))
          '[[summarize__14972 2 [3 summarize__14973]]
            ({summarize__14972 1 summarize__14973 #{4}}
             {summarize__14972 :a summarize__14973 (4)})])
      (is (= form
             (apply model (summarize form)))))))