(ns kripke
  (:require [clojure.walk :as w]
            [fletching.macros :refer (<fn ?>)]))

;; Utilities

(defn- walk-type [x]
  (cond (list? x) [:list (count x)]
        (instance? clojure.lang.IMapEntry x) [:map-entry (count x)]
        (seq? x) [:seq (count x)]
        (instance? clojure.lang.IRecord x) [:record (count x)]
        (coll? x) [:coll (count x)]
        :else x))

(defmacro ^:private gather [conj-name init-val expr]
  `(let [!acc# (-> ~init-val transient atom)
         ~conj-name (fn [x#]
                      (swap! !acc# conj! x#)
                      x#)]
     ~expr
     (persistent! @!acc#)))

(defn- iter! [coll]
  (let [!coll (atom coll)]
    #(?> (first @!coll)
         do (swap! !coll rest) ?)))

(defn- diff [f x y]
  {:pre [(fn? f)]}
  (if (not= (walk-type x) (walk-type y))
    (f x y)
    (let [f! (iter! (gather yield []
                            (w/walk yield identity x)))]
      (w/walk #(diff f (f!) %)
              identity
              y))))

;; Core

(def ^:dynamic *map* map)

(def ^:dynamic *fold* reduce)

(def ^:dynamic *op*)

(defmacro abstract [expr]
  `(let [!smaps# (ref [{}])]
     (binding [*op* (fn [f#]
                      (dosync
                        (let [symb# (gensym 'abstract__)]
                          (->> @!smaps#
                               (mapcat (<fn f# symb#))
                               (ref-set !smaps#))
                          symb#)))]
       [~expr @!smaps#])))

(defn alt [& disjuncts]
  {:pre [(fn? *op*)
         (-> disjuncts count pos?)]}
  (*op* (fn [smap symb]
          (map #(assoc smap symb (w/prewalk-replace smap %))
               disjuncts))))

(defn prune [frame smaps]
  (let [all-keys (set (mapcat keys smaps))
        diff (->> (gather yield []
                          (w/postwalk yield frame))
                  (reduce disj! (transient all-keys))
                  persistent!)]
    [frame (->> smaps
                (*map* #(persistent! (reduce dissoc! (transient %) diff)))
                set)]))

(defn model [frame smaps]
  (*map* (<fn w/prewalk-replace frame) smaps))

(defn explore [coll]
  {:pre [(coll? coll) (seq coll)]}
  (abstract (*fold* (partial diff alt)
                    coll)))

(defn summarize [coll]
  {:pre [(coll? coll) (seq coll)]}
  (let [prototype (*fold* (partial diff (fn [_ _] (gensym 'summarize__)))
                          coll)]
    [prototype
     (*map* (fn [x]
              (gather yield {}
                      (diff #(yield [%1 %2]) prototype x)))
            coll)]))