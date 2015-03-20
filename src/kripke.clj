(ns kripke
  (:require [clojure.walk :refer (walk postwalk prewalk)]
            [clojure.math.combinatorics :refer (cartesian-product)]
            [fletching.macros :refer (<fn ?>)]))

;
; Utilities
;

(defn- iter! [coll]
  (let [!coll (atom coll)]
    #(?> (first @!coll)
         do (swap! !coll rest) ?)))

(defmacro eavesdrop [spy-name expr]
  `(let [!acc# (-> [] transient atom)
         ~spy-name (fn [x#]
                     (swap! !acc# conj! x#)
                     x#)
         r# ~expr]
     [r# (persistent! @!acc#)]))

(defmacro gather [& args]
  `(second (eavesdrop ~@args)))

(defn- walk-type [x]
  (cond (list? x) [:list (count x)]
        (instance? clojure.lang.IMapEntry x) [:map-entry (count x)]
        (seq? x) [:seq (count x)]
        (instance? clojure.lang.IRecord x) [:record (count x)]
        (coll? x) [:coll (count x)]
        :else x))

(defn- diff [f x y]
  {:pre [(fn? f)]}
  (if (not= (walk-type x) (walk-type y))
    (f x y)
    (let [f! (iter! (gather yield
                            (walk yield identity x)))]
      (walk #(diff f (f!) %)
            identity
            y))))

;
; Core
;

(def ^:dynamic *map* map)

(def ^:dynamic *fold* reduce)

(defn disjunction [coll]
  {:pre [(coll? coll)
         (seq coll)]}
  [::disjunction coll])

(defn disjunction? [x]
  (and (coll? x)
       (= ::disjunction (first x))
       (-> x second coll?)))

(defn alt [& xs]
  (disjunction xs))

(defmacro alt* [& exprs]
  `(disjunction (map #(%) [~@(for [expr exprs]
                               `(fn [] ~expr))])))

(defn project [x f]
  {:pre [(fn? f)]}
  (postwalk #(if (disjunction? %)
               (-> % second f)
               %)
            x))

(defn expand
  ([x] (let [[x disjunctions] (eavesdrop spy
                                         (project x (comp disjunction spy vec)))]
         (expand x (->> disjunctions
                        (map (comp range count))
                        (apply cartesian-product)
                        (map (comp #(<fn (%))
                                   iter!
                                   (partial map #(<fn nth %))))))))
  ([x fs] (*map* (partial project x) fs)))

(defn summarize [coll]
  {:pre [(coll? coll)
         (seq coll)]}
  (let [table-id (gensym "summarize__")
        prototype (*fold* (partial diff (constantly ::wildcard))
                          coll)
        next-dom! (->> coll
                       (*map* (fn [x]
                                (gather yield
                                        (diff #(when (= %1 ::wildcard)
                                                 (yield %2))
                                              prototype
                                              x))))
                       (apply map vector)
                       iter!)]
    [(prewalk #(if (= % ::wildcard)
                 (-> (next-dom!)
                     (with-meta {::table-id table-id})
                     disjunction)
                 %)
              prototype)
     (map #(fn [dom]
             (if (= table-id (-> dom meta ::table-id))
               (nth dom %)
               dom))
          (-> coll count range))]))