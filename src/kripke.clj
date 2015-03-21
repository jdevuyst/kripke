(ns kripke
  (:require [clojure.walk :refer (walk postwalk prewalk)]
            [clojure.math.combinatorics :refer (cartesian-product)]
            [fletching.macros :refer (<fn ?>)]))

;; Utilities

(defn- walk-type [x]
  (cond (list? x) [:list (count x)]
        (instance? clojure.lang.IMapEntry x) [:map-entry (count x)]
        (seq? x) [:seq (count x)]
        (instance? clojure.lang.IRecord x) [:record (count x)]
        (coll? x) [:coll (count x)]
        :else x))

(defmacro ^:private eavesdrop [spy-name expr]
  `(let [!acc# (-> [] transient atom)
         ~spy-name (fn [x#]
                     (swap! !acc# conj! x#)
                     x#)]
     [~expr (persistent! @!acc#)]))

(defmacro ^:private gather [spy-name expr]
  `(second (eavesdrop ~spy-name ~expr)))

(defn- iter! [coll]
  (let [!coll (atom coll)]
    #(?> (first @!coll)
         do (swap! !coll rest) ?)))

(defn- diff [f x y]
  {:pre [(fn? f)]}
  (if (not= (walk-type x) (walk-type y))
    (f x y)
    (let [f! (iter! (gather yield
                            (walk yield identity x)))]
      (walk #(diff f (f!) %)
            identity
            y))))

;; Core

(def ^:dynamic *map* map)

(def ^:dynamic *fold* reduce)

(defn first= [x tag]
  (and (coll? x)
       (= tag (first x))))

(defn project [x tag f]
  {:pre [(-> tag coll? not)
         (fn? f)]}
  (postwalk #(if (first= % tag)
               (apply f (rest %))
               %)
            x))

(defn expand [x tag]
  {:pre [(-> tag coll? not)]}
  (let [[x doms] (eavesdrop yield
                            (project x tag (comp (partial vector tag)
                                                 yield
                                                 vec
                                                 distinct)))]
    (->> doms
         (map (comp range count))
         (apply cartesian-product)
         (map (comp (partial project x tag)
                    #(<fn (%))
                    iter!
                    (partial map #(<fn nth %)))))))

(defn summarize [coll tag]
  {:pre [(coll? coll) (seq coll)
         (-> tag coll? not)]}
  (let [prototype (*fold* (partial diff (constantly ::wildcard))
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
                 [tag (next-dom!)]
                 %)
              prototype)
     (fn [x]
       (*map* (comp (partial project x tag)
                    #(<fn nth %))
              (-> coll count range)))]))