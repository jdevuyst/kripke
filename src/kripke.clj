(ns kripke
  (:refer-clojure :exclude [name])
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

;; Tweakable

(def ^:dynamic *map* map)

(def ^:dynamic *mapcat* mapcat)

(def ^:dynamic *fold* reduce)

;; Core

(defn model [frame smaps]
  (*map* (<fn w/prewalk-replace frame) smaps))

(defn choice? [x]
  (-> x meta ::choice))

(defn abstract
  ([form] (abstract form [{}] vector))
  ([form smaps cont]
   (if (choice? form)
     (let [symb (gensym 'abstract__)]
       (cont symb
             (*mapcat* (<fn form symb) smaps)))
     (let [!smaps (atom smaps)]
       (cont (w/walk (<fn abstract @!smaps (fn [form smaps]
                                             (reset! !smaps smaps)
                                             form))
                     identity
                     form)
             @!smaps)))))

(defn make [form]
  (apply model (abstract form)))

(defn abstract-more [k smap]
  {:pre [(or (symbol? k) (keyword? k))
         (map? smap)]}
  (let [[form smaps] (abstract (smap k) [smap] vector)]
    (map #(assoc %1 k %2)
         smaps
         (model form smaps))))

(defmacro choicefn [[smap-name symb-name] & body]
  `(do ^::choice (fn [~smap-name ~symb-name]
                   (mapcat (partial abstract-more ~symb-name)
                           (do ~@body)))))

(defn alt [& disjuncts]
  (choicefn [smap symb]
            (map (partial assoc smap symb)
                 disjuncts)))

(defn tab [id & xs]
  {:pre [(keyword? id)]}
  (choicefn [smap symb]
            (if-let [i (smap id)]
              [(assoc smap symb (nth xs i))]
              (map #(assoc smap symb %1, id %2)
                   xs
                   (range)))))

(defmacro store [[id vexpr] & body]
  {:pre [(keyword? id)]}
  `(choicefn [smap# symb#]
             (abstract-more ~id
                            (assoc smap# ~id ~vexpr symb# (do ~@body)))))

(defmacro retr [[vname id] & body]
  {:pre [(symbol? vname)
         (keyword? id)]}
  `(choicefn [smap# symb#]
             (let [~vname (smap# ~id)]
               [(assoc smap# symb# (do ~@body))])))

(defn summarize [coll]
  {:pre [(coll? coll) (seq coll)]}
  (let [prototype (*fold* (partial diff (fn [_ _] (gensym 'summarize__)))
                          coll)]
    [prototype
     (*map* #(gather yield {}
                     (diff (comp yield vector) prototype %))
            coll)]))