(ns xutil.core
  (:refer-clojure :exclude [max min first ffirst second last count into frequencies group-by])
  (:require [clojure.core.async :as a]))

(defn make-rf
  "Returns a reducing function with initial value and finalization."
  ([f]
   (make-rf f (f) identity))
  ([f init]
   (make-rf f init identity))
  ([f init final]
   (fn make-rf-inner
     ([] init)
     ([x] (final x))
     ([x y] (f x y)))))

(defn multi-rf
  "Combines multiple reducing function."
  [rfs]
  (fn multi-rf-inner
    ([] (mapv (fn multi-rf-apply1 [f] (f)) rfs))
    ([x] (mapv (fn multi-rf-apply2 [f a] (f a)) rfs x))
    ([r x] (mapv (fn multi-rf-apply3 [f a] (f a x)) rfs r))))

(defn finalize-rf
  "Returns a reducing function with finalization."
  [rf final]
  (fn finalize-inner
    ([] (rf))
    ([x] (final (rf x)))
    ([r x] (rf r x))))

(defn- onto-chan!!
  "Blocking version of `clojure.core.async/onto-chan!`."
  [ch coll]
  (doseq [xs coll]
    (a/>!! ch (doall xs)))
  (a/close! ch))

(defn- px
  "Returns a promise object of transduced result."
  [xf rf ch]
  (let [p (promise)]
    (a/go (deliver p (a/<! (a/transduce xf rf (rf) ch))))
    p))

(defn mtransduce-async
  "Implementation of mtransduce with `clojure.core.async`.
  Realization of `coll` is controlled so not as to exceed capacity of `:buffer`.
  Application of `xf` to `coll` will happen on the caller thread.
  The sequence is partitioned with each `:part` elements and concatenated later for performance purpose."
  [xf rfs coll & {:keys [buffer part] :or {buffer 100 part 1000}}]
  (let [ch (a/chan buffer)
        m-ch (a/mult ch)
        proms (->> rfs
                   (mapv (fn tapper [rf] (let [t-ch (a/chan buffer)] (a/tap m-ch t-ch) (px cat rf t-ch))))
                   (map deref))]
    (onto-chan!! ch (sequence (comp xf (partition-all part)) coll))
    proms))

(defn mtransduce-future
  "Implementation of mtransduce using `future` object.
  This function potentially eats up memory depending on efficiency of reducing function and realization of `coll`.
  Realization can be happened on threads other than caller."
  [xf rfs coll & {}]
  (let [in-coll (sequence xf coll)]
    (->> rfs
         (mapv (fn to-future [rf] (future (transduce identity rf in-coll))))
         (mapv deref))))

(defn mtransduce-serial
  "Simple implementation of mtransduce. Whole process is done in caller thread."
  [xf rfs coll & {}]
  (transduce xf (multi-rf rfs) coll))

(defn mtransduce
  "Just like `transduce` with multiple reducing functions.
   `xf` is a transducer applied to input sequence `coll`.
   `rfs` is a vector or a map of reducing functions.
   Returns a vector of results for vector/map rfs.
  Implementation can be specified by `:mode` option."
  [xf rfs coll & {:keys [mode] :or {mode :async} :as option}]
  (let [impl (case mode
               :serial mtransduce-serial
               :burst mtransduce-future
               :async mtransduce-async)]
    (if (map? rfs)
      (zipmap (keys rfs) (apply impl xf (vals rfs) coll (apply concat option)))
      (vec (apply impl xf rfs coll (apply concat option))))))

(defn max
  "Reducing fuction for computing the max of long values.
  Note that `Long/MIN_VALUE` is converted to `nil`."
  ([] Long/MIN_VALUE)
  ([x] (if (= x Long/MIN_VALUE) nil x))
  ([r x] (clojure.core/max r x)))

(defn min
  "Reducing fuction for computing the min of long values.
  Note that `Long/MAX_VALUE` is converted to `nil`."
  ([] Long/MAX_VALUE)
  ([x] (if (= x Long/MAX_VALUE) nil x))
  ([r x] (clojure.core/min r x)))

(defn greatest-by
  "Returns a reducing function extracting the greatest element using given comparator."
  ([] (greatest-by identity compare))
  ([key-fn] (greatest-by key-fn compare))
  ([key-fn comparator]
   (fn greatest-inner
     ([] nil)
     ([x] x)
     ([r x] (if (pos? (comparator (key-fn r) (key-fn x))) r x)))))

(def
  ^{:doc "Reducing function extracting the greatest element by `compare`."}
  greatest (greatest-by))

(defn least-by
  "Returns a reducing function extracting the least element using given comparator."
  ([] (least-by identity compare))
  ([key-fn] (least-by key-fn compare))
  ([key-fn comparator]
   (fn least-inner
     ([] nil)
     ([x] x)
     ([r x] (if (nil? r) x (if (pos? (comparator (key-fn r) (key-fn x))) x r))))))

(def
  ^{:doc "Reducing function extracting the least element by `compare`."}
  least (least-by))

(defn first
  "Reducing function returns the first element."
  ([] nil)
  ([x] x)
  ([r x] (reduced x)))

(defn ffirst
  "Reducing function returns the first of the first element."
  ([] nil)
  ([x] x)
  ([r x] (reduced (clojure.core/first x))))

(defn second []
  "Reducing function returns the second element."
  ((comp (take 2) (drop 1)) first))

(defn last
  "Reducing function returns the last element."
  ([] nil)
  ([x] x)
  ([r x] x))

(defn count
  "Reducing function returns the number of elements."
  ([] 0)
  ([x] x)
  ([r x] (inc r)))

(defn into
  "Returns a reducing function which returns a new coll with elements conjoined."
  [to]
  (if (instance? clojure.lang.IEditableCollection to)
      (fn into-editable
        ([] (transient to))
        ([x] (with-meta (persistent! x) (meta to)))
        ([r x] (conj! r x)))
      (fn into-non-editable
        ([] to)
        ([x] x)
        ([r x] (conj r x)))))

(defn frequencies
  "Reducing function returns a map from distinct elements to its number of occurrence."
  ([]
   (transient {}))
  ([x]
   (persistent! x))
  ([r x]
   (assoc! r x (if-let [old (r x)] (inc old) 1))))

(defn group-by
  "Returns a reducing function acts like `group-by`."
  [key-fn]
  (fn group-by-inner
    ([] (transient {}))
    ([x] (clojure.core/into {} (map (fn [[k v]] [k (persistent! v)])) (persistent! x)))
    ([r x]
     (let [k (key-fn x)]
       (assoc! r k (if-let [old (r k)] (conj! old x) (transient [x])))))))

(defn frequenciesv
  "Like `frequencies`, but specialized for counting long numbers up to `size`."
  [size]
  (fn frequencies-vec-inner
    ([] (transient (vec (repeat size 0))))
    ([x] (persistent! x))
    ([r x] (assoc! r x (inc (get r x))))))

