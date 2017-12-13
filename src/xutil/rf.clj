(ns xutil.rf
  "Reducing functions"
  (:refer-clojure :exclude [max min first ffirst second next last count
                            into frequencies group-by juxt])
  (:require [clojure.core :as core]))

(defn make-rf
  "Returns a reducing function with initial value and finalization."
  ([f]
   (make-rf f (f) identity))
  ([f init]
   (make-rf f init identity))
  ([f init final]
   (fn make-rf-inner
     ([] init)
     ([result] (final result))
     ([result input] (f result input)))))

(defn juxt
  "Combines multiple reducing function."
  [& rfs]
  (fn juxt-inner
    ([] (mapv (fn juxt-1 [f] (f)) rfs))
    ([result] (mapv (fn juxt-2 [f a] (f (unreduced a))) rfs result))
    ([result input]
     (let [ret (mapv
                (fn juxt-3 [f r] (if (reduced? r) r (f r input)))
                rfs
                result)]
       (if (every? reduced? ret)
         (reduced (mapv unreduced ret))
         ret)))))

(defn finalize
  "Returns a reducing function with finalization."
  [rf final]
  (fn finalize-inner
    ([] (rf))
    ([result] (final (rf result)))
    ([result input] (rf result input))))

(defn max
  "Reducing fuction for computing the max of long values."
  ([] ::none)
  ([result] (when-not (= result ::none) result))
  ([result input] (if (= result ::none) input (core/max result input))))

(defn min
  "Reducing fuction for computing the min of long values."
  ([] ::none)
  ([result] (when-not (= result ::none) result))
  ([result input] (if (= result ::none) input (core/min result input))))

(defn greatest-by
  "Returns a reducing function extracting the greatest element using given
  comparator."
  ([] (greatest-by identity compare))
  ([key-fn] (greatest-by key-fn compare))
  ([key-fn comparator]
   (fn greatest-inner
     ([] ::none)
     ([result] (when-not (= result ::none) result))
     ([result input]
      (if (= result ::none)
        input
        (if (pos? (comparator (key-fn result) (key-fn input)))
          result
          input))))))

(def
  ^{:doc "Reducing function extracting the greatest element by `compare`."}
  greatest (greatest-by))

(defn least-by
  "Returns a reducing function extracting the least element using given
  comparator."
  ([] (least-by identity compare))
  ([key-fn] (least-by key-fn compare))
  ([key-fn comparator]
   (fn least-inner
     ([] ::none)
     ([result] (when-not (= result ::none) result))
     ([result input]
      (if (= result ::none)
        input
        (if (pos? (comparator (key-fn result) (key-fn input)))
          input
          result))))))

(def
  ^{:doc "Reducing function extracting the least element by `compare`."}
  least (least-by))

(defn first
  "Reducing function returns the first element."
  ([] nil)
  ([result] result)
  ([result input] (reduced input)))

(defn ffirst
  "Reducing function returns the first of the first element."
  ([] nil)
  ([result] result)
  ([result input] (reduced (core/first input))))

(defn second
  "Reducing function returns the second element."
  ([] ::none)
  ([result] (when-not (#{::none ::first} result) result))
  ([result input] (case result ::none ::first ::first (reduced input))))

(defn next
  ([] ::none)
  ([result] (if (= ::none result) nil (seq (persistent! result))))
  ([result input] (if (= ::none result) (transient []) (conj! result input))))

(defn last
  "Reducing function returns the last element."
  ([] nil)
  ([result] result)
  ([result input] input))

(defn count
  "Reducing function returns the number of elements."
  ([] 0)
  ([result] result)
  ([result input] (inc result)))

(defn into
  "Returns a reducing function which returns a new consisting of to-coll with
  all of the elements conjoined."
  [to]
  (if (instance? clojure.lang.IEditableCollection to)
      (fn into-editable
        ([] (transient to))
        ([result] (with-meta (persistent! result) (meta to)))
        ([result input] (conj! result input)))
      (fn into-non-editable
        ([] to)
        ([result] result)
        ([result input] (conj result input)))))

(defn frequencies
  "Reducing function returns a map from distinct elements to the number of
  occurrence."
  ([]
   (transient {}))
  ([result]
   (persistent! result))
  ([result input]
   (assoc! result input (inc (or (result input) 0)))))

(defn frequenciesv
  "Like `frequencies`, but specialized for counting long numbers up to `size`."
  [size]
  (fn frequencies-vec-inner
    ([] (transient (vec (repeat size 0))))
    ([result] (persistent! result))
    ([result input] (assoc! result input (inc (get result input))))))

(defn group-by
  "Returns a reducing function acts like `group-by`. Second argument `rf` is a
  reducing function applied to each group. Default is (into [])."
  ([key-fn]
   (group-by key-fn (into [])))
  ([key-fn rf]
   (fn group-by-inner
     ([] (transient {}))
     ([result]
      (core/into
       {}
       (map (fn group-by-inner-finish [[k v]] [k (rf (unreduced v))]))
       (persistent! result)))
     ([result input]
      (let [k (key-fn input)
            old (or (result k) (rf))]
        (if (reduced? old)
          result
          (assoc! result k (rf old input))))))))

(defn if-rf
  "Returns a reducing function acts like `if`. It produces a vector of two
  reduced results of given reducing functions. Evaluates pred on items and
  select either one of then-rf or else-rf."
  [pred then-rf else-rf]
  (fn if-rf-rf
    ([] [(then-rf) (else-rf)])
    ([[t-result e-result]]
     [(then-rf (unreduced t-result)) (else-rf (unreduced e-result))])
    ([[t-result e-result] input]
     (let [p (pred input)
           result (if p t-result e-result)
           ret (if (reduced? result) result ((if p then-rf else-rf) result input))]
       (if (and (reduced? ret) (reduced? (if p e-result t-result)))
         (reduced (if p [@ret @e-result] [@t-result @ret]))
         (if p [ret e-result] [t-result ret]))))))

(defn case-rf
  "Returns a reducing function which applies different reducing functions
  determined by returned value of case-fn on elements. cases must be a sequence
  of dispatching values and reducing functions."
  [case-fn & cases]
  (let [case-rfs (->> [::default (core/last cases)]
                      (concat (butlast cases))
                      (if (even? (core/count cases)) cases)
                      (apply array-map))
        ks (disj (core/into #{} (keys case-rfs)) ::default)]
    (fn case-rf-rf
      ([]
       (core/into {} (map #(vector (key %) ((val %)))) case-rfs))
      ([result]
       (core/into {} (map #(vector (key %) ((case-rfs (key %)) (unreduced (val %))))) result))
      ([result input]
       (let [case-val (get ks (case-fn input) ::default)
             case-result (get result case-val)]
         (if (reduced? case-result)
           result
           (let [ret (update result case-val (case-rfs case-val) input)]
             (if (every? (comp reduced? val) ret)
               (reduced ret)
               ret))))))))
