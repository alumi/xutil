(ns xutil.xf
  "Transducers"
  (:refer-clojure :exclude [max min first ffirst second next last count into
                            frequencies group-by juxt if case])
  (:require [clojure.core :as core]))

(defn preserving-reduced
  "Returns a reducing function wrapping the given reducing function.
  It doesn't init or finalize. Wrapping reduced value in reduced."
  [rf]
  (fn preserving-reduced-rf
    ([])
    ([result]
     result)
    ([result input]
     (let [ret (rf result input)]
       (if (reduced? ret)
         (reduced ret)
         ret)))))

(defn juxt
  "Combines multiple transducers and returns a new transducer."
  [& xfs]
  (fn juxt-xf [rf]
    (let [rf' (preserving-reduced rf)
          rfs (volatile! (core/into #{} (map #(% rf')) xfs))]
      (fn juxt-xf-xf
        ([]
         (rf))
        ([result]
         (rf (reduce (fn juxt-xf-xf-rf-1 [a f] (f a)) result @rfs)))
        ([result input]
         (let [ret (reduce
                    (fn juxt-xf-xf-rf-2 [a f]
                      (let [ret (f a input)]
                        (if (reduced? ret)
                          (do
                            (vswap! rfs disj f)
                            (f @ret))
                          ret)))
                    result
                    @rfs)]
           (if (zero? (core/count @rfs))
             (ensure-reduced ret)
             ret)))))))

(defn rf->xf
  "Creating a new transducer from a reducing function."
  [rf]
  (fn rf->xf-xf [rf']
    (let [state (volatile! (rf))]
      (fn rf->xf-xf-rf
        ([]
         (rf'))
        ([result]
         (rf' (unreduced (rf' result (rf (unreduced @state))))))
        ([result input]
         (if (reduced? (vswap! state rf input))
           (reduced result)
           result))))))

(defn max
  "Transducer which yields one max value."
  [rf]
  (let [state (volatile! ::none)]
    (fn max-rf
      ([]
       (rf))
      ([result]
       (rf (unreduced (rf result (when-not (= @state ::none) @state)))))
      ([result input]
       (vswap! state (fn max-rf-2 [s]
                       (if (= s ::none) input (core/max s input))))
       result))))

(defn min
  "Transducer which yields one min value."
  [rf]
  (let [state (volatile! ::none)]
    (fn min-rf
      ([]
       (rf))
      ([result]
       (rf (unreduced (rf result (when-not (= @state ::none) @state)))))
      ([result input]
       (vswap! state (fn min-rf-2 [s]
                       (if (= s ::none) input (core/min s input))))
       result))))

(defn greatest-by
  "Returns a transducer which yields one greatest value compared with key-fn
  and comparator."
  ([]
   (greatest-by identity compare))
  ([key-fn]
   (greatest-by key-fn compare))
  ([key-fn comparator]
   (fn greatest-by-xf [rf]
     (let [state (volatile! ::none)]
       (fn greatest-by-rf
         ([]
          (rf))
         ([result]
          (rf (unreduced (rf result (when-not (= @state ::none) @state)))))
         ([result input]
          (vswap!
           state
           (fn greatest-by-rf-2 [s]
             (if (or (= s ::none)
                     (not (pos? (comparator (key-fn s) (key-fn input)))))
               input
               s)))
          result))))))

(defn least-by
  "Returns a transducer which yields one least value compared with key-fn
  and comparator."
  ([]
   (least-by identity compare))
  ([key-fn]
   (least-by key-fn compare))
  ([key-fn comparator]
   (fn least-by-xf [rf]
     (let [state (volatile! ::none)]
       (fn least-by-rf
         ([]
          (rf))
         ([result]
          (rf (unreduced (rf result (when-not (= @state ::none) @state)))))
         ([result input]
          (vswap!
           state
           (fn least-by-rf-2 [s]
             (if (or (= s ::none) (pos? (comparator (key-fn s) (key-fn input))))
               input
               s)))
          result))))))

(defn first
  "Transducer which yields the first element."
  [rf]
  (fn first-rf
    ([]
     (rf))
    ([result]
     (rf result))
    ([result input]
     (reduced (rf result input)))))

(defn ffirst
  "Transducer which yields the first of the first element."
  [rf]
  (fn ffirst-rf
    ([] (rf))
    ([result] (rf result))
    ([result input]
     (reduced (rf result (core/first input))))))

(defn second
  "Transducer which yields the second element."
  [rf]
  (let [state (volatile! ::none)]
    (fn second-rf
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (if (= @state ::none)
         (do
           (vreset! state ::first)
           result)
         (reduced (rf result input)))))))

(defn next
  "Transducer which drops the first element or nil if empty."
  [rf]
  (let [state (volatile! ::none)]
    (fn next-rf
      ([]
       (rf))
      ([result]
       (if (= state ::none)
         (rf (unreduced (rf result nil)))
         (rf result)))
      ([result input]
       (if (= @state ::none)
         (do
           (vreset! state ::first)
           result)
         (rf result input))))))

(defn last
  "Transducer which yelds the last element."
  [rf]
  (let [state (volatile! ::none)]
    (fn last-rf
      ([]
       (rf))
      ([result]
       (rf (unreduced (rf result (when-not (= @state ::none) @state)))))
      ([result input]
       (vreset! state input)
       result))))

(defn count
  "Transducer which yields a number of elements."
  [rf]
  (let [state (volatile! 0)]
    (fn count-rf
      ([]
       (rf))
      ([result]
       (rf (unreduced (rf result @state))))
      ([result input]
       (vswap! state inc)
       result))))

(defn into
  "Transducer which yields a new collection consisting of to-coll
  with all of the lements conjoined."
  [to]
  (if (instance? clojure.lang.IEditableCollection to)
    (fn into-editable-xf [rf]
      (let [state (volatile! (transient to))]
        (fn into-editable-xf-rf
          ([]
           (rf))
          ([result]
           (->> to
                meta
                (with-meta (persistent! @state))
                (rf result)
                unreduced
                rf))
          ([result input]
           (vswap! state conj! input)
           result))))
    (fn into-non-editable-xf [rf]
      (let [state (volatile! to)]
        (fn into-non-editable-xf-rf
          ([] (rf))
          ([result]
           (rf (unreduced (rf result @state))))
          ([result input]
           (vswap! state conj input)
           result))))))

(defn frequencies
  "Transducer which yields a map of frequencies."
  [rf]
  (let [state (volatile! (transient {}))]
    (fn frequencies-rf
      ([]
       (rf))
      ([result]
       (rf (unreduced (rf result (persistent! @state)))))
      ([result input]
       (vswap!
        state
        (fn frequencies-rf-2 [s]
          (assoc! s input (inc (or (get s input) 0)))))
       result))))

(defn group-by
  "Returns a transducer which yields key-value pairs."
  ([key-fn]
   (group-by key-fn (into [])))
  ([key-fn xf]
   (fn group-by-xf [rf]
     (let [state (volatile! (transient {}))]
       (fn group-by-xf-rf
         ([] (rf))
         ([result]
          (rf
           (reduce
            (fn group-by-xf-rf-1 [r rf']
              (rf' r))
            result
            (vals (persistent! @state)))))
         ([result input]
          (let [k (key-fn input)
                rf' (or
                     (get @state k)
                     (-> (fn group-by-xf-rf-rf
                            ([])
                            ([result'] result')
                            ([result' input']
                             ((preserving-reduced rf) result' [k input'])))
                         xf
                         (as-> f (vswap! state assoc! k f))
                         (get k)))
                ret (rf' result input)]
            (if (reduced? ret)
              (if (reduced? @ret)
                @ret
                (do
                  (vswap! state assoc! k (fn ([]) ([r] r) ([r _] r)))
                  (rf' @ret)))
              ret))))))))

(defn if-xf
  "Returns a stateful transducer acts like `if`. Evaluates pred on items and
  invoke either one of reducing functions transformed with then-xf or else-xf."
  [pred then-xf else-xf]
  (fn if-xf-xf [rf]
    (let [rf' (preserving-reduced rf)
          then-rf (volatile! (then-xf rf'))
          else-rf (volatile! (else-xf rf'))]
      (fn if-xf-rf
        ([] (rf))
        ([result] (rf (reduce (fn if-xf-rf-rf [r f] (if f (f r) r)) result [@then-rf @else-rf])))
        ([result input]
         (let [p (pred input)]
           (if-let [f (if p @then-rf @else-rf)]
             (let [ret (f result input)]
               (if (and (reduced? ret) (if p @else-rf @then-rf))
                 (do
                   (vreset! (if p then-rf else-rf) nil)
                   (f @ret))
                 ret))
             result)))))))

(defn case-xf
  [case-fn & cases]
  (fn [rf]
    (let [rf' (preserving-reduced rf)
          cases (->> [::default (core/last cases)]
                     (concat (butlast cases))
                     (if (even? (core/count cases)) cases))
          case-rfs (->> cases
                        (core/into
                         {}
                         (comp
                          (partition-all 2)
                          (map (fn case-xf-make-rf [[k xf]]
                                 [k (xf rf')])))))
          state (volatile! (transient case-rfs))
          ks (core/into #{} (map core/first) (partition 2 cases))]
      (fn case-xf-rf
        ([] (rf))
        ([result]
         (rf
          (reduce
           (fn case-xf-rf-1 [r k]
             ((get case-rfs k) r))
           result
           (map core/first (partition 2 cases)))))
        ([result input]
         (let [k (case-fn input)
               rf' (get @state (get ks k ::default))
               ret (rf' result input)]
           (if (reduced? ret)
             (do
               (vswap! state assoc! k (fn ([]) ([r] r) ([r _] r))))
             ret)))))))
