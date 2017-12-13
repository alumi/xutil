(ns xutil.ctx
  "Reducing contexts"
  (:require [clojure.core.async :as a]
            [xutil.rf :as rf]))

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
  (transduce xf (apply rf/juxt rfs) coll))

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
