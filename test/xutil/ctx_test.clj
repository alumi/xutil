(ns xutil.ctx-test
  (:use [midje.sweet])
  (:require [xutil.ctx :as ctx]
            [xutil.rf :as rf]))

(tabular
 (facts
  "mtransduce-async"
  (apply ctx/mtransduce-async (map inc) [+ *] (range 10) (apply concat ?opts)) => [55 3628800])
 ?opts
 {}
 {:buffer 1}
 {:buffer 5}
 {:buffer 10}
 {:part 1}
 {:part 5}
 {:part 10}
 {:buffer 1 :part 1})

(facts
 "mtransduce-future"
 (ctx/mtransduce-future (map inc) [+ *] (range 10)) => [55 3628800])

(facts
 "mtransduce-serial"
 (ctx/mtransduce-serial (map inc) [+ *] (range 10)) => [55 3628800])

(facts
 "mtransduce"
 (ctx/mtransduce identity [+] (range 10)) => [45]
 (ctx/mtransduce identity {:sum +} (range 10)) => {:sum 45}
 (ctx/mtransduce cat [+] [(range 10) (range 10)]) => [90]
 (ctx/mtransduce (mapcat range) [+] [10 10]) => [90]
 (ctx/mtransduce cat [((map inc) +)] [(range 10) (range 10)]) => [110]
 (ctx/mtransduce (map inc) {:sum +} (range 10)) => {:sum 55}
 (ctx/mtransduce identity {:count ((map (constantly 1)) +)} (range 10)) => {:count 10}
 (ctx/mtransduce identity [+] (range 10) :mode :burst) => [45]
 (ctx/mtransduce identity [+ + + + + +] (range 10) :mode :serial) => [45 45 45 45 45 45]
 (ctx/mtransduce identity [+ + + + + +] (range 10) :mode :burst) => [45 45 45 45 45 45]
 (ctx/mtransduce identity [+ + + + + +] (range 10) :mode :async :buffer 3 :part 2) => [45 45 45 45 45 45]
 (ctx/mtransduce (take 10000000) [+] (iterate inc 0)) => [49999995000000]
 (let [i (take 10000000 (iterate inc 0))] [(reduce + i) (count i)]) => throws ;; OOM with Xmx64m option
 (transduce (take 10000000) + (iterate inc 0)) => 49999995000000
 (ctx/mtransduce (take 10000000) [+ rf/count] (iterate inc 0) :mode :serial) => [49999995000000 10000000]
 (ctx/mtransduce (take 10000000) [+ rf/count] (iterate inc 0) :mode :burst) => [49999995000000 10000000]
 (ctx/mtransduce (take 10000000) [+ rf/count] (iterate inc 0) :mode :async) => [49999995000000 10000000])
