(ns xutil.core-test
  (:use [midje.sweet])
  (:require [xutil.core :as x]))

(facts
 "make-rf"
 (let [minus (x/make-rf - 0)]
   (-) => throws
   (minus) => 0
   (- 1) => -1
   (minus 1) => 1
   (- 0 10) => -10
   (minus 0 10) => -10
   (transduce identity - (range 10)) => throws
   (transduce identity minus (range 10)) => -45
   (transduce identity ((map inc) minus) (range 10)) => -55)
 (let [plus (x/make-rf + 100 -)]
   (+) => 0
   (plus) => 100
   (+ 1) => 1
   (plus 1) => -1
   (+ 1 2) => 3
   (plus 1 2) => 3
   (transduce identity + (range 10)) => 45
   (transduce identity plus (range 10)) => -145))

(facts
 "multi-rf"
 (let [mrf (x/multi-rf [+ *])]
   (mrf) => [0 1]
   (mrf [10 10]) => [10 10]
   (mrf [10 10] 5) => [15 50]
   (transduce identity mrf (range 10)) => [45 0]))

(facts
 "finalize-rf"
 (let [frf (x/finalize-rf + str)]
   (frf) => 0
   (frf 1) => "1"
   (frf 1 2) => 3
   (transduce identity frf (range 10)) => "45"))

(tabular
 (facts
  "mtransduce-async"
  (apply x/mtransduce-async (map inc) [+ *] (range 10) (apply concat ?opts)) => [55 3628800])
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
 (x/mtransduce-future (map inc) [+ *] (range 10)) => [55 3628800])

(facts
 "mtransduce-serial"
 (x/mtransduce-serial (map inc) [+ *] (range 10)) => [55 3628800])

(facts
 "mtransduce"
 (x/mtransduce identity [+] (range 10)) => [45]
 (x/mtransduce identity {:sum +} (range 10)) => {:sum 45}
 (x/mtransduce cat [+] [(range 10) (range 10)]) => [90]
 (x/mtransduce (mapcat range) [+] [10 10]) => [90]
 (x/mtransduce cat [((map inc) +)] [(range 10) (range 10)]) => [110]
 (x/mtransduce (map inc) {:sum +} (range 10)) => {:sum 55}
 (x/mtransduce identity {:count ((map (constantly 1)) +)} (range 10)) => {:count 10}
 (x/mtransduce identity [+] (range 10) :mode :burst) => [45]
 (x/mtransduce identity [+ + + + + +] (range 10) :mode :serial) => [45 45 45 45 45 45]
 (x/mtransduce identity [+ + + + + +] (range 10) :mode :burst) => [45 45 45 45 45 45]
 (x/mtransduce identity [+ + + + + +] (range 10) :mode :async :buffer 3 :part 2) => [45 45 45 45 45 45]
 (x/mtransduce (take 10000000) [+] (iterate inc 0)) => [49999995000000]
 (let [i (take 10000000 (iterate inc 0))] [(reduce + i) (count i)]) => throws ;; OOM with Xmx64m option
 (transduce (take 10000000) + (iterate inc 0)) => 49999995000000
 (x/mtransduce (take 10000000) [+ x/count] (iterate inc 0) :mode :serial) => [49999995000000 10000000]
 (x/mtransduce (take 10000000) [+ x/count] (iterate inc 0) :mode :burst) => [49999995000000 10000000]
 (x/mtransduce (take 10000000) [+ x/count] (iterate inc 0) :mode :async) => [49999995000000 10000000])

(facts
 "max"
 (transduce identity x/max []) => nil
 (transduce identity x/max [0]) => 0
 (transduce identity x/max [0 1]) => 1
 (transduce (map inc) x/max (shuffle (range 100))) => 100
 (x/mtransduce identity [x/max] []) => [nil]
 (x/mtransduce identity [x/max] [0]) => [0]
 (x/mtransduce identity [x/max] [0 1]) => [1]
 (x/mtransduce (map inc) [x/max] (shuffle (range 100))) => [100])

(facts
 "min"
 (transduce identity x/min []) => nil
 (transduce identity x/min [0]) => 0
 (transduce identity x/min [0 1]) => 0
 (transduce (map inc) x/min (shuffle (range 100))) => 1
 (x/mtransduce identity [x/min] []) => [nil]
 (x/mtransduce identity [x/min] [0]) => [0]
 (x/mtransduce identity [x/min] [0 1]) => [0]
 (x/mtransduce (map inc) [x/min] (shuffle (range 100))) => [1])

(facts
 "greatest-by"
 (transduce identity (x/greatest-by) []) => nil
 (transduce identity (x/greatest-by) [0]) => 0
 (transduce identity (x/greatest-by) [0 1]) => 1
 (transduce identity (x/greatest-by) [[0 2] [0 1]]) => [0 2]
 (transduce identity (x/greatest-by) [[0 2] [1 0] [0 1]]) => [1 0]
 (transduce (map second) (x/greatest-by) [[0 2] [1 0] [0 1]]) => 2
 (transduce identity (x/greatest-by second) [[0 2] [1 0] [0 1]]) => [0 2]
 (transduce identity x/greatest [[0 2] [1 0] [0 1]]) => [1 0])

(facts
 "least-by"
 (transduce identity (x/least-by) []) => nil
 (transduce identity (x/least-by) [0]) => 0
 (transduce identity (x/least-by) [0 1]) => 0
 (transduce identity (x/least-by) [[0 2] [0 1]]) => [0 1]
 (transduce identity (x/least-by) [[0 2] [1 0] [0 1]]) => [0 1]
 (transduce (map second) (x/least-by) [[0 2] [1 0] [0 1]]) => 0
 (transduce identity (x/least-by second) [[0 2] [1 0] [0 1]]) => [1 0]
 (transduce identity x/least [[0 2] [1 0] [0 1]]) => [0 1])

(facts
 "first"
 (transduce identity x/first []) => nil
 (transduce identity x/first [0]) => 0
 (transduce identity x/first [0 1]) => 0
 (transduce identity x/first [[0 1] [2 3]]) => [0 1])

(facts
 "ffirst"
 (transduce identity x/ffirst []) => nil
 (transduce identity x/ffirst [0]) => throws
 (transduce identity x/ffirst [[]]) => nil
 (transduce identity x/ffirst [[0 1] [2 3]]) => 0)

(facts
 "second"
 (transduce identity x/second []) => nil
 (transduce identity x/second [0]) => nil
 (transduce identity x/second [0 1]) => 1
 (transduce identity x/second [0 1 2]) => 1)

(facts
 "last"
 (transduce identity x/last []) => nil
 (transduce identity x/last [0]) => 0
 (transduce identity x/last [0 1]) => 1)

(facts
 "count"
 (transduce identity x/count []) => 0
 (transduce identity x/count [0]) => 1
 (transduce identity x/count [0 1]) => 2
 (transduce identity x/count [[0 1] [2 3]]) => 2
 (transduce cat x/count [[0 1] [2 3]]) => 4)

(facts
 "into"
 (transduce identity (x/into [-1]) (range 3)) => [-1 0 1 2]
 (transduce cat (x/into #{}) [(range 2) (range 5)]) => #{0 1 2 3 4}
 (transduce identity (x/into {}) [[:a 1] [:b 2] [:c 3]]) => {:a 1 :b 2 :c 3}
 (transduce cat ((filter odd?) (x/into #{})) [(range 2) (range 5)]) => #{1 3}
 (transduce identity (x/into (with-meta [1] {:doc "test"})) (range 3)) => [1 0 1 2]
 (transduce identity (x/finalize-rf (x/into (with-meta [1] {:doc "test"})) meta) (range 3)) => {:doc "test"})

(facts
 "frequencies"
 (transduce identity x/frequencies []) => {}
 (transduce identity x/frequencies (range 4)) => {0 1, 1 1, 2 1, 3 1}
 (transduce (map #(quot % 4)) x/frequencies (range 10)) => {0 4, 1 4, 2 2}
 (transduce (map #(quot % 4)) x/frequencies (range 100)) => (frequencies (map #(quot % 4) (range 100))))

(facts
 "frequenciesv"
 (transduce identity (x/frequenciesv 1) []) => [0]
 (transduce identity (x/frequenciesv 1) [0]) => [1]
 (transduce identity (x/frequenciesv 1) [1]) => throws
 (transduce identity (x/frequenciesv 10) (range 10)) => [1 1 1 1 1 1 1 1 1 1]
 (transduce cat (x/frequenciesv 10) [(range 3) (range 4) (range 2)]) => [3 3 2 1 0 0 0 0 0 0])

(facts
 "group-by"
 (transduce identity (x/group-by identity) (range 5)) => {0 [0] 1 [1] 2 [2] 3 [3] 4 [4]}
 (transduce identity (x/group-by odd?) (range 5)) => {true [1 3] false [0 2 4]}
 (transduce identity (x/group-by odd?) (range 10)) => (group-by odd? (range 10))
 (transduce identity (x/group-by odd? (x/into [])) (range 5)) => {true [1 3] false [0 2 4]}
 (transduce identity (x/group-by odd? +) (range 5)) => {true 4 false 6}
 (transduce identity (x/group-by odd? (x/multi-rf [+ *])) (range 5)) => {true [4 3] false [6 0]}
 (transduce identity (x/group-by odd? x/count) (range 10)) => (frequencies (map odd? (range 10))))

