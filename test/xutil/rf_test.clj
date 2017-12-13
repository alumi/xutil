(ns xutil.rf-test
  (:require [xutil.core :as x]
            [xutil.rf :as rf]
            [midje.sweet :refer :all]))

(facts
 "make-rf"
 (let [minus (rf/make-rf - 0)]
   (-) => throws
   (minus) => 0
   (- 1) => -1
   (minus 1) => 1
   (- 0 10) => -10
   (minus 0 10) => -10
   (transduce identity - (range 10)) => throws
   (transduce identity minus (range 10)) => -45
   (transduce identity ((map inc) minus) (range 10)) => -55)
 (let [plus (rf/make-rf + 100 -)]
   (+) => 0
   (plus) => 100
   (+ 1) => 1
   (plus 1) => -1
   (+ 1 2) => 3
   (plus 1 2) => 3
   (transduce identity + (range 10)) => 45
   (transduce identity plus (range 10)) => -145))

(facts
 "juxt"
 (let [mrf (rf/juxt + *)]
   (mrf) => [0 1]
   (mrf [10 10]) => [10 10]
   (mrf [10 10] 5) => [15 50]
   (transduce identity mrf (range 10)) => [45 0]))

(facts
 "finalize"
 (let [frf (rf/finalize + str)]
   (frf) => 0
   (frf 1) => "1"
   (frf 1 2) => 3
   (transduce identity frf (range 10)) => "45"))

(facts
 "max"
 (transduce identity rf/max []) => nil
 (transduce identity rf/max [0]) => 0
 (transduce identity rf/max [0 1]) => 1
 (transduce (map inc) rf/max (shuffle (range 100))) => 100
 (x/mtransduce identity [rf/max] []) => [nil]
 (x/mtransduce identity [rf/max] [0]) => [0]
 (x/mtransduce identity [rf/max] [0 1]) => [1]
 (x/mtransduce (map inc) [rf/max] (shuffle (range 100))) => [100])

(facts
 "min"
 (transduce identity rf/min []) => nil
 (transduce identity rf/min [0]) => 0
 (transduce identity rf/min [0 1]) => 0
 (transduce (map inc) rf/min (shuffle (range 100))) => 1
 (x/mtransduce identity [rf/min] []) => [nil]
 (x/mtransduce identity [rf/min] [0]) => [0]
 (x/mtransduce identity [rf/min] [0 1]) => [0]
 (x/mtransduce (map inc) [rf/min] (shuffle (range 100))) => [1])

(facts
 "greatest-by"
 (transduce identity (rf/greatest-by) []) => nil
 (transduce identity (rf/greatest-by) [0]) => 0
 (transduce identity (rf/greatest-by) [0 1]) => 1
 (transduce identity (rf/greatest-by) [[0 2] [0 1]]) => [0 2]
 (transduce identity (rf/greatest-by) [[0 2] [1 0] [0 1]]) => [1 0]
 (transduce (map second) (rf/greatest-by) [[0 2] [1 0] [0 1]]) => 2
 (transduce identity (rf/greatest-by second) [[0 2] [1 0] [0 1]]) => [0 2]
 (transduce identity rf/greatest [[0 2] [1 0] [0 1]]) => [1 0])

(facts
 "least-by"
 (transduce identity (rf/least-by) []) => nil
 (transduce identity (rf/least-by) [0]) => 0
 (transduce identity (rf/least-by) [0 1]) => 0
 (transduce identity (rf/least-by) [[0 2] [0 1]]) => [0 1]
 (transduce identity (rf/least-by) [[0 2] [1 0] [0 1]]) => [0 1]
 (transduce (map second) (rf/least-by) [[0 2] [1 0] [0 1]]) => 0
 (transduce identity (rf/least-by second) [[0 2] [1 0] [0 1]]) => [1 0]
 (transduce identity rf/least [[0 2] [1 0] [0 1]]) => [0 1])

(tabular
 (facts
  "first"
  (transduce ?xf rf/first ?coll) => ?expected
  (transduce identity (?xf rf/first) ?coll) => ?expected)
 ?xf ?coll ?expected
 identity [] nil
 identity [0] 0
 identity [0 1] 0
 identity [[0 1] [2 3]] [0 1]
 (take 0) [0] nil
 (take 2) [0] 0
 (take 2) [0 1] 0
 (take 2) [0 1 2] 0
 (drop 2) [0 1 2] 2
 (drop 3) [0 1 2] nil)

(facts
 "first"
 (let [r (reduced 0)] (transduce (take 2) rf/first [r]) => r))

(tabular
 (facts
  "ffirst"
  (transduce ?xf rf/ffirst ?coll) => ?expected
  (transduce identity (?xf rf/ffirst) ?coll) => ?expected)
 ?xf ?coll ?expected
 identity [] nil
 identity [0] throws
 identity [[]] nil
 identity [[0 1] [2 3]] 0
 (take 0) [[0]] nil
 (take 1) [[0]] 0
 (drop 1) [[0] [1]] 1)

(tabular
 (facts
  "second"
  (transduce ?xf rf/second ?coll) => ?expected
  (transduce identity (?xf rf/second) ?coll) => ?expected)
 ?xf ?coll ?expected
 identity [] nil
 identity [0] nil
 identity [0 1] 1
 identity [0 1 2] 1
 (take 0) [] nil
 (take 1) [0] nil
 (take 2) [0] nil
 (take 2) [0 1] 1
 (drop 1) [0 1] nil
 (drop 1) [0 1 2] 2)

(facts
 "next"
 (transduce identity rf/next []) => nil
 (transduce identity rf/next [0]) => nil
 (transduce identity rf/next [0 1]) => [1]
 (transduce identity rf/next [0 1 2]) => [1 2]
 (transduce (take 1) rf/next [0 1 2]) => nil
 (transduce (take 2) rf/next [0 1 2]) => [1])

(facts
 "last"
 (transduce identity rf/last []) => nil
 (transduce identity rf/last [0]) => 0
 (transduce identity rf/last [0 1]) => 1)

(facts
 "count"
 (transduce identity rf/count []) => 0
 (transduce identity rf/count [0]) => 1
 (transduce identity rf/count [0 1]) => 2
 (transduce identity rf/count [[0 1] [2 3]]) => 2
 (transduce cat rf/count [[0 1] [2 3]]) => 4)

(facts
 "into"
 (transduce identity (rf/into [-1]) (range 3)) => [-1 0 1 2]
 (transduce cat (rf/into #{}) [(range 2) (range 5)]) => #{0 1 2 3 4}
 (transduce identity (rf/into {}) [[:a 1] [:b 2] [:c 3]]) => {:a 1 :b 2 :c 3}
 (transduce cat ((filter odd?) (rf/into #{})) [(range 2) (range 5)]) => #{1 3}
 (transduce identity (rf/into (with-meta [1] {:doc "test"})) (range 3)) => [1 0 1 2]
 (transduce identity (rf/finalize (rf/into (with-meta [1] {:doc "test"})) meta) (range 3)) => {:doc "test"})

(facts
 "frequencies"
 (transduce identity rf/frequencies []) => {}
 (transduce identity rf/frequencies (range 4)) => {0 1, 1 1, 2 1, 3 1}
 (transduce (take 4) rf/frequencies (range 10)) => {0 1, 1 1, 2 1, 3 1}
 (transduce (map #(quot % 4)) rf/frequencies (range 10)) => {0 4, 1 4, 2 2}
 (transduce (map #(quot % 4)) rf/frequencies (range 100)) => (frequencies (map #(quot % 4) (range 100))))

(facts
 "frequenciesv"
 (transduce identity (rf/frequenciesv 1) []) => [0]
 (transduce identity (rf/frequenciesv 1) [0]) => [1]
 (transduce identity (rf/frequenciesv 1) [1]) => throws
 (transduce identity (rf/frequenciesv 10) (range 10)) => [1 1 1 1 1 1 1 1 1 1]
 (transduce cat (rf/frequenciesv 10) [(range 3) (range 4) (range 2)]) => [3 3 2 1 0 0 0 0 0 0])

(facts
 "group-by"
 (transduce identity (rf/group-by identity) (range 5)) => {0 [0] 1 [1] 2 [2] 3 [3] 4 [4]}
 (transduce identity (rf/group-by odd?) (range 5)) => {true [1 3] false [0 2 4]}
 (transduce identity (rf/group-by odd?) (range 10)) => (group-by odd? (range 10))
 (transduce (take 5) (rf/group-by odd? (rf/into [])) (range 10)) => {true [1 3] false [0 2 4]}
  ;; (take 1) is a stateful reducer
 (transduce (take 2) (rf/group-by odd? ((take 1) (rf/into []))) (range 5)) => {true [] false [0]}
 (transduce identity (rf/group-by odd? +) (range 5)) => {true 4 false 6}
 (transduce identity (rf/group-by odd? (rf/juxt + *)) (range 5)) => {true [4 3] false [6 0]}
 (transduce identity (rf/group-by odd? rf/count) (range 10)) => (frequencies (map odd? (range 10))))

(facts
 "if-rf"
 (transduce (take 10) (rf/if-rf odd? (rf/into []) (rf/into #{})) (range 10))
 => [[1 3 5 7 9] #{0 2 4 6 8}]
 (transduce (take 10) (rf/if-rf odd? ((drop 1) (rf/into [])) ((take 2) (rf/into #{}))) (range 10))
 => [[3 5 7 9] #{0 2}])

(facts
 "case-rf"
 (transduce (take 10) (rf/case-rf (constantly 0) 0 (rf/into [])) (range 100))
 => {0 [0 1 2 3 4 5 6 7 8 9]}
 (transduce (take 10) (rf/case-rf odd? true (rf/into []) false (rf/into #{})) (range 100))
 => {true [1 3 5 7 9] false #{0 2 4 6 8}}
 (transduce (take 10) (rf/case-rf #(mod % 3) 0 (rf/into []) 1 (rf/into #{}) 2 rf/first) (range 100))
 => {0 [0 3 6 9] 1 #{1 4 7} 2 2}
 (transduce (take 10) (rf/case-rf #(mod % 3) 0 (rf/into []) 1 (rf/into #{}) rf/first) (range 100))
 => {0 [0 3 6 9] 1 #{1 4 7} ::rf/default 2})
