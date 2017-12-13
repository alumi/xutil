(ns xutil.xf-test
  (:require [xutil.xf :as xf]
            [xutil.rf :as rf]
            [midje.sweet :refer :all]))

(tabular
 (facts
  "juxt"
  (into [] (apply xf/juxt ?xfs) ?coll) => ?expected
  (sequence (apply xf/juxt ?xfs) ?coll) => ?expected
  (transduce (apply xf/juxt ?xfs) (rf/into []) ?coll) => ?expected
  (transduce identity ((apply xf/juxt ?xfs) (rf/into [])) ?coll) => ?expected)
 ?xfs ?coll ?expected
 [(take 2)] (range 10) [0 1]
 [(take 2) (take 2)] (range 10) [0 0 1 1]
 [(take 2) (drop 2)] (range 10) [0 1 2 3 4 5 6 7 8 9]
 [(filter odd?) (take 2)] (range 10) [0 1 1 3 5 7 9])

(facts
 "rf->xf"
 (into [] (xf/rf->xf (rf/into [])) (range 10)) => [[0 1 2 3 4 5 6 7 8 9]]
 (into [] (xf/rf->xf (rf/if-rf odd? (rf/into []) (rf/into #{}))) (range 10)) => [[[1 3 5 7 9] #{0 2 4 6 8}]])

(facts
 "max"
 (into [] xf/max (range 10)) => [9]
 (into [] (xf/rf->xf rf/max) (range 10)) => [9])

(facts
 "min"
 (into [] xf/min (range 10)) => [0]
 (into [] (xf/rf->xf rf/min) (range 10)) => [0])

(facts
 "first"
 (into [] xf/first (range 10)) => [0]
 (into [] (comp (drop 1) xf/first) (range 10)) => [1]
 (transduce xf/first + (range 10)) => 0
 (transduce identity (xf/first +) (range 10)) => 0)

(facts
 "ffirst"
 (into [] xf/ffirst (range 10)) => throws
 (into [] xf/ffirst (partition 2 (range 10))) => [0]
 (into [] (comp (drop 1) xf/ffirst) (range 10)) => throws
 (into [] (comp (drop 1) xf/ffirst) (partition 2 (range 10))) => [2]
 (transduce xf/ffirst + (range 10)) => throws
 (transduce xf/ffirst + (partition 2 (range 10))) => 0
 (transduce identity (xf/ffirst +) (range 10)) => throws
 (transduce identity (xf/ffirst +) (partition 2 (range 10))) => 0)

(facts
 "second"
 (into [] xf/second (range 10)) => [1]
 (into [] (comp (drop 1) xf/second) (range 10)) => [2]
 (transduce xf/second + (range 10)) => 1
 (transduce identity (xf/second +) (range 10)) => 1)

(facts
 "next"
 (into [] xf/next (range 10)) => [1 2 3 4 5 6 7 8 9]
 (into [] (comp (drop 1) xf/next) (range 10)) => [2 3 4 5 6 7 8 9]
 (into [] (comp (take 4) xf/next) (range 10)) => [1 2 3]
 (transduce xf/next + (range 10)) => 45
 (transduce identity (xf/next +) (range 10)) => 45)

(facts
 "last"
 (sequence xf/last (range 10)) => [9]
 (into [] xf/last (range 10)) => [9]
 (into [] (comp (drop 1) xf/last) (range 10)) => [9]
 (into [] (comp (take 4) xf/last) (range 10)) => [3]
 (transduce xf/last + (range 10)) => 9
 (transduce identity (xf/last +) (range 10)) => 9)

(facts
 "count"
 (sequence xf/count (range 10)) => [10]
 (into [] xf/count (range 10)) => [10]
 (sequence (comp (take 2) xf/count) (range 10)) => [2]
 (into [] (comp (take 2) xf/count) (range 10)) => [2]
 (transduce xf/count + (range 10)) => 10
 (transduce identity (xf/count +) (range 10)) => 10
 (transduce (take 5) (xf/count +) (range 10)) => 5)

(facts
 "into"
 (sequence (xf/into []) (range 10)) => '([0 1 2 3 4 5 6 7 8 9])
 (sequence (comp (take 5) (xf/into [])) (range 10)) => '([0 1 2 3 4])
 (sequence (xf/into #{}) (range 10)) => '(#{0 1 2 3 4 5 6 7 8 9})
 (sequence (comp (take 5) (xf/into #{})) (range 10)) => '(#{0 1 2 3 4})
 (into [] (comp (take 5) (xf/into #{})) (range 10)) => [#{0 1 2 3 4}]
 (transduce (xf/into []) (rf/into []) (range 10)) => [[0 1 2 3 4 5 6 7 8 9]]
 (transduce (xf/into []) vector (range 10)) => [[[] [0 1 2 3 4 5 6 7 8 9]]]
 (transduce (take 2) ((xf/into []) (rf/into [])) (range 10)) => [[0 1]]
 (transduce (comp (take 2) (xf/into [])) (rf/into []) (range 10)) => [[0 1]])

(facts
 "frequencies"
 (sequence xf/frequencies (range 10)) => '({0 1 1 1 2 1 3 1 4 1 5 1 6 1 7 1 8 1 9 1})
 (sequence (comp (take 5) xf/frequencies) (range 10)) => '({0 1 1 1 2 1 3 1 4 1})
 (transduce xf/frequencies (rf/into []) (range 10)) => [{0 1 1 1 2 1 3 1 4 1 5 1 6 1 7 1 8 1 9 1}]
 (transduce (comp (take 5) xf/frequencies) (rf/into []) (range 10)) => [{0 1 1 1 2 1 3 1 4 1}]
 (transduce (take 5) (xf/frequencies (rf/into [])) (range 10)) => [{0 1 1 1 2 1 3 1 4 1}])

(facts
 "group-by"
 (sequence (xf/group-by odd?) (range 10)) => '([false [0 2 4 6 8]] [true [1 3 5 7 9]])
 (sequence (xf/group-by odd? (comp (take 2) (xf/into []))) (range 10)) => '([false [0 2]] [true [1 3]])
 (into {} (xf/group-by odd? (comp (take 2) (xf/into []))) (range 10)) => {false [0 2] true [1 3]}
 (into {} (xf/group-by #(mod % 5) xf/first) (range 10)) => {0 0 1 1 2 2 3 3 4 4}
 (into {} (xf/group-by #(mod % 5) xf/last) (range 10)) => {0 5 1 6 2 7 3 8 4 9})

(facts
 "if-xf"
 (transduce (take 4) ((xf/if-xf odd? (map inc) (map dec)) +) (range 10)) => 6
 (transduce (comp (take 4) (xf/if-xf odd? (map inc) (map dec))) + (range 10)) => 6
 (transduce (take 8) ((xf/if-xf odd? (take 2) (take 2)) +) (range 10)) => 6
 (transduce (take 8) ((xf/if-xf odd? (take 1) (take 3)) +) (range 10)) => 7)

(tabular
 (facts
  "if-xf tabular"
  (into [] (xf/if-xf ?pred ?then-xf ?else-xf) ?coll) => ?expected
  (sequence (xf/if-xf ?pred ?then-xf ?else-xf) ?coll) => ?expected
  (transduce (xf/if-xf ?pred ?then-xf ?else-xf) (rf/into []) ?coll) => ?expected
  (transduce identity ((xf/if-xf ?pred ?then-xf ?else-xf) (rf/into [])) ?coll) => ?expected)
 ?pred ?then-xf ?else-xf ?coll ?expected
 odd? (map inc) (map dec) (range 10) [-1 2 1 4 3 6 5 8 7 10]
 odd? (take 2) (drop 3) (range 10) [1 3 6 8]
 #(< % 5) (mapcat #(repeat 2 %)) (drop 3) (range 10) [0 0 1 1 2 2 3 3 4 4 8 9]
 odd? (xf/into []) (xf/into #{}) (range 10) [[1 3 5 7 9] #{0 2 4 6 8}])

(facts
 "if-xf reduced"
 (let [coll [(reduced 1) (reduced 2) 3 (reduced 4) 5 6 7]
       expected (map #(get coll %) [0 1 2 4 5])]
   (sequence (xf/if-xf reduced? (take 2) (take 3)) coll) => expected
   (transduce (xf/if-xf reduced? (take 2) (take 3)) (rf/into []) coll) => expected
   (transduce (comp (take 2) (xf/if-xf reduced? (take 2) (take 3))) (rf/into []) coll) => (take 2 expected)
   (transduce identity ((xf/if-xf reduced? (take 2) (take 3)) (rf/into [])) coll) => expected
   (transduce (take 2) ((xf/if-xf reduced? (take 2) (take 3)) (rf/into [])) coll) => (take 2 expected)))

(facts
 "case-xf"
 (sequence (xf/case-xf odd? true (map inc) false (map dec)) (range 10)) => '(-1 2 1 4 3 6 5 8 7 10)
 (sequence (xf/case-xf odd? true (map inc) (map dec)) (range 10)) => '(-1 2 1 4 3 6 5 8 7 10)
 (sequence (xf/case-xf odd? true (xf/into []) false (xf/into #{})) (range 10)) => '([1 3 5 7 9] #{0 2 4 6 8})
 (sequence (xf/case-xf odd? true (xf/into []) (xf/into #{})) (range 10)) => '([1 3 5 7 9] #{0 2 4 6 8})
 (sequence (xf/case-xf odd? true (comp (take 1) (xf/into [])) (comp (take 2) (xf/into #{}))) (range 10)) => '([1] #{0 2})
 (transduce (xf/case-xf odd? true (map inc) (map dec)) + (range 10)) => 45)
