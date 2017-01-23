# xutil

A tiny utility library for transducers.

## Usage

```clojure
(require '[xutil.core :as x])

;; `mtransduce` is a parallel version of `transduce` taking multiple reducing functions.
;; Results are computed without holding entire sequence on memory.
(x/mtransduce
  (map inc)
  [+ * str]
  (range 10))

=> [55 3628800 "12345678910"]

;; Some reducing functions which can be combined with standard transducers.
(x/mtransduce
  cat
  {:count x/count
   :freq ((map #(mod % 4)) x/frequencies)
   :group ((filter odd?) (x/group-by (comp count str)))}
  [(range 10) (range 5 15)])

=> {:count 20
    :freq {0 5, 1 6, 2 5, 3 4}
	:group {1 [1 3 5 7 9 5 7 9], 2 [11 13]}}
```

## License

Copyright © 2017 Jun Imura

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
