# xutil

A tiny utility library for transducers.

[![Build Status](https://travis-ci.org/alumi/xutil.svg?branch=master)](https://travis-ci.org/alumi/xutil)

[![Clojars Project](https://img.shields.io/clojars/v/xutil.svg)](https://clojars.org/xutil)

## Installation

```clojure
[xutil "0.0.2-SNAPSHOT"]
```

## Usage

```clojure
(require '[xutil.ctx :as x])
(require '[xutil.rf :as rf])

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
  {:count rf/count
   :freq ((map #(mod % 4)) rf/frequencies)
   :group ((filter odd?) (rf/group-by (comp count str)))}
  [(range 10) (range 5 15)])

=> {:count 20
    :freq {0 5, 1 6, 2 5, 3 4}
	:group {1 [1 3 5 7 9 5 7 9], 2 [11 13]}}
```

## License

Copyright © 2017 Jun Imura

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
