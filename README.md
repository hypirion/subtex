# subtex

Clojure library to convert a subset of LaTeX into data of different shapes.

This is not intended to be a production quality library (at least not yet), but
rather a proof of concept that recursive transducers is a thing, and can be used
to create parsers and similar things. But if you can control the input, it
should be straightforward to use. However, it's likely only good for
experimenting with transducers, their definition and how one could make them
more powerful.

## Usage

As of now, the hiccup implementation is the only working thing, and it's rather
fragile. However, you could try it out like so:

```clj
(require '[com.hypirion.subtex.hiccup :as htex]
         '[hiccup.core :as hiccup])

(hiccup/html [:body (seq (:document (htex/read-string (slurp "example.tex"))))])

;; or for the complete output:
(htex/read-string (slurp "example.tex"))
```

The `example.tex` is in this project's root directory.

## License

Copyright © 2015 Jean Niklas L'orange

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
