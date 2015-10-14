# subtex

Clojure library to convert a subset of LaTeX into hiccup-like data.

## Usage

```clj
(require '[com.hypirion.subtex :as tex])

(tex/read-string "\emph{foo}")
#_=> [:em "foo"]
```

## License

Copyright © 2015 Jean Niklas L'orange

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
