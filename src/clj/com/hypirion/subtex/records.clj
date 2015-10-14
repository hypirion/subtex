(ns com.hypirion.subtex.records
  (:import (clojure.lang Keyword)))

(defrecord Item [^Keyword type ^String value])
