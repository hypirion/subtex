(ns com.hypirion.subtex.util
  (:require [com.hypirion.rexf :as rexf]))

(defn invoke? [input]
  (identical? (:type input) :invoke))
