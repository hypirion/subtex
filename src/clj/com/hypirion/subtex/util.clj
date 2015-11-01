(ns com.hypirion.subtex.util
  (:require [com.hypirion.rexf :as rexf]))

(defn invoke? [input]
  (identical? (:type input) :invoke))

(defn invoke= [name input]
  (and (invoke? input)
       (= name (:name input))))

(defn quoted? [input]
  (identical? (:type input) :quoted))

(defn para-end? [input]
  (identical? (:type input) :para-end))

(defn text? [input]
  (identical? (:type input) :text))

(defn env? [input]
  (identical? (:type input) :env))
