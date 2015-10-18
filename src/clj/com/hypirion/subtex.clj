(ns com.hypirion.subtex
  (:refer-clojure :exclude [read])
  (:require [com.hypirion.subtex.pass.minted :as minted]
            [com.hypirion.subtex.pass :refer :all])
  (:import (com.hypirion.subtex Tokenise)
           (java.util ArrayList)))

(def match-braces
  (group-with
   (fn [elem] (case (:type elem)
               :open-brace true
               :close-brace (ex-info "Unmatched closing brace" {:value elem})
               false))
   (fn [_ end]
     (identical? :close-brace (:type end)))
   (fn [res rf _ val _]
     (rf res {:type :param :value val}))))

(defn to-invokation [^ArrayList al]
  (let [call (.get al 0)
        args (vec (.subList al 1 (.size al)))]
    (.clear al)
    {:type :invoke :name (:value call) :args args}))

(def group-calls
  (reenterable
   (fn [rf]
     (let [al (ArrayList.)]
       (fn
         ([] (rf))
         ([res]
          (let [result (if (.isEmpty al)
                         res
                         (let [v (to-invokation al)]
                           (unreduced (rf res v))))]
            (rf result)))
         ([res input]
          (if-not (.isEmpty al)
            (case (:type input)
              :param (do (.add al input) res)
              :call (let [val (to-invokation al)]
                      (.add al input)
                      (rf res val))
              (-> res
                  (rf (to-invokation al))
                  (rf input)))
            (case (:type input)
              :param (throw (ex-info "Unmatched parameter" {:value input}))
              :call (do (.add al input) res)
              (rf res input)))))))))

;; This doesn't really make sense because we can't really drag out the first
;; value, but oh well.
(defn raw-env-name [env]
  ;; yess. I don't even.
  (get-in env [:args 0 :value 0 :value]))

(def group-env
  (group-with
   (fn [elem] (case (:type elem)
               :invoke (case (:name elem)
                         "\\begin" true
                         "\\end" (throw (ex-info "Unmatched environment ending" {:value elem}))
                         false)
               false))
   (fn [start end]
     (and
      (identical? (:type end) :invoke)
      (= "\\end" (:name end))
      (= (first (:args start)) (first (:args start)))))
   (fn [res rf start val _]
     (rf res {:type :env
              :name (raw-env-name start)
              :args (vec (rest (:args start)))
              :value val}))))

(def remove-comments
  (remove #(identical? (:type %) :comment)))

(defn read
  [data]
  (transduce (comp minted/process match-braces group-calls group-env remove-comments)
             conj []
             (iterator-seq (Tokenise. data))))
