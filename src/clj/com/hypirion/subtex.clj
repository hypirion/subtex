(ns com.hypirion.subtex
  (:refer-clojure :exclude [read])
  (:require [com.hypirion.subtex.pass.minted :as minted]
            [com.hypirion.subtex.pass :as pass]
            [com.hypirion.subtex.pass.hiccup :as hiccup])
  (:import (com.hypirion.subtex Tokenise)
           (java.util ArrayList)))

(def match-braces
  (pass/group-with
   (fn [elem] (case (:type elem)
               :open-brace true
               :close-brace (ex-info "Unmatched closing brace" {:value elem})
               false))
   (fn [_ end]
     (identical? :close-brace (:type end)))
   (fn [res rf _ val _]
     (rf res {:type :param :value val}))))

(defn to-invokation [call subrf]
  (let [val {:type :invoke
             :name (:value @call)
             :args (pass/sub-complete @subrf)}]
    (vreset! call nil)
    (vreset! subrf nil)
    val))

(def group-calls
  (pass/stateful-xf
   (fn [rf]
     (let [call (volatile! nil)
           subrf (volatile! nil)]
       (fn
         ([] (rf))
         ([res]
          (let [result (if (nil? @call)
                         res
                         (let [v (to-invokation call subrf)]
                           (unreduced (rf res v))))]
            (rf result)))
         ([res input]
          (if @call
            (case (:type input)
              :param (do (vswap! subrf pass/sub-step input) res)
              :call (let [val (to-invokation call subrf)]
                      (vreset! call input)
                      (vreset! subrf (pass/subreduction rf))
                      (rf res val))
              (-> res
                  (rf (to-invokation call subrf))
                  (rf input)))
            (case (:type input)
              :param (throw (ex-info "Unmatched parameter" {:value input}))
              :call (do (vreset! call input)
                        (vreset! subrf (pass/subreduction rf))
                        res)
              (rf res input)))))))))

;; This doesn't really make sense because we can't really drag out the first
;; value, but oh well.
(defn raw-env-name [env]
  ;; yess. I don't even.
  (get-in env [:args 0 :value 0 :value]))

(def group-env
  (pass/group-with
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
  (pass/stateful-xf
   (remove #(identical? (:type %) :comment))))

(def rf-conj!
  (pass/stateless-rf
   (fn
     ([] (transient []))
     ([res] (persistent! res))
     ([res input] (conj! res input)))))

(defn read
  [data]
  (transduce (comp minted/process remove-comments match-braces
                   group-calls group-env hiccup/item-to-li hiccup/itemize-to-ul
                   hiccup/enumerate-to-ol hiccup/common-invokes
                   )
             rf-conj! (transient [:body])
             (iterator-seq (Tokenise. data))))
