(ns com.hypirion.subtex
  (:refer-clojure :exclude [read])
  (:require [com.hypirion.rexf :as rexf]
            [com.hypirion.subtex.pass.minted :as minted]
            [com.hypirion.subtex.pass.hiccup :as hiccup])
  (:import (com.hypirion.subtex Tokenise)))

(def match-braces
  (rexf/group-with*
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
             :args (rexf/subcomplete @subrf)}]
    (vreset! call nil)
    (vreset! subrf nil)
    val))

(def group-calls
  (rexf/stateful-xf
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
              :param (do (vswap! subrf rexf/substep input) res)
              :call (let [val (to-invokation call subrf)]
                      (vreset! call input)
                      (vreset! subrf (rexf/subreduction rf))
                      (rf res val))
              (-> res
                  (rf (to-invokation call subrf))
                  (rf input)))
            (case (:type input)
              :param (throw (ex-info "Unmatched parameter" {:value input}))
              :call (do (vreset! call input)
                        (vreset! subrf (rexf/subreduction rf))
                        res)
              (rf res input)))))))))

;; This doesn't really make sense because we can't really drag out the first
;; value, but oh well.
(defn raw-env-name [env]
  ;; yess. I don't even.
  (get-in env [:args 0 :value 0 :value]))

(def group-env
  (rexf/group-with*
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
  (rexf/stateful-xf
   (remove #(identical? (:type %) :comment))))

(defn read
  [data]
  (rexf/into [:body]
             (comp minted/process remove-comments match-braces
                   group-calls group-env hiccup/item-to-li hiccup/itemize-to-ul
                   hiccup/enumerate-to-ol hiccup/common-invokes)
             (iterator-seq (Tokenise. data))))
