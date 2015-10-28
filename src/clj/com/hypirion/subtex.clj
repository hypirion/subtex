(ns com.hypirion.subtex
  (:refer-clojure :exclude [read])
  (:require [com.hypirion.rexf :as rexf]
            [com.hypirion.subtex.minted :as minted]
            [com.hypirion.subtex.hiccup :as hiccup])
  (:import (com.hypirion.subtex Tokenise)))

(def match-braces
  (rexf/group-with
   (fn [elem] (case (:type elem)
               :open-brace true
               :close-brace (ex-info "Unmatched closing brace" {:value elem})
               false))
   (fn [_ end]
     (identical? :close-brace (:type end)))
   (fn [_ val _]
     {:type :param :value val})))

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
(defn raw-env-name
  [env]
  (get-in env [:args 0 :value 0 :value]))

;; TODO: Passing a function IN isn't really that sensical I believe. It should
;; rather be done downstream
(defn group-env
  ([] (group-env raw-env-name))
  ([env-fn]
   (rexf/group-with
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
    (fn [start val _]
      {:type :env
       :name (env-fn start)
       :args (vec (rest (:args start)))
       :value val}))))

(def remove-comments
  (rexf/stateful-xf
   (remove #(identical? (:type %) :comment))))

(defn- omit-ws [^StringBuilder sb ^String s]
  (loop [last-ws? (boolean (if (pos? (.length sb)) ;; check last elem in sb
                             (= \space (.charAt sb (dec (.length sb))))
                             false))
         i 0]
    (when (< i (.length s))
      (let [c (.charAt s i)]
        (cond (and last-ws? (Character/isWhitespace c))
              (recur (boolean true) (inc i))

              (Character/isWhitespace c)
              (do (.append sb \space)
                  (recur (boolean true) (inc i)))

              :else
              (do (.append sb c)
                  (recur (boolean false) (inc i))))))))

(def shrink-text
  (rexf/stateful-xf
   (fn [rf]
     (let [sb (volatile! nil)]
       (fn ([] (rf))
         ([res]
          (let [res (if @sb
                      (unreduced (rf res {:type :text
                                          :value (.toString @sb)}))
                      res)]
            (rf res)))
         ([res input]
          (cond (and (some? @sb) (identical? :text (:type input)))
                (do (omit-ws @sb (:value input)) res)

                (some? @sb)
                (let [res (rf res {:type :text :value (.toString @sb)})]
                  (vreset! sb nil)
                  (if (reduced? res)
                    res
                    (rf res input)))

                (identical? :text (:type input))
                (do (vreset! sb (StringBuilder.))
                    (omit-ws @sb (:value input))
                    res)

                :else
                (rf res input))))))))

(def minted-to-pre
  (rexf/stateless-xf
   (fn [rf]
     (fn ([] (rf))
       ([res] (rf res))
       ([res input]
        (if (identical? :minted (:type input))
          (rf res [:pre (:value input)])
          (rf res input)))))))

(defn read
  [data]
  (rexf/into [:body]
             (comp minted/process remove-comments match-braces
                   group-calls (group-env #(get-in % [:args 0 :value 0])) hiccup/item-to-li
                   hiccup/itemize-to-ul hiccup/enumerate-to-ol
                   hiccup/common-invokes shrink-text hiccup/paragraphiphy
                   minted-to-pre)
             (iterator-seq (Tokenise. data))))
