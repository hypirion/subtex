(ns com.hypirion.subtex
  (:refer-clojure :exclude [read])
  (:require [com.hypirion.rexf :as rexf]
            [com.hypirion.subtex.minted :as minted]
            [com.hypirion.subtex.util :as util :refer [invoke?]]
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
              :param (do (vswap! subrf rexf/substep (:value input)) res)
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
  (get-in env [:args 0 0 :value]))

;; TODO: Passing a function IN isn't really that sensical I believe. It should
;; rather be done downstream. Also, fixup that throw ex-info
(defn group-env
  ([] (group-env raw-env-name))
  ([env-fn]
   (rexf/group-with
    (fn [elem] (and (identical? :invoke (:type elem))
                   (= "\\begin" (:name elem))))
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

(defn read-if-properties [prop-name]
  (let [if-prop (str "\\if" prop-name)]
    (rexf/stateful-xf
     (fn [rf]
       (let [in-if? (volatile! false)
             prev-newif? (volatile! false)
             val (volatile! nil)]
         (fn ([] (rf))
           ([res] (if @in-if?
                    (throw (ex-info "Unmatched if opening" {:name prop-name}))
                    (rf res)))
           ([res input]
            (if @in-if?
              (cond (and (invoke? input) (= "\\fi" (:name input)))
                    (let [res (rf res {:type :properties
                                       :value (rexf/subcomplete @val)})]
                      (vreset! in-if? false)
                      (vreset! prev-newif? false)
                      (vreset! val nil)
                      res)

                    (and (identical? (:type input) :invoke)
                         (= if-prop (:name input)))
                    (throw (ex-info "Nested \\ifprop command" {:name prop-name}))

                    :else
                    (do (vswap! val rexf/substep input)
                        res))
              ;; if (not @in-if?)
              (cond (and (identical? (:type input) :invoke)
                         (= if-prop (:name input))
                         @prev-newif?)
                    (do (vreset! prev-newif? false)
                        (rf res input))

                    (and (identical? (:type input) :invoke)
                         (= if-prop (:name input)))
                    (do (vreset! val (rexf/reduction rexf/conj!))
                        (vreset! in-if? true)
                        res)

                    (and (identical? (:type input) :invoke)
                         (= "\\newif" (:name input)))
                    (do (vreset! prev-newif? true)
                        (rf res input))

                    :else
                    (do (vreset! prev-newif? false)
                        (rf res input)))))))))))

(defn read-document
  [rfactory]
  (rexf/stateful-xf
   (fn [rf]
     (let [inner-rf (volatile! nil)]
       (fn ([] (rf))
         ([res]
          (if @inner-rf ;; couldn't close subreduction
            (throw (ex-info "Open document environment" {:value @inner-rf}))
            (rf res)))
         ([res input] ;; gobble gobble
          (if @inner-rf
            (cond (and (invoke? input)
                       (= "\\end" (:name input))
                       (= "document" (raw-env-name input)))
                  (let [produced (rexf/subcomplete @inner-rf)]
                    (vreset! inner-rf nil)
                    (rf res {:type :document :value produced}))

                  (and (invoke? input)
                       (= "\\begin" (:name input))
                       (= "document" (raw-env-name input)))
                  (throw (ex-info "Nested document environment" {}))

                  :else
                  (do (vswap! inner-rf rexf/substep input)
                      res))
            (cond (and (invoke? input)
                       (= "\\end" (:name input))
                       (= "document" (raw-env-name input)))
                  (throw (ex-info "Closing document environment, but have no matching opening"
                                  {}))

                  (and (invoke? input)
                       (= "\\begin" (:name input))
                       (= "document" (raw-env-name input)))
                  (do (vreset! inner-rf (rexf/reduction rfactory))
                      res)

                  :else
                  (rf res input)))))))))

(def rdoc
  (read-document
   ((comp (group-env)
           hiccup/item-to-li
           hiccup/itemize-to-ul hiccup/enumerate-to-ol
           hiccup/common-invokes shrink-text hiccup/paragraphiphy
           minted-to-pre)
    rexf/conj!)))

(defn read
  [data]
  (rexf/into []
             (comp minted/process remove-comments match-braces
                   group-calls (read-if-properties "post") rdoc)
             (iterator-seq (Tokenise. data))))
