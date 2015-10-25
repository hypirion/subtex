(ns com.hypirion.subtex.hiccup
  (:require [com.hypirion.rexf :as rexf])
  (:import (java.util ArrayList)))

(defn vectorize-invoke
  [name vect]
  (rexf/stateless-xf
   (fn [rf]
     (fn ([] (rf))
       ([res] (rf res))
       ([res input]
        (if (and (identical? (:type input) :invoke)
                 (= name (:name input)))
          (rf res [vect (-> input :args first :value seq)])
          (rf res input)))))))

(def h1 (vectorize-invoke "\\title" :h1))
(def h2 (vectorize-invoke "\\section" :h2))
(def h3 (vectorize-invoke "\\subsection" :h3))
(def h4 (vectorize-invoke "\\subsubsection" :h4))

(def strong (vectorize-invoke "\\textbf" :strong))
(def em     (vectorize-invoke "\\emph" :em))
(def sub    (vectorize-invoke "\\textsubscript" :sub))
(def sup    (vectorize-invoke "\\textsuperscript" :sup))

(def common-invokes
  (comp h1 h2 h3 h4 strong em sub sup))

(defn- to-li [subrf]
  (let [val [:li (seq (rexf/subcomplete @subrf))]]
    (vreset! subrf ::none)
    val))

(defn none? [val] ;; inline
  (identical? ::none val))

(def item-to-li
  (rexf/stateful-xf
   (fn [rf]
     (let [in-item (volatile! ::none)]
       (fn ([] (rf))
         ([res]
          (let [res (if (none? @in-item)
                      res
                      (let [v (to-li in-item)]
                        (unreduced (rf res v))))]
            (rf res)))
         ([res input]
          (cond (and (not (none? @in-item)) (identical? (:type input) :invoke)
                     (= "\\item" (:name input)))
                (rf res (to-li in-item))

                (not (none? @in-item))
                (do (vswap! in-item rexf/substep input) res)

                (and (identical? (:type input) :invoke)
                     (= "\\item" (:name input)))
                (do (vreset! in-item (rexf/subreduction rf)) res)

                :otherwise
                (rf res input))))))))

(def itemize-to-ul
  "Everything not directly inside a [:li ...] is collateral damage. Also, args
  are ignored."
  (rexf/stateless-xf
   (fn  [rf]
     (fn ([] (rf))
       ([res]
        (rf res))
       ([res input]
        (if (and (identical? (:type input) :env)
                 (= "itemize" (:name input)))
          (rf res [:ul (filter #(and (vector? %)
                                     (identical? (first %) :li))
                               (:value input))])
          (rf res input)))))))

(def enumerate-to-ol
  "Everything not directly inside a [:li ...] is collateral damage. Also, args
  are ignored."
  (rexf/stateless-xf
   (fn [rf]
     (fn ([] (rf))
       ([res]
        (rf res))
       ([res input]
        (if (and (identical? (:type input) :env)
                 (= "enumerate" (:name input)))
          (rf res [:ol (filter #(and (vector? %)
                                     (identical? (first %) :li))
                               (:value input))])
          (rf res input)))))))
