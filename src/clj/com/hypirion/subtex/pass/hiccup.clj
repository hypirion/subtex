(ns com.hypirion.subtex.pass.hiccup
  (:require [com.hypirion.subtex.pass :as pass])
  (:import (java.util ArrayList)))

(defn transform-invoke
  [name f]
  (fn [rf]
    (fn ([] (rf))
      ([res] (rf res))
      ([res input]
       (if (and (identical? (:type input) :invoke)
                (= name (:name input)))
         (rf res (apply f (:args input)))
         (rf res input))))))

(defn vectorize-invoke
  [name vect]
  (fn [rf]
    (fn ([] (rf))
      ([res] (rf res))
      ([res input]
       (if (and (identical? (:type input) :invoke)
                (= name (:name input)))
         (rf res [vect (-> input :args first :value seq)])
         (rf res input))))))

(def h1 (vectorize-invoke "\\title" :h1))
(def h2 (vectorize-invoke "\\section" :h2))
(def h3 (vectorize-invoke "\\subsection" :h3))
(def h4 (vectorize-invoke "\\subsubsection" :h4))

(def strong (vectorize-invoke "\\textbf" :strong))
(def em (vectorize-invoke "\\emph" :em))
(def sub (vectorize-invoke "\\textsubscript" :sub))
(def sup (vectorize-invoke "\\textsuperscript" :sub))

(def common-invokes
  (apply comp (map pass/reenterable [h1 h2 h3 h4 strong em sub sup])))

(defn- to-li [subrf]
  (let [val [:li (seq (pass/sub-complete @subrf))]]
    (vreset! subrf ::none)
    val))

(defn none? [val] ;; inline
  (identical? ::none val))

(def item-to-li
  (pass/reenterable
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
                (do (vswap! in-item pass/sub-step input) res)

                (and (identical? (:type input) :invoke)
                     (= "\\item" (:name input)))
                (do (vreset! in-item (pass/subreduction rf)) res)

                :otherwise
                (rf res input))))))))

(defn itemize-to-ul
  "Everything not directly inside a [:li ...] is collateral damage. Also, args
  are ignored."
  [rf]
  (fn ([] (rf))
    ([res]
     (rf res))
    ([res input]
     (if (and (identical? (:type input) :env)
              (= "itemize" (:name input)))
       (rf res [:ul (filter #(and (vector? %)
                                  (identical? (first %) :li))
                            (:value input))])
       (rf res input)))))

(defn enumerate-to-ol
  "Everything not directly inside a [:li ...] is collateral damage. Also, args
  are ignored."
  [rf]
  (fn ([] (rf))
    ([res]
     (rf res))
    ([res input]
     (if (and (identical? (:type input) :env)
              (= "enumerate" (:name input)))
       (rf res [:ol (filter #(and (vector? %)
                                  (identical? (first %) :li))
                            (:value input))])
       (rf res input)))))
