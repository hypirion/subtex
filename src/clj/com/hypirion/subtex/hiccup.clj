(ns com.hypirion.subtex.hiccup
  (:refer-clojure :exclude [newline])
  (:require [com.hypirion.rexf :as rexf]
            [com.hypirion.subtex.util :refer [invoke? invoke= quoted? para-end?
                                              text? env? env=]])
  (:import (com.hypirion.rexf ReducerFactory Reducer)
           (java.util ArrayList)))

(defn vectorize-invoke
  [name vect]
  (rexf/map
   (fn [input]
     (if (invoke= name input)
       [vect (-> input :args first seq)]
       input))))

(def h1 (vectorize-invoke "\\title" :h1))
(def h2 (vectorize-invoke "\\section" :h2))
(def h3 (vectorize-invoke "\\subsection" :h3))
(def h4 (vectorize-invoke "\\subsubsection" :h4))

(def strong (vectorize-invoke "\\textbf" :strong))
(def em     (vectorize-invoke "\\emph" :em))
(def sub    (vectorize-invoke "\\textsubscript" :sub))
(def sup    (vectorize-invoke "\\textsuperscript" :sup))

(def newline
  (rexf/map #(if (or (and (quoted? %) (= "\\\\" (:value %)))
                     (and (invoke? %) (= "\\newline" (:name %))))
               [:br]
               %)))

;; hrule, footnote, texttt (?)

(def common-invokes
  (comp h1 h2 h3 h4 strong em sub sup newline))

(defn- to-li [subrf]
  (let [val [:li (seq (rexf/subcomplete @subrf))]]
    (vreset! subrf ::none)
    val))

(defn none? [val] ;; todo: inline
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
          (cond (and (not (none? @in-item))
                     (invoke= "\\item" input))
                (let [li (to-li in-item)]
                  (vreset! in-item (rexf/subreduction rf))
                  (rf res li))

                (not (none? @in-item))
                (do (vswap! in-item rexf/substep input) res)

                (invoke= "\\item" input)
                (do (vreset! in-item (rexf/subreduction rf)) res)

                :otherwise
                (rf res input))))))))

(def itemize-to-ul
  "Everything not directly inside a [:li ...] is collateral damage. Also, args
  are ignored."
  (rexf/map
   (fn [input]
     (if (env= "itemize" input)
       [:ul (filter #(and (vector? %) (identical? (first %) :li))
                    (:value input))]
       input))))

(def enumerate-to-ol
  "Everything not directly inside a [:li ...] is collateral damage. Also, args
  are ignored."
  (rexf/map
   (fn [input]
     (if (env= "enumerate" input)
       [:ol (filter #(and (vector? %) (identical? (first %) :li))
                    (:value input))]
       input))))

;; href
;; url

(def text-value
  (rexf/map
   (fn [input]
     (cond (text? input) (:value input)
           (quoted? input) (subs (:value input) 1)
           :else input))))

(def blocklevels #{:address :article :aside :blockquote :canvas :dd :div :dl
                   :fieldset :figcaption :figure :footer :form :h1 :h2 :h3 :h4
                   :h5 :h6 :header :hgroup :hr :li :main :nav :noscript :ol
                   :output :p :pre :section :table :tfoot :ul :video})

(defn blocklevel? [elem]
  (boolean (blocklevels (first elem))))

(def paragraphiphy
  (rexf/toplevel
   (fn [rf]
     (let [group (volatile! nil)]
       (fn ([] (rf))
         ([res]
          (if @group
            (let [elem @group]
              (vreset! group nil)
              (rf (unreduced (rf res elem))))))
         ([res input]
          (cond (and @group (para-end? input))
                (let [res (rf res @group)]
                  (vreset! group nil)
                  res)

                (and @group (blocklevel? input))
                (let [res (rf res @group)]
                  (vreset! group nil)
                  (if (reduced? res)
                    res
                    (rf res input)))

                @group ;; (not (blocklevel? input))
                (do (vswap! group conj input)
                    res)

                (blocklevel? input) ;; (not @group)
                (rf res input)

                (para-end? input)
                res

                :else
                (do (vreset! group [:p input])
                    res))))))))
