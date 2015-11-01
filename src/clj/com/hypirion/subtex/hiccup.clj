(ns com.hypirion.subtex.hiccup
  (:refer-clojure :exclude [newline read-string])
  (:require [com.hypirion.rexf :as rexf]
            [com.hypirion.subtex :as tex]
            [com.hypirion.subtex.minted :as minted]
            [com.hypirion.subtex.properties :as props]
            [com.hypirion.subtex.util :refer [invoke? invoke= quoted? para-end?
                                              text? env? env=]]
            [hiccup.core :refer [h]]) ;; h == html-escape
  (:import (com.hypirion.rexf ReducerFactory Reducer)
           (com.hypirion.subtex Tokenise)
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
(def code   (vectorize-invoke "\\texttt" :code))
(def sub    (vectorize-invoke "\\textsubscript" :sub))
(def sup    (vectorize-invoke "\\textsuperscript" :sup))

(def hrule
  (rexf/map #(if (invoke= "\\hrule" %) [:hr] %)))

(def newline
  (rexf/map #(if (or (and (quoted? %) (= "\\\\" (:value %)))
                     (invoke= "\\newline" %))
               [:br]
               %)))

(def rm-maketitle
  (rexf/remove #(invoke= "\\maketitle" %)))

(def href
  (rexf/map
   (fn [input]
     (if (invoke= "\\href" input)
       (into [:a {:href (ffirst (:args input))}] ;; hmm, we should maybe unescape html then reescape as url?
             (second (:args input)))
       input))))

(def url
  (rexf/map
   (fn [input]
     (if (invoke= "\\url" input)
       [:a {:href (ffirst (:args input))} (ffirst (:args input))]
       input))))

(def img
  (rexf/map
   (fn [input]
     (if (invoke= "\\includegraphics" input)
       [:img {:src (ffirst (:args input))}]
       input))))

(def common-invokes
  (comp h1 h2 h3 h4 strong em code sub sup hrule newline url href img rm-maketitle))

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
     (cond (text? input) (h (:value input))
           (quoted? input) (h (subs (:value input) 1))
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

(defn- footnote-fn
  [[key val]]
  [:p key ":" val])

(defn footnote*
  ([] (footnote* 0
                (fn [state]
                  [(inc state) [:sup (str \[ (inc state) \])]])
                (fn [rf res vals]
                  (if (seq vals)
                    (reduce rf res (concat [[:br] [:br] [:br] [:hr] [:br]]
                                           (map footnote-fn vals)))
                    res))))
  ([init-state footnotemark-fn print-footnotes-fn]
   (rexf/global-xf
    (fn []
      (let [state (volatile! init-state)
            vals (volatile! [])]
        (fn [rf]
          (fn ([] (rf))
            ([res]
             (if (seq @vals)
               (throw (ex-info "Completing, but still has footnotes to print" {}))
               (rf res)))
            ([res input]
             (cond (invoke= "\\footnote" input)
                   (let [[new-state value] (footnotemark-fn @state)
                         num (inc (count @vals))
                         marker-id (str "f" num "m")
                         note-id (str "f" num "n")
                         output [:a {:href (str "#" note-id) :id marker-id} value]
                         key [:a {:href (str "#" marker-id) :id note-id} value]]
                     (do (vswap! vals conj [key (-> input :args first seq)])
                         (vreset! state new-state)
                         (rf res output)))

                   (invoke= "\\printfootnotes" input)
                   (let [res (print-footnotes-fn rf res @vals)]
                     (vreset! vals [])
                     (vreset! state init-state)
                     res)

                   :else
                   (rf res input))))))))))

(def ^:private rdoc
  (tex/read-document
   ((comp (tex/group-env #(get-in % [:args 0 0]))
          item-to-li
          itemize-to-ul enumerate-to-ol
          common-invokes text-value (footnote*)
          tex/shrink-text tex/minted-to-pre
          paragraphiphy)
    rexf/conj!)))

(def ^:private default-transducer
  (comp minted/process tex/remove-comments tex/match-braces
        tex/group-calls (tex/read-if-properties "post" props/common)
        rdoc (rexf/toplevel tex/filter-data)))

(defn read-string
  [s]
  (->>
   (iterator-seq (Tokenise. s))
   (rexf/into [] default-transducer)
   (into {})))
