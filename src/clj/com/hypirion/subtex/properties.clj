(ns com.hypirion.subtex.properties
  "properties contains a set of useful property handlers"
  (:require [com.hypirion.rexf :as rexf]
            [com.hypirion.subtex.util :as util :refer [invoke=]]))

(defn- translate
  [name f]
  (rexf/map (fn [val]
              (if (invoke= name val)
                (f val)
                val))))

(def tags
  "A rexf transducer that converts \\tags to a set of tags"
  (translate "\\tags"
             (fn [{:keys [args]}]
               [:tags (set (map (comp :value first) args))])))


(def title
  "A rexf transducer that converts \\title to a title value"
  (translate "\\title"
             (fn [title]
               [:title (get-in title [:args 0 0 :value])])))

(def description
  "A rexf transducer that converts \\description to a description"
  (translate "\\description"
             (fn [desc]
               [:description (get-in desc [:args 0 0 :value])])))

(def documentclass
  "A rexf transducer that converts \\documentclass to an actual documentclass"
  (translate "\\documentclass"
             (fn [documentclass]
               [:documentclass (get-in documentclass [:args 0 0 :value])])))

(def filter-whitespace
  (comp (rexf/remove #(identical? :para-end (:type %)))
        (rexf/remove #(and (identical? :text (:type %))
                           (.. #"\s*" (matcher ^String (:value %)) matches)))))

(def common*
  (comp title tags description documentclass filter-whitespace))

(def common
  (common* (rexf/conj!-from {})))
