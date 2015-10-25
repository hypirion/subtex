(ns com.hypirion.subtex.pass.minted
  (:require [com.hypirion.rexf :as rexf]))

(def ^:private begin-minted-match
  [[:call "\\begin"] [:open-brace "{"] [:text "minted"] [:close-brace "}"]])
(def ^:private end-minted-match
  [[:call "\\end"] [:open-brace "{"] [:text "minted"] [:close-brace "}"]])

(defn ^:private last-minted-matches? [a b]
  ;; only compares last one
  (let [n (dec (count b))]
    (= (a n) ((juxt :type :value) (b n)))))

;; This isn't perfect, but it works for now.
(defmulti ^:private tick (fn [rf res elem state] (:state state)))

(defmethod tick :start
  [rf res elem state]
  (let [new-acc (conj (:acc state) elem)]
    (if-not (last-minted-matches? begin-minted-match new-acc)
      [(reduce rf res new-acc) (assoc state :acc [])]
      (if (= (count begin-minted-match) (count new-acc))
        ;; we have a minted env, so look out for lang, then
        [res {:state :lang-0 :value [] :acc []}]
        [res (assoc state :acc new-acc)]))))

(defmethod tick :lang-0
  [rf res elem state]
  (if (identical? :open-brace (:type elem))
    [res (assoc state :state :lang-1 :value [elem])]
    ;; This bugs if you do \end immediately. That would be foolish though.
    [res {:state :end :lang "text" :value [elem] :acc []}]))

(defmethod tick :lang-1
  [rf res elem state]
  (let [new-val (conj (:value state) elem)]
    (if (identical? :text (:type elem))
      [res (assoc state :state :lang-2, :lang (:value elem)
                  :value new-val)]
      [res (-> state
               (assoc :state :end, :lang "text")
               (update :value conj elem))])))

(defmethod tick :lang-2
  [rf res elem state]
  (if (identical? :close-brace (:type elem))
    [res (assoc state :state :end :value [])]
    [res (-> state
             (assoc :state :end, :lang "text")
             (update :value conj elem))]))

(defmethod tick :end
  [rf res elem state]
  ;; like :start, but new-acc is passed into :value inside state instead.
  (let [new-acc (conj (:acc state) elem)]
    (if-not (last-minted-matches? end-minted-match new-acc)
      [res (-> state
               (update :value into new-acc)
               (assoc :acc []))]
      (if (= (count end-minted-match) (count new-acc))
        ;; Matching end, so make object and gank into res
        [(rf res (-> state
                     (select-keys [:lang :value])
                     (assoc :type :minted)
                     ;; Removes first and last char: not good if you start
                     ;; content on same line or if the content is empty.
                     (update :value #(as-> % s
                                       (map :value s)
                                       (apply str s)
                                       (subs s 1 (dec (count s)))))))
         {:state :start :acc []}]
        [res (assoc state :acc new-acc)]))))

(def process
  (rexf/stateful-xf
   (fn [rf]
     (let [state (volatile! {:state :start :acc []})]
       (fn ([] (rf))
         ([res]
          (if-not (identical? (:state @state) :start)
            (throw (ex-info "Unmatched minted environment start" {}))
            (rf (reduce rf res (:acc @state)))))
         ([res val]
          (let [[ret state'] (tick rf res val @state)]
            (vreset! state state')
            ret)))))))
