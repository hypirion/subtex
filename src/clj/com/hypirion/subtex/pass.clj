(ns com.hypirion.subtex.pass
  (:import (java.util ArrayList)))

(defprotocol Reinitializable
  (reinit [this]))

(defn verify-reinitializable
  "Verifies that rf is reinitializable, and if not, throws an exception."
  [rf]
  (when-not (extends? Reinitializable (type rf))
    (throw (ex-info "Reducer function must be reinitializable"
                    {:type (type rf) :val rf}))))

(defprotocol Stateless)

(defn stateless-rf
  "Returns a stateless recursive reducer."
  [rf]
  (reify clojure.lang.IFn
    (invoke [_] (rf))
    (invoke [_ a] (rf a))
    (invoke [_ a b] (rf a b))
    Reinitializable
    (reinit [this] this)
    Stateless))

(defn stateful-rf
  "Takes a stateful transducer and a reducer, and returns a stateful recursive
  reducer."
  [xf rf]
  (let [rf' (xf rf)]
    (reify clojure.lang.IFn
      (invoke [_] (rf'))
      (invoke [_ a] (rf' a))
      (invoke [_ a b] (rf' a b))
      Reinitializable
      (reinit [_] (stateful-rf xf (reinit rf))))))

(defn stateless-xf
  "Takes a transducer and returns a stateless recursive transducer."
  [xf]
  (fn [rf]
    (verify-reinitializable rf)
    (if (extends? Stateless (type rf)) ;; no need to reinitialise below
      (stateless-rf (xf rf))
      (stateful-rf xf rf))))

(defn stateful-xf
  "Takes a transducer and returns a stateful recursive transducer."
  [xf]
  (fn [rf]
    (verify-reinitializable rf)
    (stateful-rf xf rf)))

(defn stack-last [^ArrayList al]
  (.get al (dec (.size al))))

(defn stack-reset-last! [^ArrayList al v]
  (.set al (dec (.size al)) v))

(defn stack-push [^ArrayList al v]
  (.add al v))

(defn stack-pop [^ArrayList al]
  (.remove al (dec (.size al))))

(defn stack-add [^ArrayList al v]
  (let [inner (stack-last al)]
    (stack-reset-last! al (update inner :val (:rf inner) v))
    nil))

(defn pop-from-stack [^ArrayList al rf res f stop]
  (let [inner (stack-pop al)
        val ((:rf inner) (:val inner))] ;; completing
    (if (.isEmpty al)
      (f res rf (:start inner) val stop) ;; attach on res
      (let [inner' (stack-last al)]
        (stack-reset-last! al (update inner' :val f (:rf inner') (:start inner) val stop))
        res)))) ;; is on an inner type: attach on that instead and return res

;; TODO: formalise what a "subreduction" is, and use it here. Can be as simple
;; as the thing below.
(defn group-with [start-pred stop-pred f]
  (stateful-xf
   (fn [rf]
     (let [al (ArrayList.)]
       (fn
         ([] (rf))
         ([res] 
          (if (.isEmpty al)
            (rf res)
            (throw (ex-info (str "Unmatched opening value")
                            {:value (:start (stack-last al))
                             :collected (vec al)}))))
         ([res elem]
          (cond (and (not (.isEmpty al))
                     (stop-pred (:start (stack-last al)) elem))
                (pop-from-stack al rf res f elem)
                
                (start-pred elem)
                (let [newrf (reinit rf)]
                  (stack-push al {:start elem :rf newrf :val (newrf)})
                  res)
                
                (not (.isEmpty al))
                (do (stack-add al elem)
                    res)
                
                :otherwise (rf res elem))))))))

(defn subreduction
  [rf]
  (let [rf' (reinit rf)]
    {:value (rf')
     :rf rf'}))

(defn sub-step
  [{:keys [value rf]} elem]
  {:value (rf value elem)
   :rf rf})

(defn sub-complete
  [{:keys [value rf]}]
  (rf value))
