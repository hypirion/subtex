(ns com.hypirion.subtex.pass
  (:import (java.util ArrayList)))

(defprotocol Reenterable
  (afresh [this]))

(extend-type Object
  Reenterable
  (afresh [this] this))

(defn reenterable [xf]
  (fn [rf]
    (let [rf' (xf rf)]
      (reify clojure.lang.IFn
        (invoke [_] (rf'))
        (invoke [_ a] (rf' a))
        (invoke [_ a b] (rf' a b))
        Reenterable
        (afresh [_] (xf (afresh rf)))))))

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

(defn group-with [start-pred stop-pred f]
  (reenterable
   (fn [rf]
     (let [al (ArrayList.)]
       (fn
         ([] (rf))
         ([res] (if (.isEmpty al)
                  (rf res)
                  (throw (ex-info "Unmatched opening value"
                                  {:value (:start (stack-last al))}))))
         ([res elem]
          (cond (and (not (.isEmpty al))
                     (stop-pred (:start (stack-last al)) elem))
                (pop-from-stack al rf res f elem)
                
                (start-pred elem)
                (let [newrf (afresh rf)]
                  (stack-push al {:start elem :rf newrf :val (newrf)})
                  res)
                
                (not (.isEmpty al))
                (do (stack-add al elem)
                    res)
                
                :otherwise (rf res elem))))))))
