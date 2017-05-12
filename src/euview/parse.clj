(ns euview.parse
  (:require [clojure.string :as string]
            [euview.lex :as lex]))

(def patterns
  {:eutxt [[#{:eutxt}] (fn [[] state] state)]
   :set-var [[#{:variable} #{:equals} #{:number :ymd :string :tag}]
             (fn [[k _ v]
                  {:keys [stack variables] :as state}]
               (update-in state (cons :variables stack) assoc (:value k) v))]
   :array-entry [[#{:number :ymd :string :tag}]
             (fn [[k _ v]
                  {:keys [stack variables] :as state}]
               (update-in state (cons :variables stack) assoc (:value k) v))]
   :set-map [[#{:variable} #{:equals} #{:open}]
             (fn [[k _ _]
                  state]
               (update state :stack conj (:value k)))]
   :close [[#{:close}]
           (fn [[_] state]
             (update state :stack pop))]})

(defn parse [original-tokens]
  (println (take 10 original-tokens))
  (loop [state {:variables {}
                :stack []}
         tokens (drop 1 original-tokens)]
    (if-not (seq tokens)
      (do
        (when-not (= (:stack state) [])
          (throw (ex-info "Exited without popping full stack???" {:stack (:stack state)})))
        state)
      (let [matches (keep (fn [[k [spec state-updater]]]
                            (let [consumed (count spec)
                                  agreements (map (fn [token valid-types] ((:type token) valid-types))
                                                  tokens
                                                  spec)]
                              (when (and (= (count agreements) consumed)
                                         (every? identity agreements))
                                {:consumed consumed
                                 :state-updater (partial state-updater (take consumed tokens))})))
                          patterns)]
        (cond
          (> (count matches) 1) (throw (ex-info "Multiple valid parses" {:parses matches}))
          (= (count matches) 1) (recur ((:state-updater (first matches)) state)
                                       (drop (:consumed (first matches)) tokens))
          (empty? matches) (throw (ex-info "Unable to parse at" {:tokens (take 10 tokens)})))))))

(defn parse-file [f]
  (-> f
      slurp
      lex/lex
      parse))