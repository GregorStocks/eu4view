(ns euview.parse
  (:require [clojure.string :as string]
            [euview.lex :as lex]))

(def scalar #{:variable :tag :number :ymd :string})
(def patterns
  {:eutxt [[#{:eutxt}]
           []
           (fn [[] state] state)]
   :set-var [[scalar #{:equals} scalar]
             []
             (fn [[k _ v]
                  {:keys [stack variables] :as state}]
               (update-in state (cons :variables stack) assoc (:value k) (:value v)))]
   :array-entry [[scalar]
                 [(conj scalar :close)]
                 (fn [[v]
                      {:keys [stack variables] :as state}]
                   (when (map? (get-in state (cons :variables stack)))
                     (throw (ex-info "Trying to mix array & map around" stack)))
                   (update-in state (cons :variables stack) conj (:value v)))]
   :set-map [[scalar #{:equals} #{:open}]
             []
             (fn [[k _ _]
                  state]
               (-> state
                   (update :stack conj (:value k))
                   ;; sometimes the same value is reused for a scalar and a bit of hierarchy...
                   (update-in (cons :variables (:stack state)) dissoc (:value k))))]
   :open-by-itself-dammit-paradox [[#{:open}]
                                   []
                                   (fn [[_]
                                        {:keys [stack variables] :as state}]
                                     (update state :stack conj :empty))]
   :close [[#{:close}]
           []
           (fn [[_] state]
             (update state :stack pop))]})

(defn parse [original-tokens]
  (loop [state {:variables {}
                :stack []}
         tokens original-tokens]
    (if-not (seq tokens)
      (do
        (when-not (= (:stack state) [])
          (throw (ex-info "Exited without popping full stack???" {:stack (:stack state)})))
        state)
      (let [matches (keep (fn [[k [spec lookahead state-updater]]]
                            (let [consumed (count spec)
                                  full-spec (concat spec lookahead)
                                  agreements (map (fn [token valid-types] ((:type token) valid-types))
                                                  tokens
                                                  full-spec)]
                              (when (and (= (count agreements) (count full-spec))
                                         (every? identity agreements))
                                {:consumed consumed
                                 :state-updater (partial state-updater (take consumed tokens))})))
                          patterns)]
        (cond
          (> (count matches) 1) (throw (ex-info "Multiple valid parses" {:parses matches}))
          (= (count matches) 1) (recur ((:state-updater (first matches)) state)
                                       (drop (:consumed (first matches)) tokens))
          (empty? matches) (throw (ex-info "Unable to parse at" {:stack (:stack state)
                                                                 :tokens (take 10 tokens)})))))))

(defn parse-file [f]
  (-> f
      slurp
      lex/lex
      parse))