(ns euview.lex)

(def matchers
  {:whitespace [#"^\s+" :ignore]
   ;;   :eu4txt [#"^EU4txt" (fn [match] {:type :eu4txt})]
   :comment [#"^#.+" :ignore]
   :empty-tag [#"^---" (fn [match] {:type :tag
                                    :value "---"})]
   :equals [#"^=" (fn [match] {:type :equals})]
   :open [#"^[{]" (fn [match] {:type :open})]
   :close [#"^[}]" (fn [match] {:type :close})]
   :number-or-date [#"^[-]?[0-9.]+" (fn [match]
                                      (if-let [[_ y m d] (re-matches #"-?(\d+)[.](\d+)[.](\d+)" match)]
                                        {:type :ymd
                                         :value (into [] (map #(Long/parseLong %) [y m d]))}
                                        (if (re-find #"[.]" match)
                                          (try
                                            {:type :number
                                             :value (Float/parseFloat match)}
                                            (catch Exception e
                                              (throw (ex-info "Failed to parse float" {:float match}))))
                                          {:type :number
                                           :value (Long/parseLong match)})))]
   :unquoted-string [#"^[a-zA-Z][a-zA-Z0-9_]*" (fn [match]
                                                 (cond
                                                   (= match "EU4txt") {:type :eutxt}
                                                   (re-matches #"[A-Z][A-Z0-9][A-Z0-9]" match) {:type :tag
                                                                                                :value match}
                                                   :else {:type :variable
                                                          :value match}))]
   :string [#"^\"[^\"]*\"" (fn [match] {:type :string
                                        :value (subs match 1 (dec (count match)))})]})

(defn pull-token [s offset]
  (if (>= offset (count s))
    [{:type :eof} nil]
    (let [successes (keep (fn [[k [r f]]]
                            (let [matcher (re-matcher r s)]
                              (.region matcher offset (count s))
                              (when-let [match (re-find matcher)]
                                {:match match
                                 :match-type k
                                 :f f})))
                          matchers)]
      (cond
        (= 1 (count successes)) (let [{:keys [match match-type f]} (first successes)]
                                  (if (= f :ignore)
                                    (pull-token s (+ offset (count match)))
                                    [(f match) (+ offset (count match))]))
        (empty? successes) (throw (ex-info "Couldn't lex" {:vicinity (subs s offset (min (count s) (+ offset 50)))
                                                           :offset offset
                                                           :size (count s)}))
        :else (throw (ex-info "Multiple matchers matched:" {:matchers successes}))))))

(defn lex [s]
  (loop [acc []
         offset 0]
    (let [[token new-offset] (pull-token s offset)]
      (if (= (:type token) :eof)
        acc
        (recur (conj acc token) new-offset)))))