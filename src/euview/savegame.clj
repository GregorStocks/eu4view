(ns euview.savegame
  (:use [clojure.string :as string]
        [slingshot.slingshot :only (try+ throw+)]))

(defn my-assoc [m k v]
  (when (get m k)
    (println "double inserting" m k))
  (assoc m k v))

(def fresh-int (atom 0))
(def subline-parsers
  {:empty (fn [parse-state line]
            (when (re-matches #" *" line )
              parse-state))
   :eu4txt (fn [parse-state line]
             (when (= "EU4txt" line)
               parse-state))
   :newmap (fn [parse-state line]
             (when-let [[_ n] (re-matches #" *([-.\w]+)=[{]" line)]
               (update parse-state :stack conj n)))
   :empty-open (fn [parse-state line]
                 (when (re-matches #" *[{] *" line)
                   (update parse-state :stack conj "EMPTY")))
   :close (fn [parse-state line]
            (when (re-matches #" *} *" line)
              (if (seq (:stack parse-state))
                (if (= (:stack parse-state) ["provinces"])
                  (throw+ {:type :all-done
                           :parse-state parse-state})
                  (update parse-state :stack pop))
                (throw (ex-info "Got an unexpected }" {:line line})))))
   :keyval (fn [parse-state line]
             (when-let [[_ k v] (re-matches #" *([-.\w]+)=([^{]+)" line)]
               (update-in parse-state (cons :variables (:stack parse-state))
                          assoc k v)))
   :key-weirdval (fn [parse-state line]
                   (when-let [[_ k v] (re-matches #" *([-.\w]+)=[{]([^{}]*)}" line)]
                     (update-in parse-state (cons :variables (:stack parse-state))
                                assoc k v)))
   :dashes (fn [parse-state line]
             (when (re-matches #" *-[- ]*" line)
               ;; feck off
               parse-state))
   :numbers (fn [parse-state line]
              ;; whatever
              (when (re-matches #" *-?[\d.]+( +-?[\d.]+)* *" line)
                parse-state))
   :string (fn [parse-state line]
             (when-let [[_ s] (re-matches #" *\"(.+)\"" line)]
               (update-in parse-state (cons :variables (:stack parse-state))
                          assoc
                          (count (get-in (:variables parse-state)
                                         (:stack parse-state)))
                          s)))
   :letters (fn [parse-state line]
              (when-let [[_ s] (re-matches #" *(([a-zA-Z]+) *)+" line)]
                (update-in parse-state (cons :variables (:stack parse-state))
                           assoc
                           (count (get-in (:variables parse-state)
                                          (:stack parse-state)))
                           s)))})

(def initial-parse-state
  {:stack []
   :line-number 1})
(defn parse-subline [parse-state subline]
  (let [parsed (into {} (map (fn [[k f]]
                               (when-let [v (f parse-state subline)]
                                 [k v]))
                             subline-parsers))]
    (cond
      (> (count parsed) 1) (throw (ex-info "Multiple valid parses for subline"
                                           {:line subline
                                            :parse-state (dissoc parse-state :variables)
                                            :parsers (keys parsed)}))
      (seq parsed) (second (first parsed))
      :else (throw (ex-info "Couldn't parse subline" {:subline subline
                                                      :parse-state (dissoc parse-state :variables)})))))

(defn parse-line [parse-state line]
  (let [sublines (string/split line #"\t+")]
    (reduce parse-subline
            (-> parse-state
                (assoc :current-line line)
                (update :line-number inc))
            sublines)))

(defn parse-lines [lines]
  (try+
    (reduce parse-line initial-parse-state lines)
    (catch [:type :all-done] {:keys [parse-state]}
      parse-state)))

(defn parse-savegame [stream]
  (let [lines (string/split-lines stream)
        parsed-lines (parse-lines lines)]
    parsed-lines))