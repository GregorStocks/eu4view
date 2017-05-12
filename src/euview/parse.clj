(ns euview.parse
  (:require [clojure.string :as string]))

(defn clean-string [s]
  (string/replace s #"\"" ""))

(def fresh-int (atom 0))
(def subline-parsers
  {:empty (fn [parse-state line]
            (when (re-matches #" *" line )
              parse-state))
   :comment (fn [parse-state line]
              (when (re-matches #" *#.*" line )
                parse-state))
   :newmap (fn [parse-state line]
             (when-let [[_ n] (re-matches #" *([-.\w]+) *= *[{]" line)]
               (-> parse-state
                   (update :stack conj n)
                   (update-in (cons :variables (:stack parse-state)) dissoc n))))
   :empty-open (fn [parse-state line]
                 (when (re-matches #" *[{] *" line)
                   (update parse-state :stack conj "EMPTY")))
   :close (fn [parse-state line]
            (when (re-matches #" *} *" line)
              (if (seq (:stack parse-state))
                (update parse-state :stack pop)
                (throw (ex-info "Got an unexpected }" {:line line})))))
   :keyval (fn [parse-state line]
             (when-let [[_ k v] (re-matches #" *([-.\w]+) *= *([^{]+)" line)]
               (update-in parse-state (cons :variables (:stack parse-state))
                          assoc k (clean-string v))))
   :key-weirdval (fn [parse-state line]
                   (when-let [[_ k v] (re-matches #" *([-.\w]+) *= *[{]([^{}]*)}" line)]
                     (update-in parse-state (cons :variables (:stack parse-state))
                                assoc k (clean-string v))))
   :scalars (fn [parse-state line]
              (when (re-matches #" *[-.A-Za-z0-9]+( +[-.A-Za-z0-9]+)* *" line)
                (if (= (:stack parse-state) [])
                  ;; eu4txt - metadata, not a scalar
                  parse-state
                  (update-in parse-state (cons :variables (:stack parse-state))
                             concat (re-seq #"[^ ]+" line)))))
   :string (fn [parse-state line]
             (when-let [[_ s] (re-matches #" *\"(.+)\"" line)]
               (update-in parse-state (cons :variables (:stack parse-state))
                          assoc
                          (count (get-in (:variables parse-state)
                                         (:stack parse-state)))
                          s)))
   :letters (fn [parse-state line]
              (when false
                (when-let [[_ s] (re-matches #" *([a-zA-Z][a-zA-Z ]+)" line)]
                  (update-in parse-state (cons :variables (:stack parse-state))
                             assoc
                             (count (get-in (:variables parse-state)
                                            (:stack parse-state)))
                             s))))})

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
                                                      :line-number (:line-number parse-state)})))))

(defn parse-line [parse-state line]
  (when (= 0 (mod (:line-number parse-state) 100000))
    (println "processing line" (:line-number parse-state) "current stack" (:stack parse-state)))
  (let [sublines (string/split line #"(\t+)|(?<=[{])|(?=[}])")]
    (reduce parse-subline
            (-> parse-state
                (assoc :current-line line)
                (update :line-number inc))
            sublines)))

(defn parse-file [f]
  (reduce parse-line initial-parse-state (string/split-lines (slurp f))))
