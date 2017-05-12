(ns euview.savegame
  (:import [java.awt Color Font AlphaComposite]
           java.awt.image.BufferedImage)
  (:use [clojure.string :as string]))

(defn clean-string [s]
  (string/replace s #"\"" ""))

(def fresh-int (atom 0))
(def subline-parsers
  {:empty (fn [parse-state line]
            (when (re-matches #" *" line )
              parse-state))
   :newmap (fn [parse-state line]
             (when-let [[_ n] (re-matches #" *([-.\w]+)=[{]" line)]
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
             (when-let [[_ k v] (re-matches #" *([-.\w]+)=([^{]+)" line)]
               (update-in parse-state (cons :variables (:stack parse-state))
                          assoc k (clean-string v))))
   :key-weirdval (fn [parse-state line]
                   (when-let [[_ k v] (re-matches #" *([-.\w]+)=[{]([^{}]*)}" line)]
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

(defn parse-lines [lines]
  (reduce parse-line initial-parse-state lines))

(defn date-string->ymd [date]
  (try
    (let [[_ y m d] (re-matches #"(\d+)[.](\d+)[.](\d+)" date)]
      [(Long/parseLong y)
       (Long/parseLong m)
       (Long/parseLong d)])
    (catch Exception e
      (throw (ex-info "Failed to parse date" {:date date})))))

(defn construct-provinces [savegame]
  (for [[k v] (-> savegame :variables (get "provinces"))]
    (let [history (-> v (get "history"))
          owners (into {}
                       (for [[k v] history]
                         (if-let [tag (get v "owner")]
                           [(date-string->ymd k) tag])))]
      {:pid (Long/parseLong k)
       :initial-owner (get history "owner")
       :owners owners
       :history (get v "history")})))

(defn country-colors [savegame]
  (into {}
        (for [[k v] (-> savegame :variables (get "countries"))]
          (let [[r g b] (map #(Long/parseLong %)
                             (get (get v "colors") "map_color"))]
            [k (Color. r g b)]))))

(defn parse-savegame [stream]
  (let [lines (string/split-lines stream)
        parsed-lines (parse-lines lines)]
    {:start-ymd (-> parsed-lines :variables (get "start_date") date-string->ymd)
     :end-ymd (-> parsed-lines :variables (get "date") date-string->ymd)
     :provinces (construct-provinces parsed-lines)
     :country-colors (country-colors parsed-lines)}))