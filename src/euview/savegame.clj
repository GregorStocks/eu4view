(ns euview.savegame
  (:import [java.awt Color Font]
           java.awt.image.BufferedImage)
  (:require [clojure.string :as string]
             [euview.parse :as parse]))

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
                           [(date-string->ymd k) tag])))
          default-owner (cond
                          (get v "capital") :empty
                          (get v "patrol") :ocean
                          :else :wasteland)]
      {:pid (Long/parseLong k)
       :initial-owner (get history "owner" default-owner)
       :owners owners
       :history (get v "history")})))

(defn country-colors [savegame]
  (into {:empty (Color. 50 50 20)
         :ocean (Color. 60 120 250)
         :wasteland (Color. 70 70 70)}
        (for [[k v] (-> savegame :variables (get "countries"))]
          (let [[r g b] (map #(Long/parseLong %)
                             (get (get v "colors") "map_color"))]
            [k (Color. r g b)]))))

(defn parse-savegame [file]
  (let [parsed (parse/parse-file file)]
    {:start-ymd (-> parsed :variables (get "start_date") date-string->ymd)
     :end-ymd (-> parsed :variables (get "date") date-string->ymd)
     :provinces (construct-provinces parsed)
     :country-colors (country-colors parsed)}))