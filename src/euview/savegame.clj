(ns euview.savegame
  (:import [java.awt Color Font]
           java.awt.image.BufferedImage)
  (:require [clojure.string :as string]
             [euview.parse :as parse]))

(defn construct-provinces [savegame]
  (for [[k v] (-> savegame :variables (get "provinces"))]
    (let [history (-> v (get "history"))
          owners (into {}
                       (for [[k v] history]
                         (if-let [tag (get v "owner")]
                           [k tag])))
          default-owner (cond
                          (get v "capital") :empty
                          (get v "patrol") :ocean
                          :else :wasteland)]
      {:pid k
       :initial-owner (get history "owner" default-owner)
       :owners owners
       :history (get v "history")})))

(defn country-colors [savegame]
  (into {:empty (Color. 50 50 20)
         :ocean (Color. 60 120 250)
         :wasteland (Color. 100 50 25)}
        (for [[k v] (-> savegame :variables (get "countries"))]
          (do
            (let [[r g b] (map #(int %)
                               (get (get v "colors") "map_color"))]
              [k (Color. r g b)])))))

(defn parse-savegame [file]
  (let [parsed (parse/parse-file file)]
    {:start-ymd (-> parsed :variables (get "start_date"))
     :end-ymd (-> parsed :variables (get "date"))
     :provinces (construct-provinces parsed)
     :country-colors (country-colors parsed)}))