(ns euview.savegame
  (:import [java.awt Color Font]
           java.awt.image.BufferedImage)
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [euview.parse :as parse]))

(defn construct-provinces [savegame]
  (let [map-file (parse/parse-file (io/resource "Europa Universalis IV/map/default.map"))
        ocean-provinces (set (map - (get (:variables map-file) "sea_starts")))]
    (println ocean-provinces)
    (for [[k v] (-> savegame :variables (get "provinces"))]
      (let [history (-> v (get "history"))
            owners (into {}
                         (for [[k v] history]
                           (if-let [tag (get v "owner")]
                             [k tag])))
            default-owner (cond
                            (ocean-provinces k) :ocean
                            (get v "capital") :empty
                            :else :wasteland)]
        {:pid k
         :initial-owner (get history "owner" default-owner)
         :owners owners}))))

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