(ns euview.savegame
  (:import [java.awt Color Font]
           java.awt.image.BufferedImage)
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [euview.util :as util]
            [euview.parse :as parse]))

(defn tag->changes [parsed-file]
  (let [changes (mapcat (fn [[tag country]]
                          (let [history (get country "history")]
                            (for [[ymd changes] history]
                              (when-let [prev-tag (get changes "changed_tag_from")]
                                {:old-tag prev-tag
                                 :new-tag tag
                                 :ymd ymd}))))
                        (get (:variables parsed-file) "countries"))]
    (filter identity changes)))

(defn add-tag-changes [tag-changes owners initial-owner]
  ;; For every province, add a synthetic tag-change if it was owned by a tag changer when its tag changed
  (apply merge owners
                      (for [{:keys [old-tag new-tag ymd]} tag-changes]
                        (let [preceding-change (last
                                                (sort-by first
                                                         (filter (fn [[change-ymd tag]]
                                                                   (util/ymd-< change-ymd ymd))
                                                                 owners)))]
                          (when (or (and (not preceding-change) (= initial-owner old-tag))
                                    (= (second preceding-change) old-tag))
                            {ymd new-tag})))))

(defn construct-provinces [savegame]
  ;; todo load from eu4 directory not resources...... doy
  (let [map-file (parse/parse-file (slurp (io/resource "Europa Universalis IV/map/default.map")))
        ocean-provinces (set (map - (get (:variables map-file) "sea_starts")))
        tag-changes (tag->changes savegame)]
    (for [[pid v] (-> savegame :variables (get "provinces"))]
      (let [history (-> v (get "history"))
            owners (into {}
                         (for [[k v] history]
                           (if-let [tag (get v "owner")]
                             [k tag])))
            default-owner (cond
                            (ocean-provinces pid) :ocean
                            (get v "capital") :empty
                            :else :wasteland)
            initial-owner (get history "owner" default-owner)]
        {:pid pid
         :initial-owner initial-owner
         :owners (add-tag-changes tag-changes owners initial-owner)}))))

(def empty-color (Color. 50 50 20))
(defn country-colors [savegame]
  (assoc
   (into {:empty empty-color
          :ocean (Color. 60 120 250)
          :wasteland (Color. 100 50 25)}
         (for [[k v] (-> savegame :variables (get "countries"))]
           (do
             (let [[r g b] (map #(int %)
                                (get (get v "colors") "map_color"))]
               [k (Color. r g b)]))))
   "XXX" empty-color))

(defn process-savegame [parsed]
  {:start-ymd (-> parsed :variables (get "start_date"))
   :end-ymd (-> parsed :variables (get "date"))
   :provinces (construct-provinces parsed)
   :country-colors (country-colors parsed)})