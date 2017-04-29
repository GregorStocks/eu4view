(ns euview.render
  (:import java.awt.Image
           java.awt.image.BufferedImage)
  (:require [clojure.java.io :as io]
            [gif-clj.core :as gif]))

(defn load-map []
  (let [scale-factor 4
        base-map (javax.imageio.ImageIO/read (io/resource "provinces.bmp"))
        w (int (/ (.getWidth base-map) scale-factor))
        h (int (/ (.getHeight base-map) scale-factor))
        tmp (.getScaledInstance base-map w h Image/SCALE_SMOOTH)
        dimg (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics dimg)]
    (.drawImage g tmp 0 0 nil)
    (.dispose g)
    dimg))

(defn load-provinces-csv []
  (do
    (slurp (io/resource "definition.csv"))
    {}))

(defn province-owner-at [year])

(defn year->frame [savegame map provinces year]
  (println "constructing frame" year)
  map)

(defn frames->gif [map frames gif-filename]
  (with-open [w (io/output-stream gif-filename)]
    (.write w (gif/build-animated-gif 10 0 (cons map frames)))))

(defn construct-provinces [savegame map-bmp pid->color]
  (for [[k v] (-> savegame :variables (get "provinces"))]
    {:pid k
     :color (pid->color k)
     :overlay nil
     :history (get v "history")}))

(defn render-gif [parsed-savegame gif-filename]
  (let [map (load-map)
        provinces (construct-provinces parsed-savegame map (load-provinces-csv))]
    (frames->gif map
                 (for [year (range 1444 1460)] (year->frame parsed-savegame map provinces year))
                 gif-filename)))