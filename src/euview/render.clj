(ns euview.render
  (:import [java.awt Image Color Font]
           java.awt.image.BufferedImage
           com.github.gif.AnimatedGifEncoder)
  
  (:require [clojure.java.io :as io]))

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

(defn province-owner-at [year]
  "FRA")

(def ocean-color Color/BLUE)
(defn draw-oceans [frame]
  (doto (.createGraphics frame)
    (.setColor ocean-color)
    (.fillRect 0 0 (.getWidth frame) (.getHeight frame))))

(def transparent-color (Color. 69 69 69 69))
(defn draw-transparent [frame]
  (doto (.createGraphics frame)
    (.setColor transparent-color)
    (.fillRect 0 0 (.getWidth frame) (.getHeight frame))))

(defn write-year [frame year]
  (doto (.createGraphics frame)
    (.setColor ocean-color)
    (.fillRect 0 00 100 100)
    (.setColor Color/BLACK)
    (.setFont (Font. "SansSerif" Font/BOLD 12))
    (.drawString (str year) 50 50)))

(defn year->frame [savegame
                   ^BufferedImage map
                   provinces
                   year]
  (let [frame (BufferedImage. (.getWidth map) (.getHeight map) (.getType map))]
    (draw-transparent frame)
    (println "constructing frame" year)
    (write-year frame year)
    frame))

(defn initial-frame [map savegame]
  (let [frame (BufferedImage. (.getWidth map) (.getHeight map) (.getType map))]
    (draw-oceans frame)
    (write-year frame "1444")
    frame))

(defn frames->gif [map frames gif-filename]
  (let [encoder (AnimatedGifEncoder.)]
    (doto encoder 
      (.start gif-filename)
      (.setDelay 10)
      (.setRepeat 0)
      (.setDispose 1)
      (.setTransparent transparent-color))
    (doseq [frame frames]
      (.addFrame encoder frame))
    (.finish encoder)))

(defn construct-provinces [savegame map-bmp pid->color]
  (for [[k v] (-> savegame :variables (get "provinces"))]
    {:pid k
     :color (pid->color k)
     :overlay nil
     :history (get v "history")}))

(defn render-gif [parsed-savegame gif-filename]
  (let [map (load-map)
        provinces (construct-provinces parsed-savegame map (load-provinces-csv))
        frames (cons (initial-frame map parsed-savegame)
                     (for [year (range 1444 1460)] (year->frame parsed-savegame map provinces year)))]
    (frames->gif map frames gif-filename)))