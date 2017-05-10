(ns euview.render
  (:import [java.awt Image Color Font]
           java.awt.image.BufferedImage
           com.github.gif.AnimatedGifEncoder)
  (:require [clojure.java.io :as io]
            [mikera.image.core :as image]))

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

(def ocean-color (Color. 60 120 250))
(defn draw-oceans [frame]
  (doto (.createGraphics frame)
    (.setColor ocean-color)
    (.fillRect 0 0 (.getWidth frame) (.getHeight frame))))

(def transparent-color (Color. 69 70 69 69))
(defn draw-transparent [frame]
  (doto (.createGraphics frame)
    (.setColor transparent-color)
    (.fillRect 0 0 (.getWidth frame) (.getHeight frame))))

(defn write-year [frame ymd]
  (doto (.createGraphics frame)
    (.setColor Color/BLACK)
    (.setFont (Font. "SansSerif" Font/BOLD 14))
    (.drawString (str ymd) 50 50)))

(defn latest-new-owner [province start-ymd end-ymd]
  (let [owners (:owners province)]
    ))

(defn delta-frame [^BufferedImage map
                   provinces
                   start-ymd
                   end-ymd]
  (let [frame (BufferedImage. (.getWidth map) (.getHeight map) (.getType map))]
    (draw-transparent frame)
    (doto (.createGraphics frame)
      (.setColor ocean-color)
      (.fillRect 0 0 200 100))
    (write-year frame end-ymd)
    (doseq [province provinces]
      (when-let [owner (latest-new-owner province start-ymd end-ymd)]))
    frame))

(defn initial-frame [map provinces ymd]
  (let [frame (BufferedImage. (.getWidth map) (.getHeight map) (.getType map))]
    (draw-oceans frame)
    (write-year frame ymd)
    ;; ????????????? REMOVING THIS BREAKS EVERYTHING
    (doto (.createGraphics frame)
      (.setColor (Color. 2 3 4))
      (.fillRect 0 0 1 1))
    frame))

(defn frames->gif [frames gif-filename]
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

(defn make-frames [map provinces start-ymd end-ymd]
  (if (= (first start-ymd) (first end-ymd))
    [(initial-frame map provinces start-ymd)
     (delta-frame map provinces start-ymd end-ymd)]
    (into []
          (concat
           [(initial-frame map provinces start-ymd)
            (delta-frame map
                         provinces
                         start-ymd
                         [(inc (first start-ymd)) 1 1])]
           (for [year (range (inc (first start-ymd))
                             (first end-ymd))]
             (delta-frame map provinces [year 1 1] [(inc year) 1 1]))
           [(delta-frame map provinces [(first end-ymd) 1 1] end-ymd)]))))

(defn apply-csv [provinces csv]
  provinces)

(defn render-gif [{:keys [provinces start-ymd end-ymd]} gif-filename]
  (let [map (load-map)
        provinces (apply-csv provinces (load-provinces-csv))
        frames (make-frames map provinces start-ymd end-ymd)]
    (frames->gif frames gif-filename)))