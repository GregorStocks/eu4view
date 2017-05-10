(ns euview.render
  (:import [java.awt Color Font AlphaComposite Image]
           java.awt.image.BufferedImage
           com.github.gif.AnimatedGifEncoder)
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
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

(def ocean-color (Color. 60 120 250))
(defn draw-oceans [frame]
  (doto (.createGraphics frame)
    (.setColor ocean-color)
    (.fillRect 0 0 (.getWidth frame) (.getHeight frame))
    .dispose))

(def transparent-color (Color. 69 70 69 69))
(defn draw-transparent [frame]
  (doto (.createGraphics frame)
    (.setColor transparent-color)
    (.fillRect 0 0 (.getWidth frame) (.getHeight frame))
    .dispose))

(defn write-year [frame ymd]
  (doto (.createGraphics frame)
    (.setColor Color/BLACK)
    (.setFont (Font. "SansSerif" Font/BOLD 14))
    (.drawString (str ymd) 50 50)
    .dispose))

(defn ymd-< [x y]
  (neg? (compare x y)))
(defn ymd-<= [x y]
  (not (pos? (compare x y))))

(defn latest-new-owner [province start-ymd end-ymd]
  (->> province
       :owners
       (filter #(and
                 (ymd-< start-ymd (first %))
                 (ymd-<= (first %) end-ymd)))
       (sort-by first)
       last
       second))

(def ac (AlphaComposite/getInstance AlphaComposite/DST_IN))
(defn render-owner [province country-colors frame owner]
  (let [owner-color (country-colors owner)
        o (:overlay province) ;; recolor it, then draw onto frame
        g (.createGraphics o)
        fg (.createGraphics frame)]
    ;;(.setComposite g ac)
    ;;    (.setColor g owner-color)
    ;;(.fillRect g 0 0 (.getWidth o) (.getHeight o))
    (.drawImage fg o (:overlay-x province) (:overlay-y province) nil)
    (.dispose fg)
    (.dispose g)))

(defn add-initial-frame [{:keys [^BufferedImage map
                                 encoder
                                 provinces
                                 country-colors]}
                         ymd]
  (let [frame (BufferedImage. (.getWidth map) (.getHeight map) (.getType map))]
    (draw-oceans frame)
    (write-year frame ymd)
    (doseq [province provinces]
      (when-let [o (:initial-owner province)]
        (render-owner province country-colors frame o)))
    ;; ????????????? REMOVING THIS BREAKS EVERYTHING    
    (doto (.createGraphics frame)
      (.setColor (Color. 2 3 4))
      (.fillRect 0 0 1 1)
      .dispose)
    (.addFrame encoder frame)))

(defn add-delta-frame [{:keys [^BufferedImage map
                               encoder
                               provinces
                               country-colors]}
                       start-ymd
                       end-ymd]
  (let [frame (BufferedImage. (.getWidth map) (.getHeight map) (.getType map))]
    (draw-transparent frame)
    (doto (.createGraphics frame)
      (.setColor ocean-color)
      (.fillRect 0 0 200 100)
      .dispose)
    (write-year frame end-ymd)
    (doseq [province provinces]
      (when-let [owner (latest-new-owner province start-ymd end-ymd)]
        (render-owner province country-colors frame owner)))
    (.addFrame encoder frame)))

(defn base-gif [gif-filename]
  (doto (AnimatedGifEncoder.)
    (.start gif-filename)
    (.setDelay 10)
    (.setRepeat 0)
    (.setDispose 1)
    (.setTransparent transparent-color)))

(defn add-frames [{:keys [encoder map provinces colors] :as params} start-ymd end-ymd]
  (add-initial-frame params start-ymd)
  (if (= (first start-ymd) (first end-ymd))
    (add-delta-frame params start-ymd end-ymd)
    (do
      (add-delta-frame params start-ymd [(inc (first start-ymd)) 1 1])
      (doseq [year (range (inc (first start-ymd)) (first end-ymd))]     
        (add-delta-frame params [year 1 1] [(inc year) 1 1]))
      (add-delta-frame params [(first end-ymd) 1 1] end-ymd))))

(defn add-overlays [provinces map]
  (let [loaded (slurp (io/resource "definition.csv"))
        lines (drop 1 (string/split-lines loaded))
        colors (into {} (for [line lines]
                          (let [[_ pid r g b] (re-matches #"(\d+);(\d+);(\d+);(\d+);.*" line)]
                            [pid (Color. (Long/parseLong r)
                                         (Long/parseLong g)
                                         (Long/parseLong b))])))]
    (for [p provinces]
      (let [frame (BufferedImage. 50 50 (.getType map))
            x (rand-int 20)
            y (rand-int 5)]
        (doto (.createGraphics frame)
          (.setColor (Color. 255 255 255 0))
          (.fillRect 0 0 (.getWidth frame) (.getHeight frame))
          .dispose)
        (doto (.createGraphics frame)
          (.setColor (Color. (rand-int 255) (rand-int 255) (rand-int 255) 255))
          (.fillOval 0 0 (rand-int 100) (rand-int 100))
          .dispose)
        (assoc p
               :overlay frame
               :overlay-x (* 50 x)
               :overlay-y (* 50 y))))))

(defn render-gif [{:keys [provinces start-ymd end-ymd country-colors] :as savegame} gif-filename]
  (let [map (load-map)
        provinces (add-overlays provinces map)
        encoder (base-gif gif-filename)]
    (add-frames {:encoder encoder
                 :map map
                 :provinces provinces
                 :country-colors country-colors}
                start-ymd
                end-ymd)
    (.finish encoder)))